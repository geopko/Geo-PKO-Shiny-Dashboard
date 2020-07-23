
#load required packages

if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(GADMTools)) install.packages("GADMTools")
if(!require(Cairo)) install.packages("Cairo")
if(!require(ggrepel)) install.packages("ggrepel")
options(shiny.usecairo=TRUE)
# import data

geopko <- read_csv("geopko.csv", col_types = cols(No.TCC = col_number(), 
                                                  No.troops = col_number(),
                                                  month = col_number()))
iso <- read.csv("geopko_ccode.csv")


# data prep TCC
tcc_df <- geopko %>% select(Source, Mission, year, month, No.troops, 16:44) %>% 
  group_by(Source, Mission, year, month) %>% mutate(Total.troops=sum(No.troops)) %>% ungroup() %>%
  select(1:4, 35, 5:34, -No.troops)

colnames(tcc_df) <- sub("name.of.TCC", "nameofTCC_", colnames(tcc_df))
colnames(tcc_df) <- sub("No.troops.per.TCC", "notroopsperTCC_", colnames(tcc_df))
tcc_df <- tcc_df %>% mutate_at(vars(starts_with("notroopsperTCC")), as.character)
tcc_df <- tcc_df %>% mutate_at(vars(starts_with("nameofTCC")), as.character)

#data prep: creating list of country codes for GADM sf files dowload

cclist3 <- iso %>% select(Mission, alpha_3) %>% distinct() 
map_df <- geopko %>% unite(timepoint, c("year","month"), sep="-") %>% 
  mutate(timepoint=as.factor(timepoint))

#data prep for single-map generator

#data prep for facet maps

# designing the UI

ui <- fluidPage(
  navbarPage("Exploring GeoPKO",
             tabPanel("Map Generator",
                      sidebarLayout(
                        sidebarPanel(
                          p("Where are UN peacekeepers posted, and how many? Select the options below to visualize."),
                          selectInput(inputId="mission_map", label="Select a mission",
                                      choices=factor(geopko$Mission), width=150),
                          selectInput("timepoint_map", "Choose source's timepoint", choices=NULL),
                          checkboxInput(inputId="depsize_map", "Deployment size", value=TRUE),
                          checkboxInput(inputId="MHQ_map", "Mission HQ", value=FALSE),
                          checkboxInput(inputId="SHQ_map", "Sector HQ", value=FALSE),
                          checkboxInput(inputId="MO_map", "UNMO", value=FALSE)
                        ),
                        mainPanel(fluid=TRUE,
                                  plotOutput("depmap"))
                        
                      )
             ),
             tabPanel("Troop-contributing Countries",
                      basicPage(
                        selectInput(inputId="mission_tcc", label="Mission", choices=factor(geopko$Mission),
                                    width=150),
                        radioButtons(inputId="databy_tcc", label="Present data by:", choices=c("Deployment map", "Year"),
                                     selected="Deployment map (default)"),
                        helpText("The GeoPKO dataset collects data using the UN's deployment maps. Data presented by year takes the number of TCCs from the first map of the year, while troop counts are presented by yearly max, min, and average values.")
                      ),
                      
                      DT::dataTableOutput("tcc_table")
             )
  )
  
  
)

server <- function(input, output, session){
  #TCC tables
  bymap_df <- reactive({
    req(input$mission_tcc)
    tcc_df %>% filter(Mission %in% input$mission_tcc) %>%
      pivot_longer(c(7:34), names_to=c(".value", "TCC_id"), names_sep="_")%>%
      mutate(notroopsperTCC=as.numeric(notroopsperTCC)) %>%
      select(-TCC_id, -No.TCC) %>% 
      mutate(notroopsperTCC=as.numeric(notroopsperTCC)) %>%
      group_by(Source, Mission, year, month, Total.troops, nameofTCC)%>%
      summarise(total.tcc=sum(notroopsperTCC)) %>% 
      add_count(Source, name="No.TCC") %>%
      mutate(overview=paste0(nameofTCC, "-", total.tcc)) %>%
      select(-nameofTCC, -total.tcc) %>%
      group_by(Source, Mission, year, month, Total.troops, No.TCC) %>%
      summarise(details=str_c(overview, collapse=", ")) %>% 
      arrange(desc(year))})
  
  byyear_df <- reactive({
    req(input$mission_tcc)
    tcc_df %>% filter(Mission %in% input$mission_tcc) %>%
      pivot_longer(c(7:34), names_to=c(".value", "TCC_id"), names_sep="_")%>%
      mutate(notroopsperTCC=as.numeric(notroopsperTCC)) %>%
      select(-TCC_id, -No.TCC) %>% 
      mutate(notroopsperTCC=as.numeric(notroopsperTCC)) %>%
      group_by(Source, Mission, year, month, Total.troops, nameofTCC)%>%
      summarise(total.tcc=sum(notroopsperTCC)) %>% 
      add_count(Source, name="No.TCC") %>%
      mutate(overview=paste0(nameofTCC, "-", total.tcc)) %>%
      select(-nameofTCC, -total.tcc) %>%
      group_by(Source, Mission, year, month, Total.troops, No.TCC) %>%
      summarise(details=str_c(overview, collapse=", ")) %>% 
      arrange(desc(year)) %>%
      group_by(Mission, year) %>% mutate(min.troops= min(Total.troops),
                                         max.troops=max(Total.troops),
                                         ave.troops=round(mean(Total.troops))) %>%
      group_by(year) %>% dplyr::slice(1) %>% 
      select(Mission, year, No.TCC, details, min.troops, max.troops, ave.troops)
  })
  
  output$tcc_table <- DT::renderDataTable({
    req(input$databy_tcc)
    if(input$databy_tcc=="Deployment map"){
      DT::datatable(bymap_df(),
                    colnames = c("Source map", "Mission", "Year", "Month", "Number of TCCs", 
                                 "Details"),
                    rownames = FALSE)
    }
    else if(input$databy_tcc=="Year"){
      DT::datatable(byyear_df(),
                    colnames = c("Mission", "Year", "Number of TCCs", "Details",
                                 "Min. Troop Count", "Max. Troop Count", "Mean Troop Count"),
                    rownames= FALSE)
    }
  })
  
  #deployment maps
  #update timepoint inputs based on filtering for mission
  
  #creating list of sf objects to download
  sfdf <- reactive({
    req(input$mission_map)
    cclist3 %>% filter(Mission %in% input$mission_map)
  })
  
  observeEvent(input$mission_map,{
    updateSelectInput(session, 'timepoint_map',
                      choices = unique(map_df$timepoint[map_df$Mission==input$mission_map]))
    
  })
  
  map_df_temp <- reactive({
    req(input$mission_map)
    req(input$timepoint_map)
    map_df %>% filter(Mission %in% input$mission_map) %>%
      filter(timepoint %in% input$timepoint_map)
    
  })
  
  output$depmap <- renderPlot({
    input$depsize_map
    input$MHQ_map
    input$SHQ_map
    input$MO_map
    
    maplist <- pull(sfdf(), alpha_3)
    mapshapefiles <- gadm_sf_loadCountries(c(paste(maplist)), level=1)
    p <- ggplot() + geom_sf(data=mapshapefiles$sf) + 
      theme_void() + 
      labs(title=paste(map_df_temp()$Mission,": ", map_df_temp()$timepoint), 
           caption="Data from GeoPKO v1.2\n Shapefiles from GADM.")
    if(input$depsize_map){
      p <- p + geom_point(data=map_df_temp(), aes(x=longitude, y=latitude, size=No.troops, color=No.troops),
                          shape=20, alpha = 0.5)+
        scale_size_continuous(name="Size of deployment",range=c(2, 20))+
        scale_color_viridis_c(name="Size of deployment")+
        guides(colour = guide_legend())} 
    if(input$MHQ_map){
      p <- p +  geom_point(data=map_df_temp() %>% filter(HQ==3), aes(x=longitude, y=latitude),
                           shape=4, color="red", size=6)+
        geom_label_repel(data=map_df_temp() %>% filter(HQ==3), 
                            aes(x=longitude, y=latitude, label=paste("Mission HQ: ",location)
                            ))} 
    if(input$SHQ_map){
      p <- p +  geom_point(data=map_df_temp() %>% filter(HQ==2), 
                           aes(x=longitude, y= latitude), shape=3, color="orange", size=5)} 
    if(input$MO_map){ 
      p <- p +  geom_point(data=map_df_temp() %>% filter(UNMO..dummy.==1), 
                           aes(x=longitude, y= latitude), shape=5, color="darkgreen", size=3)}
    
    p 
    
  }
  )
  
  
}

shinyApp(ui=ui, server=server)