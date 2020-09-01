
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
if(!require(foreign)) install.packages("foreign", repos = "https://svn.r-project.org/")

options(shiny.usecairo=TRUE)
# import data

geopko <- read_csv("geopko2.csv", col_types = cols(.default="c"))
iso <- read.csv("geopko_ccode2.csv")


# data prep TCC

geopko <- geopko %>% 
  mutate(No.troops=as.numeric(No.troops),
         No.TCC=as.numeric(No.TCC),
         Longitude=as.numeric(Longitude),
         Latitude=as.numeric(Latitude),
         UNMO.dummy=as.numeric(UNMO.dummy),
         UNPOL.dummy=as.numeric(UNPOL.dummy),
         HQ=as.factor(HQ)) 

tcc_df <- geopko %>% select(Source, Mission, Year, Month, No.troops, 52:85) %>% 
  group_by(Source, Mission, Year, Month) %>% 
  mutate(Total.troops=sum(No.troops, na.rm=T)) %>% ungroup()


#colnames(tcc_df) <- sub("name.of.TCC", "nameofTCC_", colnames(tcc_df)) obsolete script
#colnames(tcc_df) <- sub("No.troops.per.TCC", "notroopsperTCC_", colnames(tcc_df))
#tcc_df <- tcc_df %>% mutate_at(vars(starts_with("notroopsperTCC")), as.character) %>% 
#  mutate_at(vars(starts_with("nameofTCC")), as.character) %>% 

#data prep: creating list of country codes for GADM sf files dowload

cclist3 <- iso %>% select(Mission, a3) %>% distinct() 
map_df <- geopko %>% unite(timepoint, c("Year","Month"), sep="-") %>% 
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
                          checkboxInput(inputId="MO_map", "UNMO", value=FALSE),
                          checkboxInput(inputId="UNPOL_map", "UNPOL", value=FALSE),
                          helpText("Errors may occur when a selected feature is not available for a map. Please deselect the option.")
                        ),
                        mainPanel(fluid=TRUE,
                                  plotOutput("depmap"),
                                  span(h6(textOutput("basecountries"), align="center"))
                                  ))
                      ),
                      tabPanel("Troop-contributing Countries",
                               basicPage(
                                 radioButtons(inputId="databy_tcc", label="Present data by:", choices=c("Deployment map", "Year"),
                                              selected="Deployment map (default)"),
                                 helpText("The GeoPKO dataset collects data by deployment maps published by the UN. For the best accuracy, display data by deployment maps. Data by year present the year's average troop counts and the highest number of TCCs."),
                                 # span(h6(textOutput("tabletext", align="right"))),
                                 DT::dataTableOutput("tcc_table")
                               )))
)

server <- function(input, output, session){
  #TCC tables
  bymap_df <- reactive({
    tcc_df %>% 
      pivot_longer(c(6:39), names_to=c(".value", "tcc_id"), names_sep="_")%>%
      mutate(notroopspertcc=as.numeric(notroopspertcc)) %>%
      filter(!is.na(nameoftcc)) %>%
      select(-tcc_id) %>% 
      group_by(Source, Mission, Year, Month, Total.troops, nameoftcc)%>%
      summarise(total.tcc=sum(notroopspertcc)) %>% 
      add_count(Source, name="No.TCC") %>%
      mutate(overview=paste0(nameoftcc, "-", total.tcc)) %>%
      select(-nameoftcc, -total.tcc) %>%
      group_by(Source, Mission, Year, Month, Total.troops, No.TCC) %>%
      summarise(details=str_c(overview, collapse=", ")) %>% 
      arrange(desc(Year))}) 
  
  byyear_df <- reactive({
    tcc_df %>% 
      pivot_longer(c(6:39), names_to=c(".value", "TCC_id"), names_sep="_")%>%
      mutate(notroopspertcc=as.numeric(notroopspertcc)) %>%
      filter(!is.na(nameoftcc)) %>%
      select(-TCC_id) %>% 
      group_by(Source, Mission, Year, Month, Total.troops, nameoftcc)%>%
      summarise(total.each.tcc=sum(notroopspertcc)) %>% 
      add_count(Source, name="No.TCC") %>%
      mutate(overview=paste0(nameoftcc, "-", total.each.tcc)) %>%
      select(-nameoftcc, -total.each.tcc) %>%
      group_by(Source, Mission, Year, Month, Total.troops, No.TCC) %>%
      summarise(details=str_c(overview, collapse=", ")) %>% 
      arrange(desc(Year)) %>%
      group_by(Mission, Year) %>% mutate(min.troops= as.character(min(Total.troops)),
                                         max.troops=as.character(max(Total.troops)),
                                         ave.troops=as.character(round(mean(Total.troops)))) %>%
      group_by(Mission, Year) %>% arrange(desc(No.TCC)) %>% dplyr::slice(1) %>% 
      mutate(ave.troops=ifelse(is.na(ave.troops), "Unknown", ave.troops),
             min.troops=ifelse(is.na(min.troops), "Unknown", min.troops),
             max.troops=ifelse(is.na(max.troops), "Unknown", max.troops)) %>%
      select(Mission, Year, No.TCC, details, min.troops, max.troops, ave.troops)
  })
  
  # output$tabletext <- renderText({
  #   req(input$databy_tcc)
  #   if(input$databy_tcc=="Deployment map"){
  #   paste("")  
  #   }
  #   else if(input$databy_tcc=="Year"){
  #   paste("Data collected")
  #   }
  # })
  #   
  output$tcc_table <- DT::renderDataTable({
    req(input$databy_tcc)
    if(input$databy_tcc=="Deployment map"){
      DT::datatable(bymap_df(),
                    colnames = c("Source map", "Mission", "Year", "Month", 
                                 "Total Troop Count", "Number of TCCs", 
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
  
  output$basecountries <- renderText({
    countrieslist <- map_df_temp() %>% distinct(Country)
    paste("This mission took place in",paste(unique(map_df_temp()$Country), collapse=", "),".")
  })
  
  UNMO_df_temp <- reactive({
    map_df_temp() %>% filter(UNMO.dummy==1)
  })
  
  UNPOL_df_temp <- reactive({
    map_df_temp() %>% filter(UNPOL.dummy==1)
  })
  
  SHQ_df_temp <- reactive({
    map_df_temp() %>% filter(HQ==2)
  })
  
  
  output$depmap <- renderPlot({
    input$depsize_map
    input$MHQ_map
    input$SHQ_map
    input$MO_map
    input$UNPOL_map
    
    maplist <- pull(sfdf(), a3)
    mapshapefiles <- gadm_sf_loadCountries(c(paste(maplist)), level=1)
    

    p <- ggplot() + geom_sf(data=mapshapefiles$sf) + 
      theme_void() + 
      labs(title=paste(map_df_temp()$Mission,": ", map_df_temp()$timepoint),
           caption="Sources: GeoPKO v1.2\n Shapefiles from GADM.")+
      geom_blank()+
      scale_shape_manual(values=c(3, 5, 23), 
                         labels=c("SHQ"="Sector HQ", "UNMO"="Military Observers", "UNPOL"="UN Police"),
                         name="")
    if(input$depsize_map){
      p <- p + geom_point(data=map_df_temp(), aes(x=Longitude, y=Latitude, size=No.troops, color=as.factor(No.TCC)),
                          shape=20, alpha = 0.5)+
        scale_size_continuous(name="Size of deployment",range=c(2, 20))+
        scale_color_brewer(palette="Set1", name="Number of TCCs")+
        guides(colour = guide_legend())} 
    if(input$MHQ_map){
      p <- p +  geom_point(data=map_df_temp() %>% filter(HQ==3), aes(x=Longitude, y=Latitude, shape="HQ"),
                           shape=4, color="red", size=6)+
        geom_label_repel(data=map_df_temp() %>% filter(HQ==3), 
                         aes(x=Longitude, y=Latitude, label=paste0("Mission HQ: ",Location)
                         ),
                         box.padding = 2,
                         size = 3, 
                         fill = alpha(c("white"),0.7))} 
    if(input$SHQ_map){
      if(length(SHQ_df_temp()$Location>1)){
      p <- p +  geom_point(data=map_df_temp() %>% filter(HQ==2), 
                           aes(x=Longitude, y= Latitude, shape="SHQ"), color="orange", size=5)}
      else{
        p <- p + labs(subtitle="Sector HQs not available. Please deselect the option.")}  
      }
    if(input$MO_map){
      if(length(UNMO_df_temp()$Location)>1){
        p <- p +  geom_point(data=map_df_temp() %>% filter(UNMO.dummy==1), 
                             aes(x=Longitude, y= Latitude, shape="UNMO"), color="darkgreen", size=3)}
      else{
        p <- p + labs(subtitle="UNMO not found. Please deselect the option.")}  
    }
    
    if(input$UNPOL_map){
      if(length(UNPOL_df_temp()$Location)>1){
        p <- p +  geom_point(data=map_df_temp() %>% filter(UNPOL.dummy==1), 
                             aes(x=Longitude, y= Latitude, shape="UNPOL"), color="darkblue", size=4)}
      else{
        p <- p + labs(subtitle="UNPOL not found. Please deselect the option.")}
    }
    
    p + theme(plot.subtitle = element_text(color="red"),
              legend.text.align = 0)
    p

    
})
  
  
}

shinyApp(ui=ui, server=server)