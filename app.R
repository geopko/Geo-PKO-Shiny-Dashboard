
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
#if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
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
if(!require(forcats)) install.packages("forcats")
if(!require(sp)) install.packages("sp")
if(!require(zoo)) install.packages("zoo")
if(!require(gifski)) install.packages("gifski")
if(!require(png)) install.packages("png")
if(!require(gganimate)) install.packages("gganimate")

if(!require(foreign)) install.packages("foreign", repos = "https://svn.r-project.org/")

options(shiny.usecairo=TRUE)
####import data####

geopko <- readr::read_csv("geopko2.csv", col_types = cols(.default="c"),
                          locale=readr::locale(encoding="latin1"))
iso <- read.csv("geopko_ccode2.csv")


####data prep TCC####

geopko <- geopko %>% 
  mutate_at(vars(c(No.troops, No.TCC, Longitude, Latitude,
                   UNMO.dummy, UNPOL.dummy)), as.numeric) %>% 
  mutate(HQ=as.factor(HQ))

tcc_df <- geopko %>% select(Source, Mission, Year, Month, No.troops, 52:85) %>% 
  group_by(Source, Mission, Year, Month) %>% 
  mutate(Total.troops=sum(No.troops, na.rm=T)) %>% ungroup()


#colnames(tcc_df) <- sub("name.of.TCC", "nameofTCC_", colnames(tcc_df)) obsolete script
#colnames(tcc_df) <- sub("No.troops.per.TCC", "notroopsperTCC_", colnames(tcc_df))
#tcc_df <- tcc_df %>% mutate_at(vars(starts_with("notroopsperTCC")), as.character) %>% 
#  mutate_at(vars(starts_with("nameofTCC")), as.character) %>% 

####map data prep####

cclist3 <- iso %>% select(Mission, a3) %>% distinct() #creating list of country codes for GADM sf files dowload 

map_df <- geopko %>% unite(joined_date, c("Year","Month"), sep=": ") %>% 
  mutate(timepoint=as.factor(joined_date)) %>% 
  mutate(slider_time=as.Date(joined_date, "%Y-%B"))

####lollipop dataprep####

Years <- geopko
Years <- Years %>% group_by(Mission, Location)%>% summarize(start_date=min(Year), end_date=max(Year))

####data prep for facet maps####


ui <- fluidPage(
  navbarPage("Exploring Geo-PKO",
             navbarMenu("Map Generator",
                        tabPanel("Static Maps", fluid=TRUE,
                                 titlePanel("Static Maps"),
                                 sidebarLayout(
                                   sidebarPanel(width=3, 
                                                p("Where are UN peacekeepers posted, and how many? Select the options below to visualise."),
                                                selectInput(inputId="mission_map", label="Select a mission",
                                                            choices=factor(geopko$Mission), width=150),
                                                selectInput("timepoint_map", 
                                                            "Choose year and month", choices=NULL),
                                                checkboxInput(inputId="depsize_map", 
                                                              "Deployment size", value=TRUE),
                                                checkboxInput(inputId="MHQ_map", 
                                                              "Mission HQ", value=FALSE),
                                                checkboxInput(inputId="SHQ_map", 
                                                              "Sector HQ", value=FALSE),
                                                checkboxInput(inputId="MO_map", 
                                                              "UNMO", value=FALSE),
                                                checkboxInput(inputId="UNPOL_map", 
                                                              "UNPOL", value=FALSE),
                                                helpText("Errors may occur when a selected feature is not available for a map. If that happens, please deselect the option.")
                                   ),
                                   mainPanel(fluid=TRUE,
                                             plotOutput("depmap"),
                                             span(h6(textOutput("basecountries"), align="center"))
                                   ))
                        ),
                        ####animated maps UI####
                        tabPanel("Animated Maps", fluid=TRUE,
                                 titlePanel("Animated Maps"),
                                 sidebarLayout(
                                   sidebarPanel(width=3, 
                                                p(""),
                                                selectInput(inputId="anim_map", label="Animated maps show changes over time. Select a mission to visualise.",
                                                            choices=factor(geopko$Mission), width=200)
                                                
                                                # sliderInput(inputId="anim_timeslider", 
                                                #             label= "Select deployment period", 
                                                #             value= c(x,y), 
                                                #             min= x,
                                                #             max= y
                                                #         )
                                   ),
                                   mainPanel(fluid=TRUE, 
                                             imageOutput("animated"))))),
             tabPanel("Contributing Countries",
                      basicPage(
                        radioButtons(inputId="databy_tcc", 
                                     label="Present data by:", 
                                     choices=c("Deployment map", "Year"),
                                     selected="Deployment map (default)"),
                        helpText("The Geo-PKO dataset collects data by deployment maps published by the UN. For the best accuracy, display data by deployment maps. Data by year present the year's average troop counts and the highest number of troop-contributing countries (TCCs)."),
                        # span(h6(textOutput("tabletext", align="right"))),
                        DT::dataTableOutput("tcc_table")
                      )),
             tabPanel("Missions",
                      sidebarLayout(
                        sidebarPanel(
                          p("Which locations had peacekeepers deployed, and when? These graphs show the years for each mission in which a location had at least one active deployment."),
                          selectInput(inputId="Lollipop_map", label="Select a mission",
                                      choices=factor(Years$Mission), width=150), width= 3,
                        ),
                        mainPanel(fluid=TRUE,
                                  plotOutput("lollipop", height="auto")
                        )
                      )),
             tabPanel ("Data",tags$div(
               includeMarkdown("data.Rmd")
             )),
             tabPanel ("About",tags$div(
               includeMarkdown("about.Rmd")
  )))
)



server <- function(input, output, session){
  #TCC tables
  bymap_df <- reactive({
    tcc_df %>% 
      pivot_longer(c(nameoftcc_1:notroopspertcc_17), names_to=c(".value", "tcc_id"), names_sep="_") %>%
      mutate(notroopspertcc=as.numeric(notroopspertcc)) %>%
      filter(!is.na(nameoftcc)) %>%
      select(-tcc_id) %>% 
      group_by(Source, Mission, Year, Month, Total.troops, nameoftcc)%>%
      summarise(total.tcc=sum(notroopspertcc, na.rm=TRUE)) %>% 
      add_count(Source, name="No.TCC") %>%
      mutate(total.tcc=ifelse(is.na(total.tcc), "unknown", total.tcc)) %>% 
      mutate(overview=paste0(nameoftcc," (",total.tcc,")")) %>%
      select(-nameoftcc, -total.tcc) %>%
      group_by(Source, Mission, Year, Month, Total.troops, No.TCC) %>%
      summarise(details=str_c(overview, collapse=", ")) %>% 
      arrange(desc(Year))}) 
  
  byyear_df <- reactive({
    tcc_df %>% 
      pivot_longer(c(nameoftcc_1:notroopspertcc_17), names_to=c(".value", "TCC_id"), names_sep="_")%>%
      mutate(notroopspertcc=as.numeric(notroopspertcc)) %>%
      filter(!is.na(nameoftcc)) %>%
      select(-TCC_id) %>% 
      group_by(Source, Mission, Year, Month, Total.troops, nameoftcc)%>%
      summarise(total.each.tcc=sum(notroopspertcc, na.rm=TRUE)) %>% 
      add_count(Source, name="No.TCC") %>%
      mutate(total.each.tcc=ifelse(is.na(total.each.tcc),"unknown", total.each.tcc)) %>% 
      mutate(overview=paste0(nameoftcc," (",total.each.tcc,")")) %>%
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
  
  ####deployment maps####
  
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
    paste0("This mission took place in ",paste(unique(map_df_temp()$Country), collapse=", "),".")
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
           caption="Sources: Geo-PKO v1.2\n Shapefiles from GADM.")+
      geom_blank()+
      scale_shape_manual(values=c(3, 5, 23), 
                         labels=c("SHQ"="Sector HQ", "UNMO"="Military Observers", "UNPOL"="UN Police"),
                         name="")
    if(input$depsize_map){
      p <- p + geom_point(data=map_df_temp(), aes(x=Longitude, y=Latitude, size=No.troops, color=as.factor(No.TCC)),
                          shape=20, alpha = 0.5)+
        scale_size_continuous(name="Size of deployment",range=c(2, 20))+
      #  scale_color_brewer(palette="Set1", name="Number of TCCs")+
        scale_color_discrete(name="Number of TCCs")+
        guides(colour = guide_legend(ncol=2, override.aes = list(size=2)))} 
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
  ####animated maps####
  anim_sf <- reactive({
    req(input$anim_map)
    cclist3 %>% filter(Mission %in% input$anim_map)
  })
  
  anim_df <- reactive({
    req(input$anim_map)
    map_df %>% filter(Mission %in% input$anim_map) %>% arrange(joined_date)
  }) 
  
  
  
  output$animated <- renderImage({
    outfile <- tempfile(fileext= '.gif')
    
    anim_maplist <- pull(anim_sf(), a3)
    anim_mapshapefiles <- gadm_sf_loadCountries(c(paste(anim_maplist)), level=1)
    mission_name <- anim_df() %>% distinct(Mission)
    colourCount = max(anim_df()$No.TCC)
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))
    
    anim_p <- ggplot() + geom_sf(data=anim_mapshapefiles$sf) + 
      theme_void() + 
      geom_point(data=anim_df(), aes(x=Longitude, y=Latitude, size=No.troops, 
                                     color=as.factor(No.TCC), group=joined_date),
                 shape=20, alpha = 0.5)+
      scale_size_continuous(name="Size of deployment",range=c(2, 20))+
      guides(color=guide_legend(ncol=2, override.aes = list(size=2)))+
      scale_color_discrete(name="Number of TCCs")+
      transition_states(states=anim_df()$joined_date)+
      labs(title=paste0(mission_name,": ", "{closest_state}"),
           caption="Sources: Geo-PKO v2.0\n Shapefiles from GADM.")+
      ease_aes('linear')
    
    anim_save("outfile.gif", animate(anim_p, fps = 4, width=650, height=400, res=100))
    
    list(src="outfile.gif",
         contentType='image/gif'
    )}, deleteFile= TRUE)
  
  
  # observeEvent(input$anim_map,{
  #   anim_temp <- map_df %>% filter(Mission %in% input$anim_map)
  #   
  #   updateSliderInput(session, 'anim_timepoint',
  #                     min = as.Date(min(anim_temp$slider_time), "%Y-%B"),
  #                     max = as.Date(max(anim_temp$slider_time), "%Y-%B"))
  #   
  # }) reserve for time input
  
  # map_df_temp <- reactive({
  #   req(input$mission_map)
  #   req(input$timepoint_map)
  #   map_df %>% filter(Mission %in% input$mission_map) %>%
  #     filter(timepoint %in% input$timepoint_map)
  #   
  # })
  
  ####lollipop####
  lollipop_df <- reactive({
    req(input$Lollipop_map)
    Years %>% filter(Mission %in% input$Lollipop_map) %>% 
      mutate_at(vars(c(start_date, end_date)), as.numeric)
  })
  
  height_lollipop <-  reactive({
    if(nrow(lollipop_df())<15){400}
    else{NROW(lollipop_df())*25+300}
  })
  
  
  output$lollipop <- renderPlot({
    lolli <-   ggplot(lollipop_df()) +
      geom_segment(aes(x=start_date, xend=end_date, 
                       y=fct_reorder(Location, start_date), 
                       yend=fct_reorder(Location, start_date)), color="grey") +
      geom_point(aes(x=end_date, y=Location), 
                 colour=rgb(0.9,0.3,0.1,0.9), size=3.5 ) +
      geom_point(aes(x=start_date, y=Location), 
                 colour=rgb(1.0,0.6,0.1,0.7), size=3) +
      scale_x_continuous(breaks = 
                           seq(1993,2020,1))+
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "none",
            axis.text.x = element_text(angle=45, hjust=1),
            axis.ticks.length.x= unit(0.1, "cm"),
            panel.grid.minor.x = element_blank(),
            panel.spacing.x = unit(1,"lines")
            
      ) +
      xlab("Years") +
      ylab("")+ #title already mentions locations, so no need for name
      labs(title=paste0(lollipop_df()$Mission), # to add (note from T but anyone can do) - , ": ", [mission's earliest year], " - ", [mission's latest year / "-" if ongoing]
           caption="Data: Geo-PKO v2.0")
    lolli
  }, height=height_lollipop)
  
}  



shinyApp(ui=ui, server=server)
