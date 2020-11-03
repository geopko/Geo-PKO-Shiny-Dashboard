#if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
#if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
#if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
#if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
#install.packages("plotly")
library(leaflet)
library(shiny)
library(dplyr)
library(tidyr)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(forcats)
library(sp)
library(htmltools)


setwd("C:/Users/tanus/Documents/R/Geo-PKO-Shiny-v2.0")

geopko2 <- read.csv("geopko2.csv")


#Basic Data modification
geopko2$NoTroops<-as.numeric(geopko2$No.troops)
geopko2$RPF_No<-as.numeric(geopko2$RPF_No)
geopko2$UNPOL<-as.numeric(geopko2$UNPOL.dummy)
geopko2$UNMO<-as.numeric(geopko2$UNMO.dummy)
geopko2$No.TCC<-as.numeric(geopko2$No.TCC)
geopko2$Av<- (geopko2$Avia + geopko2$HeSup)
geopko2$Infantry <- as.numeric(geopko2$Inf_No)
geopko2$HQ <- as.numeric(geopko2$HQ)
geopko2$Reserve <- as.numeric(geopko2$RES_No)

HQicon <- awesomeIcons(
  icon = 'fas fa-home',
  markerColor = "black",
  iconColor = "#f7fcff",
  library = 'fa'
)

Medicon <- awesomeIcons(
  icon = 'fas fa-plus',
  markerColor = "white",
  iconColor = "red",
  library = 'fa'
)

UNPOLicon <- awesomeIcons(
  icon = 'fab fa-product-hunt',
  markerColor = "blue",
  iconColor = "#f6f6f6",
  library = 'fa'
)
UNMOicon <- awesomeIcons(
  icon = 'fas fa-binoculars',
  markerColor = "darkblue",
  iconColor = "#f6f6f6",
  library = 'fa'
)

Avicon <- makeAwesomeIcon(
  icon = 'fas fa-plane',
  markerColor = "gray",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = 'fa'
)

Rivicon <- makeAwesomeIcon(
  icon = 'fas fa-anchor',
  markerColor = "cadetblue",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = 'fa'
)

Engicon <- makeAwesomeIcon(
  icon = 'fas fa-bolt',
  markerColor = "black",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = 'fa'
)

Sigicon <- makeAwesomeIcon(
  icon = 'fas fa-wifi',
  markerColor = "lightgray",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = 'fa'
)

Traicon <- makeAwesomeIcon(
  icon = 'fas fa-truck',
  markerColor = "darkpurple",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = 'fa'
)

Mainticon <- makeAwesomeIcon(
  icon = 'fas fa-wrench',
  markerColor = "darkgreen",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = 'fa'
)



#Shiny leaflet launch
gif_df <- geopko2 %>% select(Mission, Year, Country, Location, Latitude, Longitude, Infantry, NoTroops, Reserve, HQ, UNPOL, Med,Av,UNMO) %>%
  group_by(Mission, Year, Location, Country) %>% 
  mutate(Av = max(Av, na.rm=TRUE))%>%
  mutate(Med = max(Med, na.rm=TRUE))%>% 
  mutate(Infantry = as.integer(mean(Infantry, na.rm=TRUE)))%>%
  mutate(Reserve = as.integer(mean(Reserve, na.rm=TRUE)))%>%
  mutate(UNPOL = as.integer(mean(UNPOL, na.rm=TRUE)))%>% 
  mutate(UNMO = max(UNMO, na.rm=TRUE))%>% 
  mutate(ave.no.troops = as.integer(mean(NoTroops, na.rm=TRUE))) %>% 
  select(-NoTroops) %>% distinct() %>% drop_na(ave.no.troops)

gif_df$UNPOL <- str_replace_all(gif_df$UNPOL, "-Inf", "0")
gif_df$UNPOL <- as.numeric(gif_df$UNPOL)

###TCC dataframe
gif_df2 <- geopko2 %>% select(Source:Location, Latitude, Longitude,
                             No.TCC:notroopspertcc_17, HQ)

gif_df2<-gif_df2 %>% pivot_longer(nameoftcc_1:notroopspertcc_17,
                                  names_to=c(".value", "TCC_id"),
                                  names_sep="_") %>%
  filter(!is.na(nameoftcc)) %>%  #dropping empty tcc name cells
  mutate_at(vars(notroopspertcc), as.numeric) %>%
  group_by(Mission, Year, Location, Latitude, Longitude, nameoftcc) %>%
  summarise(count.per.tcc.year=as.character(max(notroopspertcc))) %>%
  mutate(count.per.tcc.year=ifelse(is.na(count.per.tcc.year), "unknown", count.per.tcc.year),
         single.tcc=paste0(nameoftcc, " (",count.per.tcc.year,(")"))) %>%
  add_count(Year, Location, name="No.TCC")%>%
  group_by(Mission, Year, Location, Latitude, Longitude, No.TCC) %>%
  summarise(year.overview = str_c(single.tcc, collapse=", "))

##Troop Type Dataframe
gif_df3 <- geopko2 %>% select(Source:Location, Latitude, Longitude, Infantry,
                             Eng:MP) %>%group_by(Mission, Year, Location)%>% mutate(Infantry = as.integer(mean(Infantry, na.rm=TRUE)))%>% distinct()

###Legend colours
qpal <- colorBin(rev(viridis::viridis(10)), gif_df$ave.no.troops, bins = c(10,50,100,500,1000,2000,4000,8000))
qpal2 <- colorBin((viridis::viridis(2)), gif_df2$No.TCC, bins = c(1,2,4,7,10,15,20))
qpal3 <- colorBin(rev(viridis::viridis(10)), gif_df3$Infantry, bins = c(10,50,100,500,1000,2000,4000,8000))


###Jitter the data so markers are not directly on top of each other
# gif_df$Latitude <- jitter(gif_df$Latitude, factor = 0.0001)
# gif_df$Longitude <- jitter(gif_df$Longitude, factor = 0.0001)
# gif_df2$Latitude <- jitter(gif_df2$Latitude, factor = 0.0001)
# gif_df2$Longitude <- jitter(gif_df2$Longitude, factor = 0.0001)
# gif_df3$Latitude <- jitter(gif_df3$Latitude, factor = 0.0001)
# gif_df3$Longitude <- jitter(gif_df3$Longitude, factor = 0.0001)


####Map doesnt load on initial go, so need to make the base here
TCC_basemap <- leaflet(geopko2, options = leafletOptions(minZoom = 2)) %>% 
  addTiles()  %>% 
  hideGroup("Mission HQ")%>%
  fitBounds(~-70,-50,~60,60) %>%
  addLegend(pal = qpal2, values = ~gif_df2$No.TCC, group = "TCC", title= "Number of TCCs") %>%
  addCircleMarkers(data= (gif_df23<-gif_df2%>%filter(Year==2020)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC), 
                   fillOpacity = 0.8, color = ~qpal2(No.TCC), group = "TCC", labelOptions = labelOptions(style= list(
                     "width"= "150px", "white-space"="normal")),
                   label = paste("<strong>", gif_df23$Mission,"</strong><br/><strong>Location:</strong>",gif_df23$Location,"<br/><strong>Total number of TCCs:</strong>",gif_df23$No.TCC, "<br/><strong>Countries:</strong>",gif_df23$year.overview)%>% lapply(htmltools::HTML))

####BaseMap Third Panel
TroopType_basemap <- leaflet(geopko2, options = leafletOptions(minZoom = 2)) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    baseGroups = c("Infantry", "None"),
    overlayGroups = c("Medical", "Engineering", "Signals", "Aviation", "Transport", "Maintenance", "Riverine"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("Medical","Aviation", "Engineering", "Transport", "Signals","Maintenance","Riverine"))%>%
  fitBounds(~-70,-50,~60,60) %>%
  addLegend(pal = qpal3, values = ~gif_df3$Infantry, group = "Infantry", title= "Number of troops") %>%
  addCircleMarkers(data=(gif_df31<-gif_df3%>%filter(Year==2020)%>%filter(Infantry>1)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(Infantry)^(1/3.5), 
                   fillOpacity = 0.6, color = ~qpal3(Infantry), group = "Infantry", 
                   label = paste("<strong>Location:</strong>",gif_df31$Location)%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df32<-gif_df3%>%filter(Year==2020)%>%filter(Med>0)), lat = ~Latitude+0.2, lng = ~Longitude+0.2, icon = Medicon, group = "Medical",
                    label=paste("<strong>Medical</strong><br/>", gif_df32$Mission," (",gif_df32$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df33<-gif_df3%>%filter(Year==2020)%>%filter(Eng>0)), lat = ~Latitude, lng = ~Longitude, icon = Engicon, group = "Engineering",
             label=paste("<strong>Engineering</strong><br/>", gif_df33$Mission," (",gif_df33$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df35<-gif_df3%>%filter(Year==2020)%>%filter(Sig>0)), lat = ~Latitude-0.2, lng = ~Longitude-0.2, icon = Sigicon, group = "Signals",
             label=paste("<strong>Signal</strong><br/>", gif_df35$Mission," (",gif_df35$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df34<-gif_df3%>%filter(Year==2020)%>%filter(Avia>0)), lat = ~Latitude+0.4, lng = ~Longitude+0.4, icon = Avicon, group = "Aviation",
             label=paste("<strong>Aviation</strong><br/>", gif_df34$Mission," (",gif_df34$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df36<-gif_df3%>%filter(Year==2020)%>%filter(Riv>0)), lat = ~Latitude-0.6, lng = ~Longitude-0.6, icon = Rivicon, group = "Riverine",
             label=paste("<strong>Riverine</strong><br/>", gif_df36$Mission,"(",gif_df36$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df37<-gif_df3%>%filter(Year==2020)%>%filter(Maint>0)), lat = ~Latitude-0.4, lng = ~Longitude-0.4, icon = Mainticon, group = "Maintenance",
             label=paste("<strong>Maintenance</strong><br/>", gif_df37$Mission," (",gif_df37$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df38<-gif_df3%>%filter(Year==2020)%>%filter(Trans>0)), lat = ~Latitude+0.6, lng = ~Longitude+0.6, icon = Traicon, group = "Transport",
             label=paste("<strong>Transport</strong><br/>", gif_df38$Mission," (",gif_df38$Location,")<br/>")%>% lapply(htmltools::HTML))






#Shiny App Code

####UI###

ui <- fluidPage(
  navbarPage ("Geo-PKO",
              navbarMenu("Troop Deployments",
                         tabPanel("Overview",tags$style(type = "text/css", "#basemap {height: calc(100vh - 130px) !important;}"), leafletOutput("basemap"),
                                  absolutePanel(top = 70, left = 85, width="20%", style = "padding: 16px; background:rgba(232, 232, 232, 0.8)",
                                                span(h6("This interactive map shows peacekeeping deployments from 1994-2020, based on publicly available United Nations (UN) peacekeeping deployment maps and mission progress reports. 'Mission Site' indicates where there are no active troop deployments, but the presence of support personnel such as UNPOL (UN Police) and/or UNMO (UN Military Observer).", align = "Left"), style="color:#15110d"),
                                                br(),
                                                span(h5(tags$b(textOutput("reactive_year"), align = "left"), style="color:#15110d")),
                                                span(h4(textOutput("reactive_troopcount"), align = "left"), style="color:#15110d"),
                                                span(h4(textOutput("reactive_UNPOLcount"), align = "left"), style="color:#666666"),
                                                span(h4(textOutput("reactive_UNMOcount"), align = "left"), style="color:#666666"),
                                                br(),
                                                pickerInput("missions","Select mission(s)", choices=as.character(unique(gif_df$Mission)),selected =as.character(unique(gif_df$Mission)) , options = list(`actions-box` = TRUE),multiple = T),
                                                chooseSliderSkin("Shiny", color = "transparent"),
                                                setSliderColor("transparent", 1),
                                                br(),
                                                sliderInput(inputId = "plot_date", 
                                                            label = "Select deployment year (1994-2020)",
                                                            min = 1994,
                                                            max = 2020,
                                                            value =2020,
                                                            step = 1,
                                                            sep= "",
                                                            animate = animationOptions(interval = 1500, loop = TRUE)), tags$style(type= "text/css", HTML(".irs-single {color:black; background:transparent}"))
                                  )),
                         tabPanel("Contributing Countries",
                                  sidebarLayout(sidebarPanel( "This map shows how many troop-contributing countries (TCCs) have deployed peacekeepers to a location. TCCs and the number of troops each country has contributed are shown in the labels.<br/><br/>"%>% lapply(htmltools::HTML),
                                                              chooseSliderSkin("Shiny", color = "transparent"),
                                                              setSliderColor("transparent", 1),
                                                              sliderInput(inputId = "plot_date2", 
                                                                          label = "Select year (1994-2020)",
                                                                          min = 1994,
                                                                          max = 2020,
                                                                          value =2020,
                                                                          step = 1,
                                                                          sep= "",
                                                                          width = "100%",
                                                                          animate = animationOptions(interval = 2000, loop = TRUE)),
                                                              pickerInput("missions2","Select mission(s)", 
                                                                          choices=as.character(unique(gif_df2$Mission)),
                                                                          selected =as.character(unique(gif_df2$Mission)) , 
                                                                          options = list(`actions-box` = TRUE),multiple = T), width = 3),
                                                mainPanel ( tags$style(type = "text/css", "#map {height: calc(100vh - 130px) !important;}"),leafletOutput("map",width = "115%")),
                                                position = c("left", "right")
                                  )),
                         tabPanel("Troop Types",
                                  sidebarLayout(sidebarPanel(paste ("This map shows the various types of troops deployed to a location. In the dataset, a specific number of deployed troops is provided for infantry. For other troop types, it is only indicated whether they are present or absent. Aviation is coded for aircraft, UAVs and helicopter support.<br/><br/>When selecting different troop types, overlap can occur. If no icons appear when selecting a troop type, this type is not present in the selected year.<br/><br/>")%>% lapply(htmltools::HTML),
                                                             chooseSliderSkin("Shiny", color = "transparent"),
                                                             setSliderColor("transparent", 1),
                                                             sliderInput(inputId = "plot_date3", 
                                                                         label = "Select year (1994-2020)",
                                                                         min = 1994,
                                                                         max = 2020,
                                                                         value =2020,
                                                                         step = 1,
                                                                         sep= "",
                                                                         width = "100%",
                                                                         animate = animationOptions(interval = 2000, loop = TRUE)),
                                                             pickerInput("missions3","Select mission(s)", 
                                                                         choices=as.character(unique(gif_df3$Mission)),
                                                                         selected =as.character(unique(gif_df3$Mission)) , 
                                                                         options = list(`actions-box` = TRUE),multiple = T), width = 3),
                                                mainPanel ( tags$style(type = "text/css", "#TroopTypeMap {height: calc(100vh - 130px) !important;}"), leafletOutput("TroopTypeMap", width = "115%")),####Screen size, responsive to different types
                                                position = c("left", "right")
                                  ))
              )))



#################Server#####################
server <- function(input, output, session){
  
  ####Reactive Data Frames 
  filteredData <- reactive({
    gif_df %>% filter(Mission %in% input$missions & Year %in% input$plot_date)
  })
  
  filteredDataTCC <- reactive({
    gif_df2 %>% filter(Mission %in% input$missions2 & Year %in% input$plot_date2)
  })
  
  filteredDataTroopType <- reactive({
    gif_df3 %>% filter(Mission %in% input$missions3 & Year %in% input$plot_date3)
  })
  
  #####Front map basis  
  output$basemap <- renderLeaflet({
    leaflet(geopko2, options = leafletOptions(minZoom = 2)) %>% 
      addTiles() %>% 
      addLayersControl(
        position = "bottomright",
        baseGroups = c("Deployments (All)", "Troops (Infantry)","Troops (Reserve)","Mission Site (No Troops)","None"),
        overlayGroups = c("UNPOL", "UNMO", "Mission HQs"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(c("UNPOL", "UNMO", "Mission HQs"))  %>%
      fitBounds(~-70,-50,~60,60) %>%
      addLegend(pal = qpal, values = ~gif_df$ave.no.troops, group = "Troop deployment", title= "Number of troops")
  })
  
  
  ####Map for TCC  
  output$map <- renderLeaflet({
    TCC_basemap
  })
  
  ####Map for Troop Types 
  output$TroopTypeMap <- renderLeaflet({
    TroopType_basemap
  })
  
  #Reactive Text for the front page
  output$reactive_year <- renderText({
    paste0("In ",unique(filteredData()$Year), " there were:")
  }) 
  
  output$reactive_troopcount <- renderText({
    paste0(prettyNum(sum(filteredData()$ave.no.troops), big.mark=","), "  peacekeepers deployed")
  }) 
  
  output$reactive_UNPOLcount <- renderText({
    paste0(prettyNum(sum(filteredData()$UNPOL, na.rm=TRUE), big.mark=","), " UNPOL deployments")
  }) 
  
  output$reactive_UNMOcount <- renderText({
    paste0(prettyNum(sum(filteredData()$UNMO, na.rm=TRUE), big.mark=","), " UNMO deployments")
  })
  
  
  
  
  ###Generate the troop deployment map
  observe({
    leafletProxy(mapId = "basemap", data = filteredData()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(data = (filteredData1<-filteredData()%>%filter(ave.no.troops>0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(ave.no.troops)^(1/3.5), 
                       fillOpacity = 0.6, color = ~qpal(ave.no.troops), group = "Deployments (All)", 
                       label=paste("<strong>Troop number:</strong>", filteredData1$ave.no.troops,"<br/><strong>Mission:</strong>", filteredData1$Mission,"<br/><strong>Location:</strong>",filteredData1$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData10<-filteredData()%>%filter(ave.no.troops==0)), lat = ~Latitude, lng = ~Longitude, weight = 0.5, radius = 3, 
                       fillOpacity = 0.4, color = "#666666", group = "Deployments (All)", 
                       label=paste("<strong>Mission site (no troop deployment)</strong><br/><strong>Mission:</strong>", filteredData10$Mission,"<br/><strong>Location:</strong>",filteredData10$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData7<-filteredData()%>%filter(Infantry>0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(ave.no.troops)^(1/3.5), 
                       fillOpacity = 0.6, color = ~qpal(Infantry), group = "Troops (Infantry)", 
                       label=paste("<strong>Troop number:</strong>", filteredData7$Infantry,"<br/><strong>Mission:</strong>", filteredData7$Mission,"<br/><strong>Location:</strong>",filteredData7$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData8<-filteredData()%>%filter(ave.no.troops==0)), lat = ~Latitude, lng = ~Longitude, weight = 0.5, radius = 3, 
                       fillOpacity = 0.4, color = "#666666", group = "Mission Site (No Troops)", 
                       label=paste("<strong>Mission site (no troop deployment)</strong><br/><strong>Mission:</strong>", filteredData8$Mission,"<br/><strong>Location:</strong>",filteredData8$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData9<-filteredData()%>%filter(Reserve>0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(Reserve)^(1/3.5), 
                       fillOpacity = 0.6, color = ~qpal(Infantry), group = "Troops (Reserve)", 
                       label=paste("<strong>Troop number:</strong>", filteredData9$Reserve,"<br/><strong>Mission:</strong>", filteredData9$Mission,"<br/><strong>Location:</strong>",filteredData9$Location)%>% lapply(htmltools::HTML)) %>%
      addAwesomeMarkers(data = (filteredData2<-filteredData()%>%filter(UNPOL>0)), lat = ~Latitude, lng = ~Longitude,icon=UNPOLicon, group = "UNPOL", 
                        label=paste("<strong>UNPOL</strong> (",filteredData2$Mission,")<br/><strong>Location:</strong>",filteredData2$Location)%>% lapply(htmltools::HTML)) %>%
      addAwesomeMarkers(data = (filteredData3<-filteredData()%>%filter(UNMO>0)), lat = ~Latitude, lng = ~Longitude, icon=UNMOicon, group = "UNMO", 
                        label=paste("<strong>UNMO <br/>Mission:</strong>", filteredData3$Mission,"<br/><strong>Location:</strong>",filteredData3$Location)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredData4<-filteredData()%>%filter(HQ==3)), lat = ~Latitude, lng = ~Longitude, icon = HQicon, group = "Mission HQs", 
                        label=paste("<strong>Mission HQ:</strong>", filteredData4$Mission,"<br/><strong>Location:</strong>",filteredData4$Location,filteredData4$Country)%>% lapply(htmltools::HTML))
  })
  
  
  ####Second observe for TCC map   
  observe({
    leafletProxy(mapId = "map", data = filteredDataTCC()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(data = filteredDataTCC(), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1.5), 
                       fillOpacity = 0.6, color = ~qpal2(No.TCC), group = "TCC", labelOptions = labelOptions(style= list(
                         "width"= "150px", "white-space"="normal")),
                       label = paste("<strong>", filteredDataTCC()$Mission,"</strong><br/><strong>Location:</strong>",filteredDataTCC()$Location, "<br/><strong>Total number of TCCs:</strong>",filteredDataTCC()$No.TCC,"<br/><strong>Countries:</strong>",filteredDataTCC()$year.overview)%>% lapply(htmltools::HTML))
    })
  
  
  ####Third observe for Troop Type map   
  observe({
    leafletProxy(mapId = "TroopTypeMap", data = filteredDataTroopType()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(data = (filteredDataTroopType0<-filteredDataTroopType()%>%filter(Infantry>0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(Infantry)^(1/3.5), 
                       fillOpacity = 0.6, color = ~qpal3(Infantry), group = "Infantry", 
                       label = paste("<strong>Location:</strong>",filteredDataTroopType()$Location)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType1<-filteredDataTroopType()%>%filter(Med>0)), lat = ~Latitude+0.2, lng = ~Longitude+0.2, icon = Medicon, group = "Medical",
                        label=paste("<strong>Medical</strong><br/>", filteredDataTroopType1$Mission," (",filteredDataTroopType1$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType2<-filteredDataTroopType()%>%filter(Eng>0)), lat = ~Latitude, lng = ~Longitude, icon = Engicon, group = "Engineering",
                 label=paste("<strong>Engineering</strong><br/>", filteredDataTroopType2$Mission," (",filteredDataTroopType2$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType4<-filteredDataTroopType()%>%filter(Sig>0)), lat = ~Latitude-0.2, lng = ~Longitude-0.2, icon = Sigicon, group = "Signals",
                 label=paste("<strong>Signal</strong><br/>", filteredDataTroopType4$Mission," (",filteredDataTroopType4$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType3<-filteredDataTroopType()%>%filter(Avia>0)), lat = ~Latitude+0.4, lng = ~Longitude+0.4, icon = Avicon, group = "Aviation",
                 label=paste("<strong>Aviation</strong><br/>", filteredDataTroopType3$Mission," (",filteredDataTroopType3$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType5<-filteredDataTroopType()%>%filter(Riv>0)), lat = ~Latitude-0.6, lng = ~Longitude-0.6, icon = Rivicon, group = "Riverine",
                 label=paste("<strong>Riverine</strong><br/>", filteredDataTroopType5$Mission,"(",filteredDataTroopType5$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType6<-filteredDataTroopType()%>%filter(Maint>0)), lat = ~Latitude-0.4, lng = ~Longitude-0.4, icon = Mainticon, group = "Maintenance",
                 label=paste("<strong>Maintenance</strong><br/>", filteredDataTroopType6$Mission," (",filteredDataTroopType6$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType7<-filteredDataTroopType()%>%filter(Trans>0)), lat = ~Latitude+0.6, lng = ~Longitude+0.6, icon = Traicon, group = "Transport",
                 label=paste("<strong>Transport</strong><br/>", filteredDataTroopType7$Mission," (",filteredDataTroopType7$Location,")<br/>")%>% lapply(htmltools::HTML))
  })
  
  
}
###Launch the app
shinyApp(ui, server)

