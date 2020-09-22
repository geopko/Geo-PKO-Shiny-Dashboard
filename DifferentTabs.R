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

GeoPKO <- geopko2


#Basic Data modification
GeoPKO$NoTroops<-as.numeric(GeoPKO$No.troops)
GeoPKO$RPF_No<-as.numeric(GeoPKO$RPF_No)
GeoPKO$UNPOL<-as.numeric(GeoPKO$UNPOL.dummy)
GeoPKO$UNMO<-as.numeric(GeoPKO$UNMO.dummy)
GeoPKO$No.TCC<-as.numeric(GeoPKO$No.TCC)
GeoPKO$Av<- (GeoPKO$Avia + GeoPKO$HeSup)
GeoPKO$Infantry <- as.numeric(GeoPKO$Inf_No)
GeoPKO$HQ <- as.numeric(GeoPKO$HQ)
GeoPKO$Reserve <- as.numeric(GeoPKO$RES_No)
GeoPKO$notroopspertcc_1<- as.numeric(GeoPKO$notroopspertcc_1)
GeoPKO$notroopspertcc_2<- as.numeric(GeoPKO$notroopspertcc_2)
GeoPKO$notroopspertcc_3<- as.numeric(GeoPKO$notroopspertcc_3)
GeoPKO$notroopspertcc_4<- as.numeric(GeoPKO$notroopspertcc_4)
GeoPKO$notroopspertcc_5<- as.numeric(GeoPKO$notroopspertcc_5)
GeoPKO$notroopspertcc_6<- as.numeric(GeoPKO$notroopspertcc_6)
GeoPKO$notroopspertcc_7<- as.numeric(GeoPKO$notroopspertcc_7)
GeoPKO$notroopspertcc_8<- as.numeric(GeoPKO$notroopspertcc_8)
GeoPKO$notroopspertcc_9<- as.numeric(GeoPKO$notroopspertcc_9)
GeoPKO$notroopspertcc_10<- as.numeric(GeoPKO$notroopspertcc_10)
GeoPKO$notroopspertcc_11<- as.numeric(GeoPKO$notroopspertcc_11)
GeoPKO$notroopspertcc_12<- as.numeric(GeoPKO$notroopspertcc_12)
GeoPKO$notroopspertcc_13<- as.numeric(GeoPKO$notroopspertcc_13)
GeoPKO$notroopspertcc_14<- as.numeric(GeoPKO$notroopspertcc_14)
GeoPKO$notroopspertcc_15<- as.numeric(GeoPKO$notroopspertcc_15)
GeoPKO$notroopspertcc_16<- as.numeric(GeoPKO$notroopspertcc_16)
GeoPKO$notroopspertcc_17<- as.numeric(GeoPKO$notroopspertcc_17)
GeoPKO$Year<- as.numeric(GeoPKO$Year)



#####Specify awesomemarkers icons
HQicon <- awesomeIcons(
  icon = 'fas fa-home',
  markerColor = "red",
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

Avicon <- awesomeIcons(
  icon = 'fas fa-plane',
  markerColor = "green",
  iconColor = "#f6f6f6",
  library = 'fa'
)


#Shiny leaflet launch
gif_df <- GeoPKO %>% select(Mission, Year, Country, Location, Latitude, Longitude, Infantry, NoTroops, Reserve, HQ, UNPOL, Med,Av,UNMO) %>%
  group_by(Mission, Year, Location, Country) %>% 
  mutate(Av = max(Av, na.rm=TRUE))%>%
  mutate(Med = max(Med, na.rm=TRUE))%>% 
  mutate(Infantry = as.integer(mean(Infantry, na.rm=TRUE)))%>%
  mutate(Reserve = as.integer(mean(Reserve, na.rm=TRUE)))%>%
  mutate(UNPOL = as.integer(mean(UNPOL, na.rm=TRUE)))%>% 
  mutate(UNMO = max(UNMO, na.rm=TRUE))%>% 
  mutate(ave.no.troops = as.integer(mean(NoTroops, na.rm=TRUE))) %>% 
  select(-NoTroops) %>% distinct() %>% drop_na(ave.no.troops)

gif_df$UNPOL <- str_replace_all(gif_df$UNPOL, "-Inf", "1")
gif_df$UNPOL <- as.numeric(gif_df$UNPOL)

###TCC dataframe
gif_df2 <- GeoPKO %>% select(Source:Location, Latitude, Longitude,
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
gif_df3 <- GeoPKO %>% select(Source:Location, Latitude, Longitude, Infantry,
                             Eng:MP) %>%group_by(Mission, Year, Location)%>% mutate(Infantry = as.integer(mean(Infantry, na.rm=TRUE)))%>% distinct()

###Legend colours
qpal <- colorNumeric(c("#ffc100", "#ff9a00", "#ff7400", "#ff4d00","#dc6900","#e0301e","#a32020", "#602320", "#451d1b", "#060606"), gif_df$ave.no.troops)
qpal2 <- colorNumeric(c("#168294", "#2d8e9e", "#136f7e", "#5ba7b4","#73b4be","#20bdd7","#136f7e", "#a1cdd4", "#b9d9de", "#d0e6e9"), gif_df2$No.TCC)
qpal3 <- colorNumeric(c("#58180d", "#942816", "#9e3d2d", "#a95244","#b4685b","#be7e73","#c9938a", "#d4a9a1", "#debeb9", "#e9d4d0"),gif_df3$Infantry)


####Map doesnt load on initial go, so need to make the base here
TCC_basemap <- leaflet(GeoPKO, options = leafletOptions(minZoom = 2)) %>% 
  addTiles()  %>% 
  hideGroup("Mission HQ")%>%
  fitBounds(~-70,-50,~60,60) %>%
  addLegend(pal = qpal2, values = ~gif_df2$No.TCC, group = "TCC", title= "Legend") %>%
  addCircleMarkers(data= (gif_df23<-gif_df2%>%filter(Year==1994)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC), 
                   fillOpacity = 0.8, color = ~qpal2(No.TCC), group = "TCC", labelOptions = labelOptions(style= list(
                     "width"= "150px", "white-space"="normal")),
                   label = paste("<strong>", gif_df23$Mission,"</strong><br/><strong>Location:</strong>",gif_df23$Location,"<br/><strong>Total number of TCCs:</strong>",gif_df23$No.TCC, "<br/><strong>Details:</strong>",gif_df23$year.overview)%>% lapply(htmltools::HTML))

####BaseMap Third Panel
TroopType_basemap <- leaflet(GeoPKO, options = leafletOptions(minZoom = 2)) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    baseGroups = c("Infantry", "None"),
    overlayGroups = c("Medical","Aviation", "Engineer", "Transport", "Signals","Maintenance","Riverine"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("Medical","Aviation", "Engineer", "Transport", "Signals","Maintenance","Riverine"))%>%
  fitBounds(~-70,-50,~60,60) %>%
  addLegend(pal = qpal3, values = ~gif_df3$Infantry, group = "Infantry", title= "Legend") %>%
  addCircleMarkers(data=(gif_df31<-gif_df3%>%filter(Year==1994)%>%filter(Infantry>1)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(Infantry)^(1/3.5), 
                   fillOpacity = 0.8, color = ~qpal3(Infantry), group = "Infantry", 
                   label = paste("<strong>Location:</strong>",gif_df31$Location)%>% lapply(htmltools::HTML))%>%
  addAwesomeMarkers(data = (gif_df32<-gif_df3%>%filter(Year==1994)%>%filter(Med>0)), lat = ~Latitude, lng = ~Longitude, icon = Medicon, group = "Medical",
                    label=paste("<strong>Medical Unit</strong><br/>", gif_df32$Mission," (",gif_df32$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addMarkers(data = (gif_df33<-gif_df3%>%filter(Year==1994)%>%filter(Eng>0)), lat = ~Latitude, lng = ~Longitude, group = "Engineer",
             label=paste("<strong>Engineer Unit</strong><br/>", gif_df33$Mission," (",gif_df33$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addMarkers(data = (gif_df34<-gif_df3%>%filter(Year==1994)%>%filter(Avia>0)), lat = ~Latitude, lng = ~Longitude, group = "Aviation",
             label=paste("<strong>Aviation Unit</strong><br/>", gif_df34$Mission," (",gif_df34$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addMarkers(data = (gif_df35<-gif_df3%>%filter(Year==1994)%>%filter(Sig>0)), lat = ~Latitude, lng = ~Longitude, group = "Signals",
             label=paste("<strong>Signal Unit</strong><br/>", gif_df35$Mission," (",gif_df35$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addMarkers(data = (gif_df36<-gif_df3%>%filter(Year==1994)%>%filter(Riv>0)), lat = ~Latitude, lng = ~Longitude, group = "Riverine",
             label=paste("<strong>Riverine Unit</strong><br/>", gif_df36$Mission,"(",gif_df36$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addMarkers(data = (gif_df37<-gif_df3%>%filter(Year==1994)%>%filter(Maint>0)), lat = ~Latitude, lng = ~Longitude, group = "Maintenance",
             label=paste("<strong>Maintenance Unit</strong><br/>", gif_df37$Mission," (",gif_df37$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
  addMarkers(data = (gif_df38<-gif_df3%>%filter(Year==1994)%>%filter(Trans>0)), lat = ~Latitude, lng = ~Longitude, group = "Transport",
             label=paste("<strong>Transport Unit</strong><br/>", gif_df38$Mission," (",gif_df38$Location,")<br/>")%>% lapply(htmltools::HTML))






#Shiny App Code

####UI###
ui <- fluidPage(
  navbarPage ("Exploring GeoPKO",
              navbarMenu("Interactive Maps",
                         tabPanel("Overview",tags$style(type = "text/css", "#basemap {height: calc(100vh - 130px) !important;}"), leafletOutput("basemap"),
                                  absolutePanel(top = 70, left = 85, width="20%", style = "background:rgba(232, 232, 232, 0.8)",
                                                span(h6("Select layers in the bottom right corner. Aviation includes both the variable of helicopter units as well as aviation in general. Please note that when selecting multiple layers overlap may occur.", align = "Left"), style="color:#15110d"),
                                                span(h5(tags$b(textOutput("reactive_year"), align = "Left"), style="color:#15110d")),
                                                span(h4(textOutput("reactive_troopcount"), align = "center"), style="color:#15110d"),
                                                span(h6(textOutput("reactive_UNPOLcount"), align = "right"), style="color:#527bd2"),
                                                span(h6(textOutput("reactive_UNMOcount"), align = "right"), style="color:#363b74"),
                                                pickerInput("missions","Select Mission(s)", choices=as.character(unique(gif_df$Mission)),selected =as.character(unique(gif_df$Mission)) , options = list(`actions-box` = TRUE),multiple = T),
                                                chooseSliderSkin("Shiny", color = "transparent"),
                                                setSliderColor("transparent", 1),
                                                sliderInput(inputId = "plot_date", 
                                                            label = "Select deployment year (1994-2020)",
                                                            min = 1994,
                                                            max = 2020,
                                                            value =1994,
                                                            step = 1,
                                                            sep= "",
                                                            animate = animationOptions(interval = 1500, loop = FALSE)), tags$style(type= "text/css", HTML(".irs-single {color:black; background:transparent}"))
                                  )),
                         tabPanel("TCCs",
                                          sidebarLayout(sidebarPanel( "This map shows an overview of the troop contributing countries (TCC). The TCCs and number of troops per TCC are shown in the label.",
                                                                           chooseSliderSkin("Shiny", color = "transparent"),
                                                                           setSliderColor("transparent", 1),
                                                                           sliderInput(inputId = "plot_date2", 
                                                                                       label = "Select year (1994-2020)",
                                                                                       min = 1994,
                                                                                       max = 2020,
                                                                                       value =1994,
                                                                                       step = 1,
                                                                                       sep= "",
                                                                                       width = "100%",
                                                                                       animate = animationOptions(interval = 2000, loop = FALSE)),
                                                                           pickerInput("missions2","Select Mission(s)", 
                                                                                       choices=as.character(unique(gif_df2$Mission)),
                                                                                       selected =as.character(unique(gif_df2$Mission)) , 
                                                                                       options = list(`actions-box` = TRUE),multiple = T), width = 3),
                                                             mainPanel ( tags$style(type = "text/css", "#map {height: calc(100vh - 130px) !important;}"),leafletOutput("map",width = "115%")),
                                                             position = c("left", "right")
                                               )),
                                      tabPanel("Troop Type",
                                               sidebarLayout(sidebarPanel(paste ("This map shows an overview of the troop types. When selecting different troop types overlap can occur.If no icons appear when selecting a troop type, this type is not present in the selected year.<br/>-----")%>% lapply(htmltools::HTML),
                                                                           chooseSliderSkin("Shiny", color = "transparent"),
                                                                           setSliderColor("transparent", 1),
                                                                           sliderInput(inputId = "plot_date3", 
                                                                                       label = "Select year (1994-2020)",
                                                                                       min = 1994,
                                                                                       max = 2020,
                                                                                       value =1994,
                                                                                       step = 1,
                                                                                       sep= "",
                                                                                       width = "100%",
                                                                                       animate = animationOptions(interval = 2000, loop = FALSE)),
                                                                           pickerInput("missions3","Select Mission(s)", 
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
    leaflet(GeoPKO, options = leafletOptions(minZoom = 2)) %>% 
      addTiles() %>% 
      addLayersControl(
        position = "bottomright",
        baseGroups = c("Troop deployment (All)", "Troop deployment (Infantry only)","Troop deployment (Reserve)","Locations without active troop deployment","None"),
        overlayGroups = c("Medical units","Aviation","UNPOL", "UNMO", "Mission HQ"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(c("Medical units","Aviation", "UNPOL", "UNMO", "Mission HQ"))  %>%
      fitBounds(~-70,-50,~60,60) %>%
      addLegend(pal = qpal, values = ~gif_df$ave.no.troops, group = "Troop deployment", title= "Legend")
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
    paste0(prettyNum(sum(filteredData()$ave.no.troops), big.mark=","), " deployed peacekeepers")
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
                       fillOpacity = 0.5, color = ~qpal(ave.no.troops), group = "Troop deployment (All)", 
                       label=paste("<strong>Troop number:</strong>", filteredData1$ave.no.troops,"<br/><strong>Mission:</strong>", filteredData1$Mission,"<br/><strong>Location:</strong>",filteredData1$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData10<-filteredData()%>%filter(ave.no.troops==0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(ave.no.troops)^(1), 
                       fillOpacity = 0.5, color = "#0b0a0a", group = "Troop deployment (All)", 
                       label=paste("<strong>No Active Troop Deployment</strong><br/><strong>Mission:</strong>", filteredData10$Mission,"<br/><strong>Location:</strong>",filteredData10$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData7<-filteredData()%>%filter(Infantry>0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(ave.no.troops)^(1/3.5), 
                       fillOpacity = 0.5, color = ~qpal(Infantry), group = "Troop deployment (Infantry only)", 
                       label=paste("<strong>Troop number:</strong>", filteredData7$Infantry,"<br/><strong>Mission:</strong>", filteredData7$Mission,"<br/><strong>Location:</strong>",filteredData7$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData8<-filteredData()%>%filter(ave.no.troops==0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(ave.no.troops)^(1), 
                       fillOpacity = 0.5, color = "#0b0a0a", group = "Locations without active troop deployment", 
                       label=paste("<strong>No Active Troop Deployment</strong><br/><strong>Mission:</strong>", filteredData8$Mission,"<br/><strong>Location:</strong>",filteredData8$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData9<-filteredData()%>%filter(Reserve>0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(Reserve)^(1/3.5), 
                       fillOpacity = 0.5, color = ~qpal(Infantry), group = "Troop deployment (Reserve)", 
                       label=paste("<strong>Troop number:</strong>", filteredData9$Reserve,"<br/><strong>Mission:</strong>", filteredData9$Mission,"<br/><strong>Location:</strong>",filteredData9$Location)%>% lapply(htmltools::HTML)) %>%
      addAwesomeMarkers(data = (filteredData2<-filteredData()%>%filter(UNPOL>0)), lat = ~Latitude, lng = ~Longitude,icon=UNPOLicon, group = "UNPOL", 
                        label=paste("<strong>UNPOL</strong> (",filteredData2$Mission,")<br/><strong>Location:</strong>",filteredData2$Location)%>% lapply(htmltools::HTML)) %>%
      addAwesomeMarkers(data = (filteredData3<-filteredData()%>%filter(UNMO>0)), lat = ~Latitude, lng = ~Longitude, icon=UNMOicon, group = "UNMO", 
                        label=paste("<strong>UNMO <br/>Mission:</strong>", filteredData3$Mission,"<br/><strong>Location:</strong>",filteredData3$Location)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredData4<-filteredData()%>%filter(HQ==3)), lat = ~Latitude, lng = ~Longitude, icon = HQicon, group = "Mission HQ", 
                        label=paste("<strong>Mission HQ:</strong>", filteredData4$Mission,"<br/><strong>Location:</strong>",filteredData4$Location)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredData5<-filteredData()%>%filter(Med==1)), lat = ~Latitude, lng = ~Longitude, icon = Medicon, group = "Medical units",
                        label=paste("<strong>Mission:</strong>", filteredData5$Mission,"<br/><strong>Location:</strong>",filteredData5$Location)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredData6<-filteredData()%>%filter(Av>0)), lat = ~Latitude, lng = ~Longitude, icon = Avicon, group = "Aviation", 
                        label=paste("<strong>Mission:</strong>", filteredData6$Mission,"<br/><strong>Location:</strong>",filteredData6$Location)%>% lapply(htmltools::HTML))
  })
  
  
  ####Second observe for TCC map   
  observe({
    leafletProxy(mapId = "map", data = filteredDataTCC()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(data = filteredDataTCC(), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1.5), 
                       fillOpacity = 0.8, color = ~qpal2(No.TCC), group = "TCC", labelOptions = labelOptions(style= list(
                         "width"= "150px", "white-space"="normal")),
                       label = paste("<strong>", filteredDataTCC()$Mission,"</strong><br/><strong>Location:</strong>",filteredDataTCC()$Location, "<br/><strong>Total number of TCCs:</strong>",filteredDataTCC()$No.TCC,"<br/><strong>Details:</strong>",filteredDataTCC()$year.overview)%>% lapply(htmltools::HTML))
    })
  
  
  ####Third observe for Troop Type map   
  observe({
    leafletProxy(mapId = "TroopTypeMap", data = filteredDataTroopType()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(data = (filteredDataTroopType0<-filteredDataTroopType()%>%filter(Infantry>0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(Infantry)^(1/3.5), 
                       fillOpacity = 0.8, color = ~qpal3(Infantry), group = "Infantry", 
                       label = paste("<strong>Location:</strong>",filteredDataTroopType()$Location)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredDataTroopType1<-filteredDataTroopType()%>%filter(Med>0)), lat = ~Latitude, lng = ~Longitude, icon = Medicon, group = "Medical",
                        label=paste("<strong>Medical Unit</strong><br/>", filteredDataTroopType1$Mission," (",filteredDataTroopType1$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addMarkers(data = (filteredDataTroopType2<-filteredDataTroopType()%>%filter(Eng>0)), lat = ~Latitude, lng = ~Longitude, group = "Engineer",
                 label=paste("<strong>Engineer Unit</strong><br/>", filteredDataTroopType2$Mission," (",filteredDataTroopType2$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addMarkers(data = (filteredDataTroopType3<-filteredDataTroopType()%>%filter(Avia>0)), lat = ~Latitude, lng = ~Longitude, group = "Aviation",
                 label=paste("<strong>Aviation Unit</strong><br/>", filteredDataTroopType3$Mission," (",filteredDataTroopType3$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addMarkers(data = (filteredDataTroopType4<-filteredDataTroopType()%>%filter(Sig>0)), lat = ~Latitude, lng = ~Longitude, group = "Signals",
                 label=paste("<strong>Signal Unit</strong><br/>", filteredDataTroopType4$Mission," (",filteredDataTroopType4$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addMarkers(data = (filteredDataTroopType5<-filteredDataTroopType()%>%filter(Riv>0)), lat = ~Latitude, lng = ~Longitude, group = "Riverine",
                 label=paste("<strong>Riverine Unit</strong><br/>", filteredDataTroopType5$Mission,"(",filteredDataTroopType5$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addMarkers(data = (filteredDataTroopType6<-filteredDataTroopType()%>%filter(Maint>0)), lat = ~Latitude, lng = ~Longitude, group = "Maintenance",
                 label=paste("<strong>Maintenance Unit</strong><br/>", filteredDataTroopType6$Mission," (",filteredDataTroopType6$Location,")<br/>")%>% lapply(htmltools::HTML))%>%
      addMarkers(data = (filteredDataTroopType7<-filteredDataTroopType()%>%filter(Trans>0)), lat = ~Latitude, lng = ~Longitude, group = "Transport",
                 label=paste("<strong>Transport Unit</strong><br/>", filteredDataTroopType7$Mission," (",filteredDataTroopType7$Location,")<br/>")%>% lapply(htmltools::HTML))
  })
  
  
}
###Launch the app
shinyApp(ui, server)
