#if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
#if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
#if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
#if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
library(leaflet)
library(shiny)
library(dplyr)
library(tidyr)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)

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
  markerColor = "blue",
  iconColor = "#f7fcff",
  library = 'fa'
)
#Shiny leaflet launch
GeoPKO <- Newdataset_2_
class(GeoPKO$No.troops)
GeoPKO$NoTroops<-as.numeric(GeoPKO$No.troops)
GeoPKO$RPF_No<-as.numeric(GeoPKO$RPF_No)
GeoPKO$UNPOL<-as.numeric(GeoPKO$UNPOL.dummy)
GeoPKO$UNMO<-as.numeric(GeoPKO$UNMO.dummy)
GeoPKO$No.TCC<-as.numeric(GeoPKO$No.TCC)


gif_df <- GeoPKO %>% select(Mission, Year, Country, Location, Latitude, Longitude, NoTroops, HQ, UNPOL, Med,UNMO, No.TCC, nameoftcc_1, nameoftcc_2, nameoftcc_3, nameoftcc_4, nameoftcc_5, nameoftcc_6, nameoftcc_7, nameoftcc_8, nameoftcc_9, nameoftcc_10, nameoftcc_11, nameoftcc_12, nameoftcc_13, nameoftcc_14,nameoftcc_15,nameoftcc_16,nameoftcc_17) %>%
  group_by(Mission, Year, Location, Country) %>%
  mutate(ave.no.troops = as.integer(mean(NoTroops, na.rm=TRUE))) %>% select(-NoTroops) %>% distinct() %>% drop_na(ave.no.troops)

gif_df$nameoftcc_2 <- str_replace_all(gif_df$nameoftcc_2, "NA", "")
gif_df$nameoftcc_3 <- str_replace_all(gif_df$nameoftcc_3, "NA", "")
gif_df$nameoftcc_4 <- str_replace_all(gif_df$nameoftcc_4, "NA", "")
gif_df$nameoftcc_5 <- str_replace_all(gif_df$nameoftcc_5, "NA", "")
gif_df$nameoftcc_6 <- str_replace_all(gif_df$nameoftcc_6, "NA", "")
gif_df$nameoftcc_7 <- str_replace_all(gif_df$nameoftcc_7, "NA", "")
gif_df$nameoftcc_8 <- str_replace_all(gif_df$nameoftcc_8, "NA", "")
gif_df$nameoftcc_9 <- str_replace_all(gif_df$nameoftcc_9, "NA", "")
gif_df$nameoftcc_10 <- str_replace_all(gif_df$nameoftcc_10, "NA", "")
gif_df$nameoftcc_11 <- str_replace_all(gif_df$nameoftcc_11, "NA", "")
gif_df$nameoftcc_12 <- str_replace_all(gif_df$nameoftcc_12, "NA", "")
gif_df$nameoftcc_13 <- str_replace_all(gif_df$nameoftcc_13, "NA", "")
gif_df$nameoftcc_14 <- str_replace_all(gif_df$nameoftcc_14, "NA", "")
gif_df$nameoftcc_15 <- str_replace_all(gif_df$nameoftcc_15, "NA", "")
gif_df$nameoftcc_16 <- str_replace_all(gif_df$nameoftcc_16, "NA", "")
gif_df$nameoftcc_17 <- str_replace_all(gif_df$nameoftcc_17, "NA", "")

qpal <- colorNumeric("RdYlBu", gif_df$ave.no.troops)

####UI###
ui <- fluidPage(
  navbarPage ("Exploring GeoPKO",
              tabPanel ("Mapper", leafletOutput("basemap", height=850),
                        absolutePanel(top = 70, left = 80,width = 300, style = "background:rgba(255, 224, 189, 0.5)",
                                      span(tags$i(h6("The Geo-PKO dataset provides data on UN peacekeeping deployments. It offers information on key attributes of peacekeeping deployments at the local level, including location, size, troop type, headquarters, troop-contributing countries and other variables. This visualization is based on GeoPKO 2.0.")), style="color:#045a8d"),
                                      span(h5(textOutput("reactive_year"), align = "Left"), style="color:#15110d"),
                                      span(h4(textOutput("reactive_troopcount"), align = "center"), style="color:#15110d"),
                                      span(h6(textOutput("reactive_UNPOLcount"), align = "right"), style="color:#527bd2"),
                                      span(h6(textOutput("reactive_UNMOcount"), align = "right"), style="color:#363b74"),
                                      pickerInput("missions","Select Missions", choices=as.character(unique(gif_df$Mission)),selected =as.character(unique(gif_df$Mission)) , options = list(`actions-box` = TRUE),multiple = T),
                                      chooseSliderSkin("Shiny", color = "transparent"),
                                      setSliderColor("transparent", 1),
                                      sliderInput(inputId = "plot_date", 
                                                  label = "Select deployment year (1994-2020)",
                                                  min = 1994,
                                                  max = 2020,
                                                  value =1994,
                                                  step = 1,
                                                  sep= ""),
                                      span(tags$i(h6("Data used in the visualization is derived from UN deployment maps. Furthermore, the data is average by year")), style="color:#15110d")
                        )
              ),tabPanel ("About")
  ))


#################Server#####################
server <- function(input, output, session){
  
  filteredData <- reactive({
    gif_df %>% filter(Mission %in% input$missions & Year %in% input$plot_date)
  })
  
  
  output$basemap <- renderLeaflet({
    leaflet(GeoPKO, options = leafletOptions(minZoom = 2)) %>% 
      addTiles() %>% 
      addLayersControl(
        position = "bottomright",
        baseGroups = c("Troop deployment", "TCC", "None"),
        overlayGroups = c("Medical units", "UNPOL", "UNMO", "Mission HQ"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(c("Medical units","UNPOL", "UNMO", "Mission HQ"))  %>%
      fitBounds(~-70,-50,~60,60) %>%
      setMaxBounds(~-70,-50,~60,60)  %>%
      addLegend(pal = qpal, values = ~gif_df$ave.no.troops, group = "Troop deployment", title= "Legend")
  })
  
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
  
  
  observe({
    leafletProxy(mapId = "basemap", data = filteredData()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(data = (filteredData1<-filteredData()%>%filter(ave.no.troops>0)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(ave.no.troops)^(1/3.5), 
                       fillOpacity = 0.5, color = ~qpal(ave.no.troops), group = "Troop deployment", 
                       label=paste("<strong>Troop number:</strong>", filteredData1$ave.no.troops,"<br/><strong>Mission:</strong>", filteredData1$Mission,"<br/><strong>Location:</strong>",filteredData1$Location)%>% lapply(htmltools::HTML)) %>%
      addAwesomeMarkers(data = (filteredData2<-filteredData()%>%filter(UNPOL>0)), lat = ~Latitude, lng = ~Longitude,icon=UNPOLicon, group = "UNPOL", 
                        label=paste("<strong>UNPOL</strong> (",filteredData2$Mission,")<br/><strong>Location:</strong>",filteredData2$Location)%>% lapply(htmltools::HTML)) %>%
      addCircleMarkers(data = (filteredData3<-filteredData()%>%filter(No.TCC==1)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData3$Location, "<br/><strong>",filteredData3$No.TCC, " TCC:</strong><br/>", filteredData3$nameoftcc_1)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredData4<-filteredData()%>%filter(UNMO>0)), lat = ~Latitude, lng = ~Longitude, icon=UNMOicon, group = "UNMO", 
                        label=paste("<strong>UNMO <br/>Mission:</strong>", filteredData4$Mission,"<br/><strong>Location:</strong>",filteredData4$Location)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredData5<-filteredData()%>%filter(HQ==3)), lat = ~Latitude, lng = ~Longitude, icon = HQicon, group = "Mission HQ", 
                        label=paste("<strong>Mission HQ:</strong>", filteredData5$Mission,"<br/><strong>Location:</strong>",filteredData5$Location)%>% lapply(htmltools::HTML))%>%
      addAwesomeMarkers(data = (filteredData22<-filteredData()%>%filter(Med==1)), lat = ~Latitude, lng = ~Longitude, icon = Medicon, group = "Medical units", 
                        label=paste("<strong>Mission HQ:</strong>", filteredData22$Mission,"<br/><strong>Location:</strong>",filteredData22$Location)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData6<-filteredData()%>%filter(No.TCC==2)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData6$Location, "<br/><strong>",filteredData6$No.TCC, " TCCs:</strong><br/>", filteredData6$nameoftcc_1,"<br/>",filteredData6$nameoftcc_2)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData7<-filteredData()%>%filter(No.TCC==3)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData7$Location, "<br/><strong>",filteredData7$No.TCC, " TCCs:</strong><br/>", filteredData7$nameoftcc_1,"<br/>",filteredData7$nameoftcc_2,"<br/>",filteredData7$nameoftcc_3)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData8<-filteredData()%>%filter(No.TCC==4)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData8$Location, "<br/><strong>",filteredData8$No.TCC, " TCCs:</strong><br/>", filteredData8$nameoftcc_1,"<br/>",filteredData8$nameoftcc_2,"<br/>",filteredData8$nameoftcc_3,"<br/>",filteredData8$nameoftcc_4)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData9<-filteredData()%>%filter(No.TCC==5)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData9$Location, "<br/><strong>",filteredData9$No.TCC, " TCCs:</strong><br/>", filteredData9$nameoftcc_1,"<br/>",filteredData9$nameoftcc_2,"<br/>",filteredData9$nameoftcc_3,"<br/>",filteredData9$nameoftcc_4,"<br/>",filteredData9$nameoftcc_5)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData10<-filteredData()%>%filter(No.TCC==6)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData10$Location, "<br/><strong>",filteredData10$No.TCC, " TCCs:</strong><br/>", filteredData10$nameoftcc_1,"<br/>",filteredData10$nameoftcc_2,"<br/>",filteredData10$nameoftcc_3,"<br/>",filteredData10$nameoftcc_4,"<br/>",filteredData10$nameoftcc_5,"<br/>",filteredData10$nameoftcc_6)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData11<-filteredData()%>%filter(No.TCC==7)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData11$Location, "<br/><strong>",filteredData11$No.TCC, " TCCs:</strong><br/>", filteredData11$nameoftcc_1,"<br/>",filteredData11$nameoftcc_2,"<br/>",filteredData11$nameoftcc_3,"<br/>",filteredData11$nameoftcc_4,"<br/>",filteredData11$nameoftcc_5,"<br/>",filteredData11$nameoftcc_6,"<br/>",filteredData11$nameoftcc_7)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData12<-filteredData()%>%filter(No.TCC==8)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData12$Location, "<br/><strong>",filteredData12$No.TCC, " TCCs:</strong><br/>", filteredData12$nameoftcc_1,"<br/>",filteredData12$nameoftcc_2,"<br/>",filteredData12$nameoftcc_3,"<br/>",filteredData12$nameoftcc_4,"<br/>",filteredData12$nameoftcc_5,"<br/>",filteredData12$nameoftcc_6,"<br/>",filteredData12$nameoftcc_7,"<br/>",filteredData12$nameoftcc_8)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData13<-filteredData()%>%filter(No.TCC==9)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData13$Location, "<br/><strong>",filteredData13$No.TCC, " TCCs:</strong><br/>", filteredData13$nameoftcc_1,"<br/>",filteredData13$nameoftcc_2,"<br/>",filteredData13$nameoftcc_3,"<br/>",filteredData13$nameoftcc_4,"<br/>",filteredData13$nameoftcc_5,"<br/>",filteredData13$nameoftcc_6,"<br/>",filteredData13$nameoftcc_7,"<br/>",filteredData13$nameoftcc_8,"<br/>",filteredData13$nameoftcc_9)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData14<-filteredData()%>%filter(No.TCC==10)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData14$Location, "<br/><strong>",filteredData14$No.TCC, " TCCs:</strong><br/>", filteredData14$nameoftcc_1,"<br/>",filteredData14$nameoftcc_2,"<br/>",filteredData14$nameoftcc_3,"<br/>",filteredData14$nameoftcc_4,"<br/>",filteredData14$nameoftcc_5,"<br/>",filteredData14$nameoftcc_6,"<br/>",filteredData14$nameoftcc_7,"<br/>",filteredData14$nameoftcc_8,"<br/>",filteredData14$nameoftcc_9,"<br/>",filteredData14$nameoftcc_10)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData15<-filteredData()%>%filter(No.TCC==11)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData15$Location, "<br/><strong>",filteredData15$No.TCC, " TCCs:</strong><br/>", filteredData15$nameoftcc_1,"<br/>",filteredData15$nameoftcc_2,"<br/>",filteredData15$nameoftcc_3,"<br/>",filteredData15$nameoftcc_4,"<br/>",filteredData15$nameoftcc_5,"<br/>",filteredData15$nameoftcc_6,"<br/>",filteredData15$nameoftcc_7,"<br/>",filteredData15$nameoftcc_8,"<br/>",filteredData15$nameoftcc_9,"<br/>",filteredData15$nameoftcc_10,"<br/>",filteredData15$nameoftcc_11)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData16<-filteredData()%>%filter(No.TCC==12)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData16$Location, "<br/><strong>",filteredData16$No.TCC, " TCCs:</strong><br/>", filteredData16$nameoftcc_1,"<br/>",filteredData16$nameoftcc_2,"<br/>",filteredData16$nameoftcc_3,"<br/>",filteredData16$nameoftcc_4,"<br/>",filteredData16$nameoftcc_5,"<br/>",filteredData16$nameoftcc_6,"<br/>",filteredData16$nameoftcc_7,"<br/>",filteredData16$nameoftcc_8,"<br/>",filteredData16$nameoftcc_9,"<br/>",filteredData16$nameoftcc_10,"<br/>",filteredData16$nameoftcc_11,"<br/>",filteredData16$nameoftcc_12)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData17<-filteredData()%>%filter(No.TCC==13)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData17$Location, "<br/><strong>",filteredData17$No.TCC, " TCCs:</strong><br/>", filteredData17$nameoftcc_1,"<br/>",filteredData17$nameoftcc_2,"<br/>",filteredData17$nameoftcc_3,"<br/>",filteredData17$nameoftcc_4,"<br/>",filteredData17$nameoftcc_5,"<br/>",filteredData17$nameoftcc_6,"<br/>",filteredData17$nameoftcc_7,"<br/>",filteredData17$nameoftcc_8,"<br/>",filteredData17$nameoftcc_9,"<br/>",filteredData17$nameoftcc_10,"<br/>",filteredData17$nameoftcc_11,"<br/>",filteredData17$nameoftcc_12,"<br/>",filteredData17$nameoftcc_13)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData18<-filteredData()%>%filter(No.TCC==14)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData18$Location, "<br/><strong>",filteredData18$No.TCC, " TCCs:</strong><br/>", filteredData18$nameoftcc_1,"<br/>",filteredData18$nameoftcc_2,"<br/>",filteredData18$nameoftcc_3,"<br/>",filteredData18$nameoftcc_4,"<br/>",filteredData18$nameoftcc_5,"<br/>",filteredData18$nameoftcc_6,"<br/>",filteredData18$nameoftcc_7,"<br/>",filteredData18$nameoftcc_8,"<br/>",filteredData18$nameoftcc_9,"<br/>",filteredData18$nameoftcc_10,"<br/>",filteredData18$nameoftcc_11,"<br/>",filteredData18$nameoftcc_12,"<br/>",filteredData18$nameoftcc_13,"<br/>",filteredData18$nameoftcc_14)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData19<-filteredData()%>%filter(No.TCC==15)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData19$Location, "<br/><strong>",filteredData19$No.TCC, " TCCs:</strong><br/>", filteredData19$nameoftcc_1,"<br/>",filteredData19$nameoftcc_2,"<br/>",filteredData19$nameoftcc_3, "<br/>",filteredData19$nameoftcc_4,"<br/>",filteredData19$nameoftcc_5,"<br/>",filteredData19$nameoftcc_6,"<br/>",filteredData19$nameoftcc_7,"<br/>",filteredData19$nameoftcc_8,"<br/>",filteredData19$nameoftcc_9,"<br/>",filteredData19$nameoftcc_10,"<br/>",filteredData19$nameoftcc_11,"<br/>",filteredData19$nameoftcc_12,"<br/>",filteredData19$nameoftcc_13,"<br/>",filteredData19$nameoftcc_14, "<br/>",filteredData19$nameoftcc_15)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData20<-filteredData()%>%filter(No.TCC==16)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData20$Location, "<br/><strong>",filteredData20$No.TCC, " TCCs:</strong><br/>", filteredData20$nameoftcc_1,"<br/>",filteredData20$nameoftcc_2,"<br/>",filteredData20$nameoftcc_3, "<br/>",filteredData20$nameoftcc_4,"<br/>",filteredData20$nameoftcc_5,"<br/>",filteredData20$nameoftcc_6,"<br/>",filteredData20$nameoftcc_7,"<br/>",filteredData20$nameoftcc_8,"<br/>",filteredData20$nameoftcc_9,"<br/>",filteredData20$nameoftcc_10,"<br/>",filteredData20$nameoftcc_11,"<br/>",filteredData20$nameoftcc_12,"<br/>",filteredData20$nameoftcc_13,"<br/>",filteredData20$nameoftcc_14, "<br/>",filteredData20$nameoftcc_15,"<br/>",filteredData20$nameoftcc_16)%>% lapply(htmltools::HTML))%>%
      addCircleMarkers(data = (filteredData21<-filteredData()%>%filter(No.TCC==17)), lat = ~Latitude, lng = ~Longitude, weight = 1, radius = ~(No.TCC)*(1), 
                       fillOpacity = 0.8, color = "#b11226", group = "TCC", 
                       label=paste("<strong>Location:</strong>",filteredData21$Location, "<br/><strong>",filteredData21$No.TCC, " TCCs:</strong><br/>", filteredData21$nameoftcc_1,"<br/>",filteredData21$nameoftcc_2,"<br/>",filteredData21$nameoftcc_3, "<br/>",filteredData21$nameoftcc_4,"<br/>",filteredData21$nameoftcc_5,"<br/>",filteredData21$nameoftcc_6,"<br/>",filteredData21$nameoftcc_7,"<br/>",filteredData21$nameoftcc_8,"<br/>",filteredData21$nameoftcc_9,"<br/>",filteredData21$nameoftcc_10,"<br/>",filteredData21$nameoftcc_11,"<br/>",filteredData21$nameoftcc_12,"<br/>",filteredData21$nameoftcc_13,"<br/>",filteredData21$nameoftcc_14, "<br/>",filteredData21$nameoftcc_15,"<br/>",filteredData21$nameoftcc_16,"<br/>",filteredData21$nameoftcc_17)%>% lapply(htmltools::HTML))
  })
  
}

shinyApp(ui, server)
