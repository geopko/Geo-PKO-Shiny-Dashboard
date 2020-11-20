
#load required packages

if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
#if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("maps", repos = "http://cran.us.r-project.org")
library(ggplot2)
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
#if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
#if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(GADMTools)) install.packages("GADMTools")
if(!require(Cairo)) install.packages("Cairo")
if(!require(ggrepel)) install.packages("ggrepel")
#if(!require(forcats)) install.packages("forcats")
if(!require(sp)) install.packages("sp")
if(!require(gifski)) install.packages("gifski")
if(!require(png)) install.packages("png")
if(!require(gganimate)) install.packages("gganimate")
if(!require(scales)) install.packages("scales")
if(!require(ggnewscale)) install.packages("ggnewscale")
if(!require(shinycssloaders)) install.packages("shinycssloaders")
if(!require(purrr)) install.packages("purrr")
if(!require(foreign)) install.packages("foreign", repos = "https://svn.r-project.org/")
#if(!require(countrycode)) install.packages("countrycode")

options(shiny.usecairo=TRUE)

####import data####

geopko <- readr::read_csv("Geo_PKO_v2_ISO3.csv", col_types = cols(.default="c"),
                          locale=readr::locale(encoding="latin1"))

# geopko <- readxl::read_xlsx("Geo_PKO_v.2.0.xlsx", col_types="text")


####data prep TCC####

map_df <- geopko %>%
  mutate_at(vars(c(No.troops, No.TCC, longitude, latitude,
                   UNMO.dummy, UNPOL.dummy)), as.numeric) %>%
  mutate(HQ=as.factor(HQ))

tcc_df <- geopko %>% 
  mutate_at(vars(c(No.troops, No.TCC, longitude, latitude,
                   UNMO.dummy, UNPOL.dummy)), as.numeric) %>% 
  mutate(HQ=as.factor(HQ)) %>% 
  select(Source, Mission, year, month, MonthName,
         No.troops, nameoftcc_1:notroopspertcc_17) %>% 
  group_by(Source, Mission, year, month, MonthName) %>% 
  mutate(Total.troops=sum(No.troops, na.rm=T)) %>% ungroup()


#colnames(tcc_df) <- sub("name.of.TCC", "nameofTCC_", colnames(tcc_df)) obsolete script
#colnames(tcc_df) <- sub("No.troops.per.TCC", "notroopsperTCC_", colnames(tcc_df))
#tcc_df <- tcc_df %>% mutate_at(vars(starts_with("notroopsperTCC")), as.character) %>% 
#  mutate_at(vars(starts_with("nameofTCC")), as.character) %>% 

# ####map data prep####
# 
# #appending ISO code to the dataset
# ISO_df <- codelist %>% select(cown, iso3c) %>% mutate_all(as.character)
# 
# map_df <- left_join(geopko, ISO_df, by=c("cow_code"="cown"))
# 
# map_df <- map_df %>% 
#   mutate(iso3c = case_when(country == "Ethiopia/Eritrea" ~ "ETH, ERI",
#                            country == "Kosovo" ~ "XKO", 
#                            TRUE ~ as.character(iso3c)),
#          Month = as.numeric(Month),
#          MonthName = as.character(month(Month, label = TRUE, abbr = FALSE))) %>% 
#   unite(joined_date, c("Year","MonthName"), sep=": ", remove=FALSE) %>% 
#   unite(timepoint, c("Year","MonthName"), sep=" ", remove=FALSE) 

cclist3 <- map_df %>% select(Mission, iso3c) %>% distinct() %>% #creating list of country codes for GADM sf files dowload 
  mutate(iso3c=strsplit(as.character(iso3c), ", ")) %>% 
  unnest(iso3c) %>% distinct()
#names(map_df)[names(map_df) == "Inf"] <- "Infantry"

#oxford comma paste
country_list <-function(w, oxford=T) {
  if(length(w)==1) return(paste("This mission was active in the following country or territory:",w));
  if(length(w)==2) return(paste("This mission was active in the following countries or territories:", w[1],"and",w[2]));
  paste0("This mission was active in the following countries or territories: ",paste(w[-length(w)], collapse=", "), 
         ifelse(oxford,",","")," and ", w[length(w)] )
}

####lollipop data prep####

Years <- geopko
Years <- Years %>% group_by(Mission, location)%>% summarize(start_date=min(year), end_date=max(year))

####facet map data prep (placeholder)####



####UI####

ui <- fluidPage(
  navbarPage("Exploring Geo-PKO",
             navbarMenu("The Map Generator",
                        tabPanel("Deployment Maps (Static)", fluid=TRUE,
                                 titlePanel("Deployment Maps"),
                                 sidebarLayout(
                                   sidebarPanel(width=3, 
                                                p("Where are UN peacekeepers posted, and how many? Select the options below to visualise."),
                                                selectInput(inputId="mission_map", label="Select a mission",
                                                            choices=forcats::fct_relevel(factor(map_df$Mission)), width=150, selected=NULL),
                                                selectInput("timepoint_map", 
                                                            "Choose year and month", choices=NULL),
                                                # checkboxInput(inputId="depsize_map", 
                                                #               "Deployment size", value=TRUE),
                                                checkboxInput(inputId="MHQ_map", 
                                                              "Mission HQ", value=FALSE),
                                                checkboxInput(inputId="SHQ_map", 
                                                              "Sector HQ", value=FALSE),
                                                checkboxInput(inputId="MO_map", 
                                                              "UNMO", value=FALSE),
                                                checkboxInput(inputId="UNPOL_map", 
                                                              "UNPOL", value=FALSE),
                                                #actionButton("dostaticmap", "Generate map"),
                                                helpText("Errors may occur when a selected feature is not available for a map. If that happens, please deselect the option.")
                                   ),
                                   mainPanel(
                                     withSpinner(plotOutput("depmap", height="auto")),
                                     span(h6(textOutput("basecountries"), align="center")),
                                     hr(),
                                     fluidRow(
                                       DT::dataTableOutput(outputId="map_df_details")
                                     )
                                     
                                   )
                                 )
                        ),
                        ####animated maps UI####
                        tabPanel("Animated Maps", fluid=TRUE,
                                 titlePanel("Animated Maps"),
                                 sidebarLayout(
                                   sidebarPanel(width=3, 
                                                p(""),
                                                selectInput(inputId="anim_map", label="Animated maps show changes over time. Select a mission to visualise.",
                                                            choices=forcats::fct_relevel(factor(geopko$Mission)), width=200),
                                                tags$div("This tool relies on the package",tags$em("gganimate."),"The animation may take time, as rendering entails producing and combining multiple frames.")
                                   ),
                                   # sliderInput(inputId="anim_timeslider", 
                                   #             label= "Select deployment period", 
                                   #             value= c(x,y), 
                                   #             min= x,
                                   #             max= y
                                   #         )
                                   
                                   mainPanel(fluid=TRUE, 
                                             withSpinner(imageOutput("animated")))))),
             ####TCC Table UI####
             tabPanel("Contributing Countries",
                      titlePanel("Troop-contributing Countries"),
                      sidebarLayout(
                        sidebarPanel(radioButtons(inputId="databy_tcc", 
                                                  label="Present data by:", 
                                                  choices=c("Deployment map", "Year"),
                                                  selected="Deployment map (default)"),
                                     helpText("The Geo-PKO dataset collects data by deployment maps published by the UN. For the best accuracy, display data by deployment maps. Data by year present the year's average troop counts and the highest number of troop-contributing countries (TCCs).")
                        ),
                        # span(h6(textOutput("tabletext", align="right"))),
                        mainPanel(fluid=TRUE,
                                  DT::dataTableOutput("tcc_table")
                        ))),
             tabPanel("Missions",
                      sidebarLayout(
                        sidebarPanel(
                          p("This graph shows the specific time period during which a location had at least one active deployment."),
                          selectInput(inputId="Lollipop_map", label="Select a mission",
                                      choices=forcats::fct_relevel(factor(Years$Mission)), width=150), width= 3
                        ),
                        mainPanel(fluid=TRUE,
                                  plotOutput("lollipop", height="auto")
                        )
                      )),
             tabPanel ("Data",tags$div(
               includeHTML("using-the-dataset-3.html")
             )),
             tabPanel ("About",tags$div(
               includeHTML("about2.html")))
  )
)



server <- function(input, output, session){
  
  mission_list <- map_df %>% distinct(Mission) %>% arrange(Mission)
  
  ####TCC tables####
  
  bymap_df <- reactive({
    tcc_df %>% 
      pivot_longer(c(nameoftcc_1:notroopspertcc_17), names_to=c(".value", "tcc_id"), names_sep="_") %>%
      filter(!is.na(nameoftcc)) %>%
      mutate_at(vars(notroopspertcc), as.numeric) %>% 
      select(-tcc_id) %>% 
      group_by(Source, Mission, year, MonthName, Total.troops, nameoftcc)%>%
      summarise(total.tcc=as.character(sum(notroopspertcc), na.rm=TRUE)) %>% 
      add_count(Source, name="No.TCC") %>%
      mutate(total.tcc=ifelse(is.na(total.tcc), "size unknown", total.tcc), 
             overview=paste0(nameoftcc," (",total.tcc,")")) %>%
      group_by(Source, Mission, year, MonthName, Total.troops, No.TCC) %>%
      summarise(details=str_c(overview, collapse=", ")) %>% 
      arrange(desc(year))
  }) 
  
  byyear_df <- reactive({
    tcc_df %>% 
      pivot_longer(c(nameoftcc_1:notroopspertcc_17), names_to=c(".value", "TCC_id"), names_sep="_")%>%
      filter(!is.na(nameoftcc)) %>%
      mutate_at(vars(notroopspertcc), as.numeric) %>% 
      select(-TCC_id) %>% 
      group_by(Source, Mission, year, month, Total.troops, nameoftcc)%>%
      summarise(total.each.tcc=as.character(sum(notroopspertcc, na.rm=TRUE))) %>% 
      add_count(Source, name="No.TCC") %>%
      mutate(total.each.tcc=ifelse(total.each.tcc=="0","size unknown", total.each.tcc),
             overview=paste0(nameoftcc," (",total.each.tcc,")")) %>%
      select(-nameoftcc, -total.each.tcc) %>%
      group_by(Source, Mission, year, month, Total.troops, No.TCC) %>%
      summarise(byyear.overview=str_c(overview, collapse=", ")) %>% 
      arrange(desc(year)) %>%
      group_by(Mission, year) %>% mutate(min.troops= as.character(min(Total.troops)),
                                         max.troops=as.character(max(Total.troops)),
                                         ave.troops=as.character(round(mean(Total.troops)))) %>%
      group_by(Mission, year) %>% arrange(desc(No.TCC)) %>% dplyr::slice(1) %>% 
      mutate(ave.troops=ifelse(is.na(ave.troops), "Unknown", ave.troops),
             min.troops=ifelse(is.na(min.troops), "Unknown", min.troops),
             max.troops=ifelse(is.na(max.troops), "Unknown", max.troops)) %>%
      select(Mission, year, No.TCC, byyear.overview, min.troops, max.troops, ave.troops)
  })
  
  
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
  }, height=450)
  
  ####deployment maps####
  
  #creating list of sf objects to download
  sfdf <- reactive({
    req(input$mission_map)
    cclist3 %>% filter(Mission %in% input$mission_map)
  })
  
  observeEvent(input$mission_map,{
    updateSelectInput(session, 'timepoint_map',
                      choices = unique(map_df$joined_date[map_df$Mission==input$mission_map]))
    
  })
  
  map_df_temp <- reactive({
    req(input$mission_map)
    req(input$timepoint_map)
    
    map_df %>% filter(Mission %in% input$mission_map) %>%
      filter(joined_date %in% input$timepoint_map)
    
  })
  
  size_for_nrow1 <- reactive({
    req(map_df_temp())
    if(NROW(map_df_temp())==1){
      map_df_temp() %>% pull(No.troops) %>% `^`^(1/3)
    }
  })
  # map_zero <- reactive({
  #   map_df_temp() %>% filter(No.troops==0, No.TCC==0)
  # })
  
  output$basecountries <- renderText({
    unique_country <- unique(map_df_temp()$country)
    country_list(unique_country)
    
    # if(length(countrieslist)<=2){
    # paste0("This mission took place in: ",paste0(unique(map_df_temp()$country), collapse=" and "),".")}
    # else{
    #   paste0("This mission took place in ",paste0(unique(map_df_temp()$country), collapse=" and "),".")  
    # }
  })
  
  
  UNMO_df_temp <- reactive({
    req(input$MO_map)
    map_df_temp() %>% filter(UNMO.dummy==1)
  })
  
  UNPOL_df_temp <- reactive({
    req(input$UNPOL_map)
    map_df_temp() %>% filter(UNPOL.dummy==1)
  })
  
  SHQ_df_temp <- reactive({
    req(input$SHQ_map)
    
    map_df_temp() %>% filter(HQ=="2")
  })
  
  MHQ_df_temp <- reactive({
    req(input$MHQ_map)
    map_df_temp() %>% filter(HQ=="3")
  })
  
  maplist <- reactive({
    sfdf() %>%  pull(iso3c) 
  })
  
  # mapshapefiles <- reactive({
  #   gadm_sf_loadCountries(c(paste(maplist())), level=1)
  # })
  
  # max_no_tcc <- reactive({
  #   map_df_temp() %>% mutate(No.TCC=ifelse(is.na(No.TCC), 0, No.TCC))
  # })
  
  # g <- guide_legend("title")
  
  output$depmap <- renderPlot({
    
    #    input$depsize_map
    req(input$mission_map)
    req(input$timepoint_map)
    # input$MHQ_map
    # input$SHQ_map
    # input$MO_map
    # input$UNPOL_map
    
    mapshapefiles <- gadm_sf_loadCountries(c(maplist()), level=1)
    
    max_no_tcc <- map_df_temp() %>% mutate(No.TCC=ifelse(is.na(No.TCC), 0, No.TCC))
    
    p <- ggplot() + geom_sf(data=mapshapefiles$sf, fill="grey80") + 
      theme_void() + 
      labs(title=paste(map_df_temp()$Mission,": ", map_df_temp()$timepoint),
           caption="Sources: Geo-PKO v2.0\n Shapefiles from GADM.")+
      geom_blank()+
      geom_point(data=map_df_temp(), 
                 aes(x=longitude, y=latitude, shape="Blank", color="Blank"),
                 size=2, stroke=0.7, fill="grey44")+
      scale_shape_manual(values=c("Blank"=22),
                         labels=c("Blank"="Mission sites"),
                         name="")+
      scale_color_manual(values=c("Blank"="grey44"),
                         labels=c("Blank"="Mission sites"),
                         name="")+
      new_scale_color()+
      new_scale("shape")
    
    if (nrow(map_df_temp())>1){
      if(max(map_df_temp()$No.troops, na.rm=TRUE)>0){
        p <- p+
          geom_point(data=map_df_temp() %>% filter(!is.na(No.troops & No.TCC)),
                     aes(x=longitude, y=latitude, size=No.troops, color=as.integer(No.TCC)),
                     shape=20, alpha = 0.8)+
          scale_size_binned(name="Size of deployment",range=c(2, 16))+
          {if (max(max_no_tcc$No.TCC) <=4)
            scale_color_continuous(low = "thistle3", high = "darkred",
                                   guide="colorbar", name="No. of Troop-\nContributing Countries",
                                   breaks=c(0,1,2,3,4),
                                   limits=c(0,4))
          } +
          {if (max(max_no_tcc$No.TCC) > 4)
            scale_color_continuous(low = "thistle3", high = "darkred",
                                   guide="colorbar", name="No. of Troop-\nContributing Countries",
                                   breaks=pretty_breaks()
            )}
      }
      else{
        p <- p+
          geom_point(data=map_df_temp() %>% filter(!is.na(No.troops & No.TCC)),
                     aes(x=longitude, y=latitude, color=as.integer(No.TCC)),
                     shape=20, alpha = 0.8)+
          {if (max(max_no_tcc$No.TCC) <=4)
            scale_color_continuous(low = "thistle3", high = "darkred",
                                   guide="colorbar", name="No. of Troop-\nContributing Countries",
                                   breaks=c(0,1,2,3,4),
                                   limits=c(0,4))
          } +
          {if (max(max_no_tcc$No.TCC) > 4)
            scale_color_continuous(low = "thistle3", high = "darkred",
                                   guide="colorbar", name="No. of Troop-\nContributing Countries",
                                   breaks=pretty_breaks()
            )}
        
      }
    }
      if(nrow(map_df_temp()) ==1){
        p <- p+ geom_point(data=map_df_temp() %>% filter(!is.na(No.troops & No.TCC)),
                           aes(x=longitude, y=latitude, color=as.integer(No.TCC), size="Custom"),
                           shape=20, alpha = 0.8)+
          scale_size_manual(name="Size of deployment", values=c("Custom"=round((max(map_df_temp()$No.troops))^(1/3))),
                            labels=c("Custom"=paste(max(map_df_temp()$No.troops))))+
          {if (max(max_no_tcc$No.TCC) <=4)
            scale_color_continuous(low = "thistle3", high = "darkred",
                                   guide="colorbar", name="No. of Troop-\nContributing Countries",
                                   breaks=c(0,1,2,3,4),
                                   limits=c(0,4))
          } +
          {if (max(max_no_tcc$No.TCC) > 4)
            scale_color_continuous(low = "thistle3", high = "darkred",
                                   guide="colorbar", name="No. of Troop-\nContributing Countries",
                                   breaks=pretty_breaks()
            )}
      }
      
      p <- p +
        new_scale_color()+
        scale_shape_manual(values=c("SHQ"=3,
                                    "UNMO"=24,
                                    "UNPOL"=23),
                           labels=c("SHQ"="Sector HQ", "UNMO"="Military Observers", "UNPOL"="UN Police"),
                           name="")+
        scale_color_manual(values=c("SHQ"="orange",
                                    "UNMO"="darkblue",
                                    "UNPOL"="darkgreen"),
                           labels=c("SHQ"="Sector HQ", "UNMO"="Military Observers", "UNPOL"="UN Police"),
                           name="")
      
      
      # if(input$depsize_map){
      #   if(nrow(map_zero()) >0){
      #     p <- p + 
      #       geom_point(data=map_df_temp() %>% filter(No.troops>0 | No.TCC>0), 
      #                  aes(x=longitude, y=latitude, size=No.troops, color=as.integer(No.TCC)),
      #                  shape=20, alpha = 0.8)+
      #       scale_size_binned(name="Size of deployment",range=c(2, 16))+
      #       {if(max(map_df_temp()$No.TCC)<=4)list(
      #         scale_color_continuous(low = "thistle3", high = "darkred", 
      #                                guide="colorbar", name="Number of TCCs",
      #                                breaks=c(1,2,3,4),
      #                                limits=c(1,4)))}+
      #       {if(max(map_df_temp()$No.TCC)>4)list(
      #         scale_color_continuous(low = "thistle3", high = "darkred", 
      #                                guide="colorbar", name="Number of TCCs",
      #                                breaks=pretty_breaks()))}+
      #       new_scale_color()+
      #       
      #       geom_point(data=map_df_temp() %>% filter(No.troops==0, No.TCC==0), 
      #                  aes(x=longitude, y=latitude, shape="Blank", color="Blank"), 
      #                  size=2, stroke=0.5)+
      #       scale_shape_manual(values=c("Blank"=22),
      #                          labels=c("Blank"="Locations with no troops recorded"),
      #                          name="")+
      #       
      #       scale_color_manual(values=c("Blank"="grey44"),
      #                          labels=c("Blank"="Locations with no troops recorded"),
      #                          name="")+
      #       # guides(shape=guide_legend(title="", order=3), color=guide_legend(title="", order=3))+
      #       new_scale_color()+
      #       new_scale("shape")+
      #       scale_shape_manual(values=c("SHQ"=3,
      #                                   "UNMO"=24,
      #                                   "UNPOL"=23),
      #                          labels=c("SHQ"="Sector HQ", "UNMO"="Military Observers", "UNPOL"="UN Police"),
      #                          name="Non-combat functions")+
      #       scale_color_manual(values=c("SHQ"="orange",
      #                                   "UNMO"="darkblue",
      #                                   "UNPOL"="darkgreen"),
      #                          labels=c("SHQ"="Sector HQ", "UNMO"="Military Observers", "UNPOL"="UN Police"),
      #                          name="Non-combat functions")
      #   }
      #   else{
      #     p <- p + geom_point(data=map_df_temp(), 
      #                         aes(x=longitude, y=latitude, size=No.troops, color=as.integer(No.TCC)),
      #                         shape=20, alpha = 0.8)+
      #       scale_size_binned(name="Size of deployment",range=c(2, 16))+
      #       #  scale_color_brewer(palette="Set1", name="Number of TCCs")+
      #       {if(max(map_df_temp()$No.TCC)<=4)list(
      #         scale_color_continuous(low = "thistle3", high = "darkred", 
      #                                guide="colorbar", name="Number of TCCs",
      #                                breaks=c(1,2,3,4),
      #                                limits=c(1,4)))}+
      #       {if(max(map_df_temp()$No.TCC)>4)list(
      #         scale_color_continuous(low = "thistle3", high = "darkred", 
      #                                guide="colorbar", name="Number of TCCs",
      #                                breaks=pretty_breaks()))}+
      #       new_scale_color()+
      #       scale_shape_manual(values=c("SHQ"=3,
      #                                   "UNMO"=24,
      #                                   "UNPOL"=23),
      #                          labels=c("SHQ"="Sector HQ", 
      #                                   "UNMO"="Military Observers", 
      #                                   "UNPOL"="UN Police"),
      #                          name="Non-combat functions")+
      #       scale_color_manual(values=c("SHQ"="orange",
      #                                   "UNMO"="darkblue",
      #                                   "UNPOL"="darkgreen"),
      #                          labels=c("SHQ"="Sector HQ", 
      #                                   "UNMO"="Military Observers", 
      #                                   "UNPOL"="UN Police"),
      #                          name="Non-combat functions")
      #   }
      # # }
      
      if(input$MHQ_map){
        if(length(MHQ_df_temp()$location)>0){
          p <- p +  geom_point(data=MHQ_df_temp(),
                               aes(x=longitude, y=latitude, shape="HQ"),
                               shape=4, color="red", size=6)+
            geom_label_repel(data=MHQ_df_temp(),
                             aes(x=longitude, y=latitude, label=paste0("Mission HQ: ",location)
                             ),
                             box.padding = 2,
                             size = 3,
                             fill = alpha(c("white"),0.7))}
        else{
          p <- p + labs(subtitle="Mission HQs not available for this time period. Please deselect the option.")}
      }
      
      if(input$SHQ_map){
        if(length(SHQ_df_temp()$location)>0){
          p <- p +  geom_point(data=map_df_temp() %>% filter(HQ=="2"),
                               aes(x=longitude, y= latitude, shape="SHQ", color="SHQ"), size=5)}
        else{
          p <- p + labs(subtitle="Sector HQs not available for this time period. Please deselect the option.")}
      }
      
      if(input$MO_map){
        if(length(UNMO_df_temp()$location)>0){
          p <- p +  geom_point(data=map_df_temp() %>% filter(UNMO.dummy==1),
                               aes(x=longitude, y= latitude, shape="UNMO", color="UNMO"),
                               #color="darkblue",
                               position=position_jitter(),
                               size=3)}
        else{
          p <- p + labs(subtitle="UNMO not found. Please deselect the option.")}
      }
      
      if(input$UNPOL_map){
        if(length(UNPOL_df_temp()$location)>0){
          p <- p +  geom_point(data=map_df_temp() %>% filter(UNPOL.dummy==1),
                               aes(x=longitude, y= latitude, shape="UNPOL", color="UNPOL"),
                               position=position_jitter(),
                               size=4)}
        else{
          p <- p + labs(subtitle="UNPOL not found. Please deselect the option.")}
      }
      
      p <- p +
        theme(plot.subtitle = element_text(color="red"),
              plot.title=element_text(face="bold", hjust=0),
              #      plot.caption.position = "plot",
              plot.caption = element_text(hjust=1),
              legend.direction = "horizontal",
              legend.position = "bottom",
              legend.box = "vertical")
      
      print(p)
      
    }, height=600) 
  
  ####map_df_detail####
  
  typecheck_df <- reactive({
    req(input$mission_map)
    req(input$timepoint_map)
    
    map_df_temp() %>% tibble::rowid_to_column("ID") %>% 
      select(ID, location, No.troops, No.TCC, RPF:UAV, Other.Type, -RPF_No,
             -Inf_No, -FPU_No, -RES_No, -FP_No) %>%
      mutate_at(vars(`Inf`:`Other.Type`), as.numeric) %>% 
      rowwise(ID) %>% 
      mutate(typecheck_var=sum(c_across(`Inf`:`Other.Type`))) %>% 
      filter(typecheck_var >0)
  })
  
  static_map_details <- reactive({
    if(length(typecheck_df()>0)){
      map_df_temp() %>% tibble::rowid_to_column("ID") %>% 
        select(ID, location, No.troops, No.TCC, RPF:UAV, Other.Type, -RPF_No,
               -Inf_No, -FPU_No, -RES_No, -FP_No) %>% 
        mutate(across(everything(), as.character)) %>% 
        pivot_longer(5:23, names_to="trooptypes", values_to="binary") %>% 
        filter(binary==1) %>% 
        mutate(trooptypes=case_when(trooptypes == "Other.Type" ~ "Others",
                                    trooptypes == "SF" ~ "Special Forces", 
                                    trooptypes == "Inf" ~ "Infantry",
                                    TRUE ~ as.character(trooptypes))) %>% 
        group_by(ID, location, No.troops, No.TCC) %>% 
        summarize(Troop.Compo = str_c(trooptypes, collapse=", ")) %>% ungroup() %>% 
        mutate(No.TCC=ifelse(is.na(No.TCC), "Unknown", No.TCC)) %>% 
        select(-ID)}
    else {
      map_df_temp() %>% 
        select(location, No.troops, No.TCC) %>% 
        mutate(Troop.type="Data on troop types not available for this location") %>% 
        mutate(No.TCC=ifelse(is.na(No.TCC), "Unknown", No.TCC))
    }
  })
  
  output$map_df_details <- renderDataTable({
    DT::datatable(static_map_details(), 
                  colnames = c("Location", "No. Troops", "No. TCCs", "Troop Types"),
                  rownames = FALSE)
    
  })
  
  ####animated maps####
  anim_sf <- reactive({
    req(input$anim_map)
    cclist3 %>% filter(Mission %in% input$anim_map)
  })
  
  anim_df <- reactive({
    req(input$anim_map)
    map_df %>% filter(Mission %in% input$anim_map) %>% arrange(timepoint)
  }) 
  
  
  
  output$animated <- renderImage({
    req(input$anim_map)
    
    outfile <- tempfile(fileext= '.gif')
    
    anim_maplist <- pull(anim_sf(), iso3c)
    anim_max_no_tcc <- anim_df() %>% mutate(No.TCC=ifelse(is.na(No.TCC), 0, No.TCC))
    anim_mapshapefiles <- gadm_sf_loadCountries(c(anim_maplist), level=1)
    mission_name <- anim_df() %>% distinct(Mission)
    colourCount = max(anim_df()$No.TCC)
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))
    
    anim_p <- ggplot() + geom_sf(data=anim_mapshapefiles$sf) + 
      theme_void() + 
      geom_point(data=anim_df(), aes(x=longitude, y=latitude, size=No.troops, 
                                     color=as.integer(No.TCC), group=timepoint),
                 shape=20, alpha = 0.5)+
      scale_size_binned(name="Size of deployment",range=c(2, 16))+
      {if(max(anim_max_no_tcc$No.TCC)<=4)list(
        scale_color_continuous(low = "thistle3", high = "darkred",
                               guide="colorbar", name="No. of Troop-\nContributing Countries",
                               breaks=c(1,2,3,4),
                               limits=c(1,4)))
      } +
      {if(max(anim_max_no_tcc$No.TCC)>4)list(
        scale_color_continuous(low = "thistle3", high = "darkred",
                               guide="colorbar", name="No. of Troop-\nContributing Countries",
                               breaks=pretty_breaks())
      )}+
      theme(plot.subtitle = element_text(color="red"),
            plot.title=element_text(face="bold", hjust=0),
            #      plot.caption.position = "plot",
            plot.caption = element_text(hjust=1),
            legend.direction = "vertical",
            legend.box="horizontal",
            legend.position = "right")+
      transition_states(states=anim_df()$timepoint)+
      labs(title=paste0(mission_name,": ", "{closest_state}"),
           caption="Sources: Geo-PKO v2.0\n Shapefiles from GADM.")+
      ease_aes('linear')
    
    anim_save("outfile.gif", animate(anim_p, fps = 4, width=650, height=500, res=120))
    
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
                       y=fct_reorder(location, start_date), 
                       yend=fct_reorder(location, start_date)), color="grey") +
      geom_point(aes(x=end_date, y=location), 
                 colour=rgb(0.9,0.3,0.1,0.9), size=3.5 ) +
      geom_point(aes(x=start_date, y=location), 
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
  