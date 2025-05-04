
# load required packages

if (!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if (!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if (!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if (!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if (!require(readr)) install.packages("maps", repos = "http://cran.us.r-project.org")
if (!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if (!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if (!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
# if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if (!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if (!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if (!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if (!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if (!require(tidyr)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if (!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if (!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if (!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if (!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
#if (!require(GADMTools)) install.packages("GADMTools")
if (!require(rnaturalearth)) install.packages("rnaturalearth")
if (!require(rnaturalearthdata)) install.packages("rnaturalearthdata")
if (!require(Cairo)) install.packages("Cairo")
if (!require(ggrepel)) install.packages("ggrepel")
if (!require(forcats)) install.packages("forcats")
if (!require(gifski)) install.packages("gifski")
if (!require(png)) install.packages("png")
if (!require(gganimate)) install.packages("gganimate")
if (!require(scales)) install.packages("scales")
if (!require(ggnewscale)) install.packages("ggnewscale")
if (!require(shinycssloaders)) install.packages("shinycssloaders")
if (!require(purrr)) install.packages("purrr")
if (!require(foreign)) install.packages("foreign", repos = "https://svn.r-project.org/")
if (!require(shinyBS)) install.packages("shinyBS", repos = "https://cran.r-project.org/")
if (!require(knitr)) install.packages("knitr", repos = "https://cran.r-project.org/")
if (!require(htmltools)) install.packages("htmltools", repos = "https://cran.r-project.org/")
if (!require(markdown)) install.packages("markdown")
library(waiter)

options(shiny.usecairo = TRUE)

#### import data####

geopko <- readr::read_csv("data/Geo_PKO_v.2.3_ISO3.csv", col_types = cols(.default = "c"))
#geopko <- readr::read_csv("data/GEO_PKO_v.2.1_ISO3.csv", col_types = cols(.default = "c"))
#geopko2 <- readxl::read_xlsx("Geo_PKO_v.2.0.xlsx", col_types="text")

# Rmd pages

rmdfiles <- c("about.Rmd", "data.Rmd")
sapply(rmdfiles, knit, quiet = T)


#### leaflet data prep####
# Set symbols for Icons
HQicon <- awesomeIcons(
  icon = "fas fa-home",
  markerColor = "black",
  iconColor = "#f7fcff",
  library = "fa"
)

Medicon <- awesomeIcons(
  icon = "fas fa-plus",
  markerColor = "white",
  iconColor = "red",
  library = "fa"
)

UNPOLicon <- awesomeIcons(
  icon = "fab fa-product-hunt",
  markerColor = "blue",
  iconColor = "#f6f6f6",
  library = "fa"
)
UNMOicon <- awesomeIcons(
  icon = "fas fa-binoculars",
  markerColor = "darkblue",
  iconColor = "#f6f6f6",
  library = "fa"
)

Avicon <- makeAwesomeIcon(
  icon = "fas fa-plane",
  markerColor = "gray",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = "fa"
)

Rivicon <- makeAwesomeIcon(
  icon = "fas fa-anchor",
  markerColor = "cadetblue",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = "fa"
)

Engicon <- makeAwesomeIcon(
  icon = "fas fa-bolt",
  markerColor = "black",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = "fa"
)

Sigicon <- makeAwesomeIcon(
  icon = "fas fa-wifi",
  markerColor = "lightgray",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = "fa"
)

Traicon <- makeAwesomeIcon(
  icon = "fas fa-truck",
  markerColor = "darkpurple",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = "fa"
)

Mainticon <- makeAwesomeIcon(
  icon = "fas fa-wrench",
  markerColor = "darkgreen",
  iconColor = "#ffffff",
  squareMarker = FALSE,
  library = "fa"
)

# Data frame for first map

FrontmapData <- readxl::read_xlsx("data/FrontMap_v.2.3.xlsx", col_types = "text")
#FrontmapData <- readr::read_csv("data/FrontMap.csv", col_types = cols(.default = "c"))
FrontmapData <- FrontmapData %>%
  mutate_at(vars(latitude:ave.no.troops), as.numeric)

FrontmapData_abso <- FrontmapData # Make a duplicate for that other front map

#### Oxford comma mission text front page####
mission_comma <- function(w, oxford = T) {
  paste0(
    paste(w[-length(w)], collapse = ", "),
    ifelse(oxford, ",", ""), " and ", w[length(w)],"."
  )
}


### TCC dataframe (second map)
TCCmapData <- readxl::read_xlsx("data/TCCMap_v.2.3.xlsx", col_types = "text")
#TCCmapData <- readr::read_csv("data/TCCMap.csv", col_types = cols(.default = "c"))
TCCmapData %>% mutate_at(vars(longitude, latitude, No.TCC), as.numeric) -> TCCmapData

## Troop Type Dataframe (third map)
TTmapData <- readxl::read_xlsx("data/TTMap_v.2.3.xlsx", col_types = "text")
#TTmapData <- readr::read_csv("data/TTMap.csv", col_types = cols(.default = "c"))
TTmapData <- TTmapData %>%
  mutate_at(vars(latitude:trans), as.numeric)

### Legend colours
ColoursFrontmap <- colorBin(rev(viridis::viridis(10)), FrontmapData$ave.no.troops, bins = c(10, 50, 100, 500, 1000, 2000, 4000, 6000, 8000))
ColoursFrontmapReserve <- colorBin(rev(viridis::viridis(10)), FrontmapData$Reserve, bins = c(10, 50, 100, 500, 1000, 2000, 4000, 6000, 8000))
ColoursTCCmap <- colorBin((viridis::viridis(2)), TCCmapData$No.TCC, bins = c(1, 2, 4, 7, 10, 15, 20))
ColoursTTmap <- colorBin(rev(viridis::viridis(10)), TTmapData$Infantry, bins = c(10, 50, 100, 500, 1000, 2000, 4000, 8000))

## Set basemaps for leaflet tabs
# Map with panel above
basemapFront <- leaflet(geopko, options = leafletOptions(minZoom = 2)) %>%
  addTiles() %>%
  setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
  addLayersControl(
    position = "bottomright",
    baseGroups = c("Deployments (All)", "Troops (Infantry)", "Troops (Reserve)", "Mission Site (No Troops)", "None"),
    overlayGroups = c("UNPOL", "UNMO", "Mission HQs"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c("UNPOL", "UNMO", "Mission HQs")) %>%
  fitBounds(~ -70, -50, ~60, 60) %>%
  addLegend(pal = ColoursFrontmap, values = ~ FrontmapData$ave.no.troops, group = "Troop deployment", title = "Number of troops") %>%
  addCircleMarkers(
    data = (FrontmapDataTroop <- FrontmapData %>% filter(ave.no.troops > 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (ave.no.troops)^(1 / 3.5),
    fillOpacity = 0.6, color = ~ ColoursFrontmap(ave.no.troops), group = "Deployments (All)",
    label = paste("<strong>", FrontmapDataTroop$mission, "<br/>Location:</strong>", FrontmapDataTroop$location, "<br/><strong>Troop number:</strong>", FrontmapDataTroop$ave.no.troops) %>% lapply(htmltools::HTML)
  ) %>%
  addCircleMarkers(
    data = (FrontmapDataMissionSite <- FrontmapData %>% filter(ave.no.troops == 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, weight = 0.5, radius = 3,
    fillOpacity = 0.4, color = "#666666", group = "Deployments (All)",
    label = paste("<strong>", FrontmapDataMissionSite$mission, "<br/>Location:</strong>", FrontmapDataMissionSite$location, "<br/><strong>Mission site</strong> (no troop deployment)") %>% lapply(htmltools::HTML)
  ) %>%
  addCircleMarkers(
    data = (FrontmapDataInfantry <- FrontmapData %>% filter(Infantry > 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (ave.no.troops)^(1 / 3.5),
    fillOpacity = 0.6, color = ~ ColoursFrontmap(Infantry), group = "Troops (Infantry)",
    label = paste("<strong>", FrontmapDataInfantry$mission, "<br/>Location:</strong>", FrontmapDataInfantry$location, "<br/><strong>Troop number:</strong>", FrontmapDataInfantry$Infantry) %>% lapply(htmltools::HTML)
  ) %>%
  addCircleMarkers(
    data = (FrontmapDataMissionSiteOnly <- FrontmapData %>% filter(ave.no.troops == 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, weight = 0.5, radius = 3,
    fillOpacity = 0.4, color = "#666666", group = "Mission Site (No Troops)",
    label = paste("<strong>", FrontmapDataMissionSiteOnly$mission, "<br/>Location:</strong>", FrontmapDataMissionSiteOnly$location, "<br/><strong>Mission site </strong>(no troop deployment)") %>% lapply(htmltools::HTML)
  ) %>%
  addCircleMarkers(
    data = (FrontmapDataReserve <- FrontmapData %>% filter(Reserve > 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (Reserve)^(1 / 3.5),
    fillOpacity = 0.6, color = ~ ColoursFrontmapReserve(Reserve), group = "Troops (Reserve)",
    label = paste("<strong>", FrontmapDataReserve$mission, "<br/>Location:</strong>", FrontmapDataReserve$location, "<br/><strong>Reserve Troop number:</strong>", FrontmapDataReserve$Reserve) %>% lapply(htmltools::HTML)
  ) %>%
  addAwesomeMarkers(
    data = (FrontmapDataUNPOL <- FrontmapData %>% filter(UNPOL > 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, icon = UNPOLicon, group = "UNPOL",
    label = paste("<strong>UNPOL</strong><br/>", FrontmapDataUNPOL$location, "-", FrontmapDataUNPOL$mission) %>% lapply(htmltools::HTML)
  ) %>%
  addAwesomeMarkers(
    data = (FrontmapDataUNMO <- FrontmapData %>% filter(UNMO > 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, icon = UNMOicon, group = "UNMO",
    label = paste("<strong>UNMO</strong><br/>", FrontmapDataUNMO$location, "-", FrontmapDataUNMO$mission) %>% lapply(htmltools::HTML)
  ) %>%
  addAwesomeMarkers(
    data = (FrontmapDataHQ <- FrontmapData %>% filter(hq == 3) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, icon = HQicon, group = "Mission HQs",
    label = paste("<strong>Mission HQ</strong><br/>", FrontmapDataHQ$location, "-", FrontmapDataHQ$mission) %>% lapply(htmltools::HTML)
  )

# Map with absolute panel
basemapFront_abso <- leaflet(geopko, options = leafletOptions(minZoom = 2)) %>%
  addTiles() %>%
  setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
  addLayersControl(
    position = "bottomright",
    baseGroups = c("Deployments (All)", "Troops (Infantry)", "Troops (Reserve)", "Mission Site (No Troops)", "None"),
    overlayGroups = c("UNPOL", "UNMO", "Mission HQs"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c("UNPOL", "UNMO", "Mission HQs")) %>%
  fitBounds(~ -70, -50, ~60, 60) %>%
  addLegend(pal = ColoursFrontmap, values = ~ FrontmapData_abso$ave.no.troops, group = "Troop deployment", title = "Number of troops") %>%
  addCircleMarkers(
    data = (FrontmapData_abso_Troop <- FrontmapData_abso %>% filter(ave.no.troops > 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (ave.no.troops)^(1 / 3.5),
    fillOpacity = 0.6, color = ~ ColoursFrontmap(ave.no.troops), group = "Deployments (All)",
    label = paste("<strong>", FrontmapData_abso_Troop$mission, "<br/>Location:</strong>", FrontmapData_abso_Troop$location, "<br/><strong>Troop number:</strong>", FrontmapData_abso_Troop$ave.no.troops) %>% lapply(htmltools::HTML)
  ) %>%
  addCircleMarkers(
    data = (FrontmapData_abso_MissionSite <- FrontmapData_abso %>% filter(ave.no.troops == 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, weight = 0.5, radius = 3,
    fillOpacity = 0.4, color = "#666666", group = "Deployments (All)",
    label = paste("<strong>", FrontmapData_abso_MissionSite$mission, "<br/>Location:</strong>", FrontmapData_abso_MissionSite$location, "<br/><strong>Mission site</strong> (no troop deployment)") %>% lapply(htmltools::HTML)
  ) %>%
  addCircleMarkers(
    data = (FrontmapData_abso_Infantry <- FrontmapData_abso %>% filter(Infantry > 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (ave.no.troops)^(1 / 3.5),
    fillOpacity = 0.6, color = ~ ColoursFrontmap(Infantry), group = "Troops (Infantry)",
    label = paste("<strong>", FrontmapData_abso_Infantry$mission, "<br/>Location:</strong>", FrontmapData_abso_Infantry$location, "<br/><strong>Troop number:</strong>", FrontmapData_abso_Infantry$Infantry) %>% lapply(htmltools::HTML)
  ) %>%
  addCircleMarkers(
    data = (FrontmapData_abso_MissionSiteOnly <- FrontmapData_abso %>% filter(ave.no.troops == 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, weight = 0.5, radius = 3,
    fillOpacity = 0.4, color = "#666666", group = "Mission Site (No Troops)",
    label = paste("<strong>", FrontmapData_abso_MissionSiteOnly$mission, "<br/>Location:</strong>", FrontmapData_abso_MissionSiteOnly$location, "<br/><strong>Mission site </strong>(no troop deployment)") %>% lapply(htmltools::HTML)
  ) %>%
  addCircleMarkers(
    data = (FrontmapData_abso_Reserve <- FrontmapData_abso %>% filter(Reserve > 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (Reserve)^(1 / 3.5),
    fillOpacity = 0.6, color = ~ ColoursFrontmapReserve(Reserve), group = "Troops (Reserve)",
    label = paste("<strong>", FrontmapData_abso_Reserve$mission, "<br/>Location:</strong>", FrontmapData_abso_Reserve$location, "<br/><strong>Reserve Troop number:</strong>", FrontmapData_abso_Reserve$Reserve) %>% lapply(htmltools::HTML)
  ) %>%
  addAwesomeMarkers(
    data = (FrontmapData_abso_UNPOL <- FrontmapData_abso %>% filter(UNPOL > 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, icon = UNPOLicon, group = "UNPOL",
    label = paste("<strong>UNPOL</strong><br/>", FrontmapData_abso_UNPOL$location, "-", FrontmapData_abso_UNPOL$mission) %>% lapply(htmltools::HTML)
  ) %>%
  addAwesomeMarkers(
    data = (FrontmapData_abso_UNMO <- FrontmapData_abso %>% filter(UNMO > 0) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, icon = UNMOicon, group = "UNMO",
    label = paste("<strong>UNMO</strong><br/>", FrontmapData_abso_UNMO$location, "-", FrontmapData_abso_UNMO$mission) %>% lapply(htmltools::HTML)
  ) %>%
  addAwesomeMarkers(
    data = (FrontmapData_abso_HQ <- FrontmapData_abso %>% filter(hq == 3) %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, icon = HQicon, group = "Mission HQs",
    label = paste("<strong>Mission HQ</strong><br/>", FrontmapData_abso_HQ$location, "-", FrontmapData_abso_HQ$mission) %>% lapply(htmltools::HTML)
  )



#### Map doesnt load on initial go, Map 2 base here
TCC_basemap <- leaflet(geopko, options = leafletOptions(minZoom = 2)) %>%
  addTiles() %>%
  setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
  fitBounds(~ -70, -50, ~60, 60) %>%
  addLegend(pal = ColoursTCCmap, values = ~ TCCmapData$No.TCC, group = "TCC", title = "Number of TCCs") %>%
  addCircleMarkers(
    data = (TCCmapData2024 <- TCCmapData %>% filter(year == 2024)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (No.TCC),
    fillOpacity = 0.8, color = ~ ColoursTCCmap(No.TCC), group = "TCC", labelOptions = labelOptions(style = list(
      "width" = "150px", "white-space" = "normal"
    )),
    label = paste(
      "<strong>", TCCmapData2024$mission, "</strong><br/><strong>Location:</strong>", TCCmapData2024$location,
      "<br/><strong>Total number of TCCs:</strong>", TCCmapData2024$No.TCC, "<br/><strong>Countries:</strong>",
      TCCmapData2024$year.overview
    ) %>% lapply(htmltools::HTML)
  )

#### BaseMap third map

TroopType_basemap <- leaflet(geopko, options = leafletOptions(minZoom = 2)) %>%
  addTiles() %>%
  setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
  addLayersControl(
    position = "bottomright",
    baseGroups = c("Infantry", "None"),
    overlayGroups = c("Medical", "Engineering", "Signals", "Aviation", "Transport", "Maintenance", "Riverine"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c("Medical", "Aviation", "Engineering", "Transport", "Signals", "Maintenance", "Riverine")) %>%
  fitBounds(~ -70, -50, ~60, 60) %>%
  addLegend(pal = ColoursTTmap, values = ~ TTmapData$Infantry, group = "Infantry", title = "Number of troops") %>%
  addCircleMarkers(
    data = (TTmapDataInf <- TTmapData %>% filter(year == 2024) %>% filter(Infantry > 0)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (Infantry)^(1 / 3.5),
    fillOpacity = 0.6, color = ~ ColoursTTmap(Infantry), group = "Infantry",
    label = paste("<strong>", TTmapDataInf$mission, "</strong><br/><strong>Location:</strong>", TTmapDataInf$location, "<br/><strong>Troop number:</strong>", TTmapDataInf$Infantry) %>% lapply(htmltools::HTML)
  ) %>%
  addAwesomeMarkers(
    data = (TTmapDataMed <- TTmapData %>% filter(year == 2024) %>% filter(med > 0)), lat = ~ latitude + 0.2, lng = ~ longitude + 0.2, icon = Medicon, group = "Medical",
    label = paste("<strong>Medical</strong><br/>", TTmapDataMed$location, "-", TTmapDataMed$mission) %>% lapply(htmltools::HTML)
  ) %>%
  addAwesomeMarkers(
    data = (TTmapDataEng <- TTmapData %>% filter(year == 2024) %>% filter(eng > 0)), lat = ~latitude, lng = ~longitude, icon = Engicon, group = "Engineering",
    label = paste("<strong>Engineering</strong><br/>", TTmapDataEng$location, "-", TTmapDataEng$mission) %>% lapply(htmltools::HTML)
  ) %>%
  addAwesomeMarkers(
    data = (TTmapDataSig <- TTmapData %>% filter(year == 2024) %>% filter(sig > 0)), lat = ~ latitude - 0.2, lng = ~ longitude - 0.2, icon = Sigicon, group = "Signals",
    label = paste("<strong>Signal</strong><br/>", TTmapDataSig$location, "-", TTmapDataSig$mission) %>% lapply(htmltools::HTML)
  ) %>%
  addAwesomeMarkers(
    data = (TTmapDataAvia <- TTmapData %>% filter(year == 2024) %>% filter(av > 0)), lat = ~ latitude + 0.4, lng = ~ longitude + 0.4, icon = Avicon, group = "Aviation",
    label = paste("<strong>Aviation</strong><br/>", TTmapDataAvia$location, "-", TTmapDataAvia$mission) %>% lapply(htmltools::HTML)
  ) %>%
  addAwesomeMarkers(
    data = (TTmapDataRiv <- TTmapData %>% filter(year == 2024) %>% filter(riv > 0)), lat = ~ latitude - 0.6, lng = ~ longitude - 0.6, icon = Rivicon, group = "Riverine",
    label = paste("<strong>Riverine</strong><br/>", TTmapDataRiv$location, "-", TTmapDataRiv$mission) %>% lapply(htmltools::HTML)
  ) %>%
  addAwesomeMarkers(
    data = (TTmapDataMaint <- TTmapData %>% filter(year == 2024) %>% filter(maint > 0)), lat = ~ latitude - 0.4, lng = ~ longitude - 0.4, icon = Mainticon, group = "Maintenance",
    label = paste("<strong>Maintenance</strong><br/>", TTmapDataMaint$location, "-", TTmapDataMaint$mission) %>% lapply(htmltools::HTML)
  ) %>%
  addAwesomeMarkers(
    data = (TTmapDataTrans <- TTmapData %>% filter(year == 2024) %>% filter(trans > 0)), lat = ~ latitude + 0.6, lng = ~ longitude + 0.6, icon = Traicon, group = "Transport",
    label = paste("<strong>Transport</strong><br/>", TTmapDataTrans$location, "-", TTmapDataTrans$mission) %>% lapply(htmltools::HTML)
  )


#### data prep deployment maps####

map_df <- geopko %>%
  mutate_at(vars(c(
    no.troops, no.tcc, longitude, latitude,
    unmo.dummy, unpol.dummy
  )), as.numeric) %>%
  mutate(hq = as.factor(hq)) %>% 
  mutate(ordered_yrm =zoo::as.yearmon(timepoint, "%Y %B")) %>% 
  group_by(mission) %>% 
  arrange(ordered_yrm) %>% ungroup()

cclist3 <- map_df %>%
  dplyr::select(mission, iso3c) %>%
  distinct() %>% # creating list of country codes for GADM sf files dowload
  mutate(iso3c = strsplit(as.character(iso3c), ", ")) %>%
  unnest(iso3c) %>%
  distinct()

# oxford comma paste
country_list <- function(w, oxford = T) {
  if (length(w) == 1) {
    return(paste("This mission was active in the following country or territory:", w))
  }
  if (length(w) == 2) {
    return(paste("This mission was active in the following countries or territories:", w[1], "and", w[2]))
  }
  paste0(
    "This mission was active in the following countries or territories: ", paste(w[-length(w)], collapse = ", "),
    ifelse(oxford, ",", ""), " and ", w[length(w)]
  )
}

#### data prep TCC ####
tcc_df <- geopko %>%
  mutate_at(vars(c(
    no.troops, no.tcc, longitude, latitude,
    unmo.dummy, unpol.dummy
  )), as.numeric) %>%
  mutate(hq = as.factor(hq)) %>%
  dplyr::select(
    source, mission, year, month, MonthName,
    no.troops, nameoftcc_1:notroopspertcc_17
  ) %>%
  group_by(source, mission, year, month, MonthName) %>%
  mutate(Total.troops = sum(no.troops, na.rm = T)) %>%
  ungroup()

#### anim ####

map_df %>%
  group_by(mission) %>%
  count(length(unique(timepoint))) %>%
  filter(`length(unique(timepoint))` > 5) %>%
  pull(mission) -> anim_choice_list1

map_df %>%
  group_by(mission, timepoint) %>%
  summarise(total = sum(no.troops, na.rm = TRUE)) %>%
  filter(total != 0) %>%
  distinct(mission) %>%
  pull() -> anim_choice_list2

intersect(anim_choice_list1, anim_choice_list2) -> anim_choice_list

#### lollipop data prep####

Years <- geopko %>% 
  group_by(mission, location) %>%
  summarize(start_date = min(year), end_date = max(year))

#### UI####

ui <- bootstrapPage(
  navbarPage("Exploring Geo-PKO",
             collapsible = TRUE,
             navbarMenu(
               "Global Deployments",
               tabPanel(
                 "Troop Deployments",
                 # spinner on loading
                 use_waiter(),
                 waiter_show_on_load(html = spin_wave()),
                 # JS to grab page width
                 tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
                 
                 # 1st conditional panel for smaller screens
                 conditionalPanel(
                   condition = "window.innerWidth < 1000 || window.innerHeight < 720",
                   column(
                     width = 4, style = "padding-left:8px; padding-right:8px; margin-bottom:20px;",
                     span("This interactive map shows peacekeeping deployments from 1994-2024 based on 
                                             publicly available United Nations (UN) peacekeeping deployment maps and mission 
                                             progress reports. 'Mission Site' indicates no active troop deployments, 
                                             but the presence of support personnel such as UNPOL (UN Police) 
                                             and/or UNMO (UN Military Observer).")
                   ),
                   column(
                     width = 2, style = "padding-left:8px; padding-right:8px; margin-bottom:20px;",
                     div(
                       style = "margin-bottom:15px;",
                       span(tags$b(textOutput("reactive_year"), align = "left"), style = "color:#15110d"),
                       tags$b(p(textOutput("reactive_troopcount")), align = "left", style = "color:#888888"),
                       tags$b(p(textOutput("reactive_UNPOLcount")), align = "left", style = "color:#888888"),
                       tags$b(p(textOutput("reactive_UNMOcount")), align = "left", style = "color:#888888")
                     )
                   ),
                   column(
                     width = 3, style = "padding-left:8px; padding-right:8px;",
                     div(
                       sliderInput(
                         inputId = "YearFront",
                         label = "Select deployment year",
                         min = 1994,
                         max = 2024,
                         value = 2024,
                         step = 1,
                         sep = "",
                         width = "100%",
                         animate = animationOptions(interval = 2000, loop = TRUE)
                       ),
                       chooseSliderSkin("Shiny", color = "transparent")
                     ),
                     tags$style(type = "text/css", HTML(".irs-single {color:black; background:transparent}"))
                   ),
                   column(
                     width = 3, style = "padding-left:8px; padding-right:8px; margin-bottom:20px;",
                     div(
                       pickerInput("missionsFront", "Select mission(s)",
                                   choices = as.character(unique(FrontmapData$mission)),
                                   selected = as.character(unique(FrontmapData$mission)),
                                   options = list(`actions-box` = TRUE, size = 5), multiple = T
                       ),
                       span(textOutput("reactive_yearMissions"), align = "left", style = "color:#888888")
                     )
                   )
                 ),
                 conditionalPanel(condition = "window.innerWidth < 1000 || window.innerHeight < 720", div(
                   class = "outer",
                   tags$style(type = "text/css", "#basemap {height: calc(100vh - 110px) !important;}"),
                   leafletOutput("basemap")
                 )),
                 
                 # 2nd conditional panel for larger screens
                 conditionalPanel(
                   condition = "window.innerWidth > 1000 && window.innerHeight > 720",
                   div(
                     class = "outer", tags$style(type = "text/css", "#basemap_abso {height: calc(100vh - 110px) !important;}"),
                     leafletOutput("basemap_abso")
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "window.innerWidth > 1000 && window.innerHeight > 720",
                   absolutePanel(
                     class = "panel panel-default", top = 70, left = 85, width = 320,
                     height = "auto", fixed = TRUE,
                     style = "padding: 14px; background:rgba(232, 232, 232, 0.8);bottom:25px",
                     div(
                       style = "margin-bottom:15px;",
                       p("This interactive map shows peacekeeping deployments from 1994-2024, based on 
                                             publicly available United Nations (UN) peacekeeping deployment maps and mission 
                                             progress reports. 'Mission Site' indicates no active troop deployments, 
                                             but the presence of support personnel such as UNPOL (UN Police) 
                                             and/or UNMO (UN Military Observer)."),
                     ),
                     div(
                       style = "margin-bottom:15px;",
                       tags$b(p(textOutput("reactive_year_absoPanel")), align = "left", style = "color:#15110d"),
                       tags$b(p(textOutput("reactive_troopcount_absoPanel")), align = "left", style = "color:#888888"),
                       tags$b(p(textOutput("reactive_UNPOLcount_absoPanel")), align = "left", style = "color:#888888"),
                       tags$b(p(textOutput("reactive_UNMOcount_absoPanel")), align = "left", style = "color:#888888"),
                     ),
                     div(
                       sliderInput(
                         inputId = "YearFront_abso",
                         label = "Select deployment year",
                         min = 1994,
                         max = 2024,
                         value = 2024,
                         step = 1,
                         sep = "",
                         width = "100%",
                         animate = animationOptions(interval = 2000, loop = TRUE)
                       ),
                       chooseSliderSkin("Shiny", color = "transparent"),
                       tags$style(type = "text/css", HTML(".irs-single {color:black; background:transparent}"))
                     ),
                     div(
                       pickerInput("missionsFront_abso", "Select mission(s)",
                                   choices = as.character(unique(FrontmapData_abso$mission)),
                                   selected = as.character(unique(FrontmapData_abso$mission)),
                                   options = list(`actions-box` = TRUE, size = 5), multiple = T
                       ),
                       span(textOutput("reactive_yearMissions_absoPanel"), align = "left", style = "color:#888888"),
                       tags$style(type = "text/css", HTML(".irs-grid-text {color:#333333}"))
                     )
                   )
                 )
               ),
               tabPanel(
                 "Troop-Contributing Countries",
                 sidebarLayout(sidebarPanel("This map shows how many troop-contributing countries (TCCs) have deployed peacekeepers to a location. TCCs and the number of troops each country has contributed are shown in the labels.<br/><br/>" %>% lapply(htmltools::HTML),
                                            chooseSliderSkin("Shiny", color = "transparent"),
                                            sliderInput(
                                              inputId = "YearTCC",
                                              label = "Select year",
                                              min = 1994,
                                              max = 2024,
                                              value = 2024,
                                              step = 1,
                                              sep = "",
                                              width = "100%",
                                              animate = animationOptions(interval = 2000, loop = TRUE)
                                            ),
                                            pickerInput("missionsTCC", "Select mission(s)",
                                                        choices = as.character(unique(TCCmapData$mission)),
                                                        selected = as.character(unique(TCCmapData$mission)),
                                                        options = list(`actions-box` = TRUE), multiple = T
                                            ),
                                            width = 3
                 ),
                 mainPanel(tags$style(type = "text/css", "#map {height: calc(100vh - 130px) !important; margin-bottom: 15px;}"), leafletOutput("map", width = "100%")),
                 position = c("left", "right")
                 )
               ),
               tabPanel(
                 "Troop Types",
                 sidebarLayout(sidebarPanel(paste("This map shows the various types of troops deployed to a location. In the dataset, a specific number of deployed troops is provided for infantry. For other troop types, it is only indicated whether they are present or absent. Aviation includes both air- and helicopter support.<br/><br/>When selecting different troop types, overlap can occur. If no icons appear when selecting a troop type, this type is not present in the selected year.<br/><br/>") %>% lapply(htmltools::HTML),
                                            chooseSliderSkin("Shiny", color = "transparent"),
                                            sliderInput(
                                              inputId = "YearTT",
                                              label = "Select year",
                                              min = 1994,
                                              max = 2024,
                                              value = 2024,
                                              step = 1,
                                              sep = "",
                                              width = "100%",
                                              animate = animationOptions(interval = 2000, loop = TRUE)
                                            ),
                                            pickerInput("missionsTT", "Select mission(s)",
                                                        choices = as.character(unique(TTmapData$mission)),
                                                        selected = as.character(unique(TTmapData$mission)),
                                                        options = list(`actions-box` = TRUE), multiple = T
                                            ),
                                            width = 3
                 ),
                 mainPanel(tags$style(type = "text/css", "#TroopTypeMap {height: calc(100vh - 130px) !important; margin-bottom: 15px;}"), leafletOutput("TroopTypeMap", width = "100%")), #### Screen size, responsive to different types
                 position = c("left", "right")
                 )
               )
             ),
             navbarMenu(
               "Missions",
               tabPanel("Mission Maps",
                        fluid = TRUE,
                        titlePanel("Mission Maps"),
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            p("Where are UN peacekeepers posted, and what are their strengths and 
                                                  capabilities? Select the options below to visualise deployment patterns."),
                            selectInput(
                              inputId = "mission_map", label = "Select a mission",
                              choices = levels(factor(map_df$mission)),
                              width = 150, selected = "MINUSMA", multiple = F
                            ),
                            selectInput("timepoint_map",
                                        "Choose year and month",
                                        choices = NULL, selected = NULL
                            ),
                            # checkboxInput(inputId="depsize_map",
                            #               "Deployment size", value=TRUE),
                            checkboxInput(
                              inputId = "MHQ_map",
                              "Mission HQ", value = FALSE
                            ),
                            checkboxInput(
                              inputId = "SHQ_map",
                              "Sector HQ", value = FALSE
                            ),
                            checkboxInput(
                              inputId = "MO_map",
                              "UNMO", value = FALSE
                            ),
                            checkboxInput(
                              inputId = "UNPOL_map",
                              "UNPOL", value = FALSE
                            ),
                            # actionButton("dostaticmap", "Generate map"),
                            helpText("Maps may not offer all of the above features. If errors occur, please deselect the option to redraw the map.")
                          ),
                          mainPanel(
                            withSpinner(plotOutput("depmap", height = "auto")),
                            span(h6(textOutput("basecountries"), align = "center")),
                            hr(),
                            fluidRow(
                              DT::dataTableOutput(outputId = "map_df_details")
                            )
                          )
                        )
               ),
               #### animated maps UI####
               tabPanel("Animated Maps",
                        fluid = TRUE,
                        titlePanel("Animated Maps"),
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            p("Animated maps show how deployment patterns change over time for each mission. Select a mission and click on the button below to explore."),
                            selectInput(
                              inputId = "anim_map", label = "Mission",
                              choices = anim_choice_list, width = 200, selected = NULL
                            ),
                            actionButton("go_anim", tags$b("Render animation")),
                            p(""),
                            helpText("Rendering may take time as it entails producing and combining multiple frames. For the best effect, only missions with more than five source maps are available to select here.")
                          ),
                          mainPanel(
                            fluid = TRUE,
                            withSpinner(imageOutput("animated"))
                          )
                        )
               ),
               tabPanel(
                 "Deployment Periods",
                 sidebarLayout(
                   sidebarPanel(
                     p("This graph shows the specific time period during which a location had at least one active deployment."),
                     selectInput(
                       inputId = "Lollipop_map", label = "Select a mission",
                       choices = levels(factor(geopko$mission)), width = 150,
                       selected = "MONUSCO"
                     ),
                     width = 3,
                     helpText("The graphs do not show possible temporal interruptions.")
                   ),
                   mainPanel(
                     fluid = TRUE,
                     plotOutput("lollipop", height = "auto")
                   )
                 )
               )
             ),
             #### TCC Table UI####
             tabPanel(
               "Countries",
               titlePanel("Countries"),
               sidebarLayout(
                 sidebarPanel(
                   p("How many troops have different countries contributed to peacekeeping missions? Choose to view by each published deployment map (recommended) or by year."),
                   radioButtons(
                     inputId = "databy_tcc",
                     label = "Present data by:",
                     choices = c("Deployment map", "Year"),
                     selected = "Deployment map"
                   ),
                   width = 3,
                   helpText("The Geo-PKO dataset collects data by deployment maps published by the UN. Data by year present the year's average troop counts and the highest number of troop-contributing countries (TCCs).")
                 ),
                 # span(h6(textOutput("tabletext", align="right"))),
                 mainPanel(
                   fluid = TRUE,
                   helpText("Tip: Use the search box to filter for a specific mission or a troop-contributing country."),
                   DT::dataTableOutput("tcc_table")
                 )
               )
             ),
             tabPanel("Data",
                      tags$div(
                        withMathJax(includeMarkdown("data.md")),
                      ),
                      style = "margin-left:40px;margin-right:40px;"
             ),
             tabPanel("About", tags$div(
               withMathJax(includeMarkdown("about.md"))
             ), style = "margin-left:40px;margin-right:40px;"),
             tags$head(tags$style(".leaflet-top {z-index:999!important;}"))
  )
)


server <- function(input, output, session) {
  Sys.sleep(5) # do something that takes time
  waiter_hide()
  
  mission_list <- map_df %>%
    distinct(mission) %>%
    arrange(mission)
  
  #### Leaflet####
  
  #### Reactive Data Frames
  # Front map Datas
  filteredData <- reactive({
    FrontmapData %>%
      drop_na(ave.no.troops) %>%
      filter(mission %in% input$missionsFront &
               year %in% input$YearFront)
  })
  
  filteredData_absolute <- reactive({
    FrontmapData_abso %>%
      drop_na(ave.no.troops) %>%
      filter(mission %in% input$missionsFront_abso &
               year %in% input$YearFront_abso)
  })
  
  # Troop Contributing Countries map Data
  filteredDataTCC <- reactive({
    TCCmapData %>% filter(mission %in% input$missionsTCC & year %in% input$YearTCC)
  })
  
  # Troop Type map Data
  filteredDataTroopType <- reactive({
    TTmapData %>% filter(mission %in% input$missionsTT & year %in% input$YearTT)
  })
  
  ##### Front map basis
  output$basemap <- renderLeaflet({
    leaflet(geopko, options = leafletOptions(minZoom = 2)) %>%
      addTiles() %>%
      setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
      addLayersControl(
        position = "bottomright",
        baseGroups = c("Deployments (All)", "Troops (Infantry)", "Troops (Reserve)", "Mission Site (No Troops)", "None"),
        overlayGroups = c("UNPOL", "UNMO", "Mission HQs"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("UNPOL", "UNMO", "Mission HQs")) %>%
      fitBounds(~ -70, -50, ~60, 60) %>%
      addLegend(pal = ColoursFrontmap, values = ~ FrontmapData$ave.no.troops, group = "Troop deployment", title = "Number of troops")
  })
  
  ### Map Front
  output$ basemap <- renderLeaflet({
    basemapFront
  })
  output$basemap_abso <- renderLeaflet({
    basemapFront_abso
  })
  
  #### Map for TCC
  output$map <- renderLeaflet({
    TCC_basemap
  })
  
  #### Map for Troop Types
  output$TroopTypeMap <- renderLeaflet({
    TroopType_basemap
  })
  
  
  # Reactive Text for the front page
  # Year

  
  
  # Active missions in given year
  output$reactive_yearMissions <- renderText({
    paste0("Active missions in ", unique(filteredData()$year), ": ", mission_comma(unique(filteredData()$mission)))
  })
  output$reactive_yearMissions_absoPanel <- renderText({
    paste0("Active missions in ", unique(filteredData_absolute()$year), ": ", mission_comma(unique(filteredData_absolute()$mission)))
  })
  
  
  ### Generate the troop deployment map (front)
  ### Observe Front map with top panel
  observe({
    req(input$dimension[1] < 1000)
    
    leafletProxy(mapId = "basemap", data = filteredData()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(
        data = (filteredDataTroop <- filteredData() %>% filter(ave.no.troops > 0)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (ave.no.troops)^(1 / 3.5),
        fillOpacity = 0.6, color = ~ ColoursFrontmap(ave.no.troops), group = "Deployments (All)",
        label = paste("<strong>", filteredDataTroop$mission, "<br/>Location:</strong>", filteredDataTroop$location, "<br/><strong>Troop number:</strong>", filteredDataTroop$ave.no.troops) %>% lapply(htmltools::HTML)
      ) %>%
      addCircleMarkers(
        data = (filteredDataMissionSite <- filteredData() %>% filter(ave.no.troops == 0)), lat = ~latitude, lng = ~longitude, weight = 0.5, radius = 3,
        fillOpacity = 0.4, color = "#666666", group = "Deployments (All)",
        label = paste("<strong>", filteredDataMissionSite$mission, "<br/>Location:</strong>", filteredDataMissionSite$location, "<br/><strong>Mission site</strong> (no troop deployment)") %>% lapply(htmltools::HTML)
      ) %>%
      addCircleMarkers(
        data = (filteredDataInfantry <- filteredData() %>% filter(Infantry > 0)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (ave.no.troops)^(1 / 3.5),
        fillOpacity = 0.6, color = ~ ColoursFrontmap(Infantry), group = "Troops (Infantry)",
        label = paste("<strong>", filteredDataInfantry$mission, "<br/>Location:</strong>", filteredDataInfantry$location, "<br/><strong>Troop number:</strong>", filteredDataInfantry$Infantry) %>% lapply(htmltools::HTML)
      ) %>%
      addCircleMarkers(
        data = (filteredDataMissionSiteOnly <- filteredData() %>% filter(ave.no.troops == 0)), lat = ~latitude, lng = ~longitude, weight = 0.5, radius = 3,
        fillOpacity = 0.4, color = "#666666", group = "Mission Site (No Troops)",
        label = paste("<strong>", filteredDataMissionSiteOnly$mission, "<br/>Location:</strong>", filteredDataMissionSiteOnly$location, "<br/><strong>Mission site </strong>(no troop deployment)") %>% lapply(htmltools::HTML)
      ) %>%
      addCircleMarkers(
        data = (filteredDataReserve <- filteredData() %>% filter(Reserve > 0)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (Reserve)^(1 / 3.5),
        fillOpacity = 0.6, color = ~ ColoursFrontmapReserve(Reserve), group = "Troops (Reserve)",
        label = paste("<strong>", filteredDataReserve$mission, "<br/>Location:</strong>", filteredDataReserve$location, "<br/><strong>Reserve Troop number:</strong>", filteredDataReserve$Reserve) %>% lapply(htmltools::HTML)
      ) %>%
      addAwesomeMarkers(
        data = (filteredDataUNPOL <- filteredData() %>% filter(UNPOL > 0)), lat = ~latitude, lng = ~longitude, icon = UNPOLicon, group = "UNPOL",
        label = paste("<strong>UNPOL</strong><br/>", filteredDataUNPOL$location, "-", filteredDataUNPOL$mission) %>% lapply(htmltools::HTML)
      ) %>%
      addAwesomeMarkers(
        data = (filteredDataUNMO <- filteredData() %>% filter(UNMO > 0)), lat = ~latitude, lng = ~longitude, icon = UNMOicon, group = "UNMO",
        label = paste("<strong>UNMO</strong><br/>", filteredDataUNMO$location, "-", filteredDataUNMO$mission) %>% lapply(htmltools::HTML)
      ) %>%
      addAwesomeMarkers(
        data = (filteredDataHQ <- filteredData() %>% filter(hq == 3)), lat = ~latitude, lng = ~longitude, icon = HQicon, group = "Mission HQs",
        label = paste("<strong>Mission HQ</strong><br/>", filteredDataHQ$location, "-", filteredDataHQ$mission) %>% lapply(htmltools::HTML)
      )
  })
  
  ### Observe Front map with absolute panel
  
  observe({
    req(input$dimension[1] > 1000)
    
    leafletProxy(mapId = "basemap_abso", data = filteredData_absolute()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(
        data = (filteredDataTroop <- filteredData_absolute() %>% filter(ave.no.troops > 0)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (ave.no.troops)^(1 / 3.5),
        fillOpacity = 0.6, color = ~ ColoursFrontmap(ave.no.troops), group = "Deployments (All)",
        label = paste("<strong>", filteredDataTroop$mission, "<br/>Location:</strong>", filteredDataTroop$location, "<br/><strong>Troop number:</strong>", filteredDataTroop$ave.no.troops) %>% lapply(htmltools::HTML)
      ) %>%
      addCircleMarkers(
        data = (filteredDataMissionSite <- filteredData_absolute() %>% filter(ave.no.troops == 0)), lat = ~latitude, lng = ~longitude, weight = 0.5, radius = 3,
        fillOpacity = 0.4, color = "#666666", group = "Deployments (All)",
        label = paste("<strong>", filteredDataMissionSite$mission, "<br/>Location:</strong>", filteredDataMissionSite$location, "<br/><strong>Mission site</strong> (no troop deployment)") %>% lapply(htmltools::HTML)
      ) %>%
      addCircleMarkers(
        data = (filteredDataInfantry <- filteredData_absolute() %>% filter(Infantry > 0)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (ave.no.troops)^(1 / 3.5),
        fillOpacity = 0.6, color = ~ ColoursFrontmap(Infantry), group = "Troops (Infantry)",
        label = paste("<strong>", filteredDataInfantry$mission, "<br/>Location:</strong>", filteredDataInfantry$location, "<br/><strong>Troop number:</strong>", filteredDataInfantry$Infantry) %>% lapply(htmltools::HTML)
      ) %>%
      addCircleMarkers(
        data = (filteredDataMissionSiteOnly <- filteredData_absolute() %>% filter(ave.no.troops == 0)), lat = ~latitude, lng = ~longitude, weight = 0.5, radius = 3,
        fillOpacity = 0.4, color = "#666666", group = "Mission Site (No Troops)",
        label = paste("<strong>", filteredDataMissionSiteOnly$mission, "<br/>Location:</strong>", filteredDataMissionSiteOnly$location, "<br/><strong>Mission site </strong>(no troop deployment)") %>% lapply(htmltools::HTML)
      ) %>%
      addCircleMarkers(
        data = (filteredDataReserve <- filteredData_absolute() %>% filter(Reserve > 0)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (Reserve)^(1 / 3.5),
        fillOpacity = 0.6, color = ~ ColoursFrontmapReserve(Reserve), group = "Troops (Reserve)",
        label = paste("<strong>", filteredDataReserve$mission, "<br/>Location:</strong>", filteredDataReserve$location, "<br/><strong>Reserve Troop number:</strong>", filteredDataReserve$Reserve) %>% lapply(htmltools::HTML)
      ) %>%
      addAwesomeMarkers(
        data = (filteredDataUNPOL <- filteredData_absolute() %>% filter(UNPOL > 0)), lat = ~latitude, lng = ~longitude, icon = UNPOLicon, group = "UNPOL",
        label = paste("<strong>UNPOL</strong><br/>", filteredDataUNPOL$location, "-", filteredDataUNPOL$mission) %>% lapply(htmltools::HTML)
      ) %>%
      addAwesomeMarkers(
        data = (filteredDataUNMO <- filteredData_absolute() %>% filter(UNMO > 0)), lat = ~latitude, lng = ~longitude, icon = UNMOicon, group = "UNMO",
        label = paste("<strong>UNMO</strong><br/>", filteredDataUNMO$location, "-", filteredDataUNMO$mission) %>% lapply(htmltools::HTML)
      ) %>%
      addAwesomeMarkers(
        data = (filteredDataHQ <- filteredData_absolute() %>% filter(hq == 3)), lat = ~latitude, lng = ~longitude, icon = HQicon, group = "Mission HQs",
        label = paste("<strong>Mission HQ</strong><br/>", filteredDataHQ$location, "-", filteredDataHQ$mission) %>% lapply(htmltools::HTML)
      )
  })
  #### Second observe for TCC map
  observe({
    leafletProxy(mapId = "map", data = filteredDataTCC()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(
        data = filteredDataTCC(), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (No.TCC) * (1.5),
        fillOpacity = 0.6, color = ~ ColoursTCCmap(No.TCC), group = "TCC", labelOptions = labelOptions(style = list(
          "width" = "150px", "white-space" = "normal"
        )),
        label = paste("<strong>", filteredDataTCC()$mission, "</strong><br/><strong>Location:</strong>", filteredDataTCC()$location, "<br/><strong>Total number of TCCs:</strong>", filteredDataTCC()$No.TCC, "<br/><strong>Countries:</strong>", filteredDataTCC()$year.overview) %>% lapply(htmltools::HTML)
      )
  })
  
  
  #### Third observe for Troop Type map
  observe({
    leafletProxy(mapId = "TroopTypeMap", data = filteredDataTroopType()) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(
        data = (filteredDataTTInfantry <- filteredDataTroopType() %>% filter(Infantry > 0)), lat = ~latitude, lng = ~longitude, weight = 1, radius = ~ (Infantry)^(1 / 3.5),
        fillOpacity = 0.6, color = ~ ColoursTTmap(Infantry), group = "Infantry",
        label = paste("<strong>", filteredDataTTInfantry$mission, "</strong><br/><strong>Location:</strong>", filteredDataTTInfantry$location, "<br/><strong>Troop Number:</strong>", filteredDataTTInfantry$Infantry) %>% lapply(htmltools::HTML)
      ) %>%
      addAwesomeMarkers(
        data = (filteredDataTTMed <- filteredDataTroopType() %>% filter(med > 0)), lat = ~ latitude + 0.2, lng = ~ longitude + 0.2, icon = Medicon, group = "Medical",
        label = paste("<strong>Medical</strong><br/>", filteredDataTTMed$location, "-", filteredDataTTMed$mission) %>% lapply(htmltools::HTML)
      ) %>%
      addAwesomeMarkers(
        data = (filteredDataTTEng <- filteredDataTroopType() %>% filter(eng > 0)), lat = ~latitude, lng = ~longitude, icon = Engicon, group = "Engineering",
        label = paste("<strong>Engineering</strong><br/>", filteredDataTTEng$location, "-", filteredDataTTEng$mission) %>% lapply(htmltools::HTML)
      ) %>%
      addAwesomeMarkers(
        data = (filteredDataTTSig <- filteredDataTroopType() %>% filter(sig > 0)), lat = ~ latitude - 0.2, lng = ~ longitude - 0.2, icon = Sigicon, group = "Signals",
        label = paste("<strong>Signal</strong><br/>", filteredDataTTSig$location, "-", filteredDataTTSig$mission) %>% lapply(htmltools::HTML)
      ) %>%
      addAwesomeMarkers(
        data = (filteredDataTTAvia <- filteredDataTroopType() %>% filter(av > 0)), lat = ~ latitude + 0.4, lng = ~ longitude + 0.4, icon = Avicon, group = "Aviation",
        label = paste("<strong>Aviation</strong><br/>", filteredDataTTAvia$location, "-", filteredDataTTAvia$mission) %>% lapply(htmltools::HTML)
      ) %>%
      addAwesomeMarkers(
        data = (filteredDataTTRiv <- filteredDataTroopType() %>% filter(riv > 0)), lat = ~ latitude - 0.6, lng = ~ longitude - 0.6, icon = Rivicon, group = "Riverine",
        label = paste("<strong>Riverine</strong><br/>", filteredDataTTRiv$location, "-", filteredDataTTRiv$mission) %>% lapply(htmltools::HTML)
      ) %>%
      addAwesomeMarkers(
        data = (filteredDataTTMaint <- filteredDataTroopType() %>% filter(maint > 0)), lat = ~ latitude - 0.4, lng = ~ longitude - 0.4, icon = Mainticon, group = "Maintenance",
        label = paste("<strong>Maintenance</strong><br/>", filteredDataTTMaint$location, "-", filteredDataTTMaint$mission) %>% lapply(htmltools::HTML)
      ) %>%
      addAwesomeMarkers(
        data = (filteredDataTTTra <- filteredDataTroopType() %>% filter(trans > 0)), lat = ~ latitude + 0.6, lng = ~ longitude + 0.6, icon = Traicon, group = "Transport",
        label = paste("<strong>Transport</strong><br/>", filteredDataTTTra$location, "-", filteredDataTTTra$mission) %>% lapply(htmltools::HTML)
      )
  })
  
  #### TCC tables####
  
  bymap_df <- reactive({
    geopko %>%
      mutate_at(vars(c(
        no.troops, no.tcc, longitude, latitude,
        unmo.dummy, unpol.dummy
      )), as.numeric) %>%
      mutate(hq = as.factor(hq)) %>%
      dplyr::select(
        source, mission, year, month, MonthName,
        no.troops, nameoftcc_1:notroopspertcc_17
      ) %>%
      group_by(source, mission, year, month, MonthName) %>%
      mutate(Total.troops = sum(no.troops, na.rm = T)) %>%
      ungroup() %>%
      pivot_longer(c(nameoftcc_1:notroopspertcc_17), names_to = c(".value", "tcc_id"), names_sep = "_") %>%
      filter(!is.na(nameoftcc)) %>%
      mutate_at(vars(notroopspertcc), as.numeric) %>%
      dplyr::select(-tcc_id) %>%
      group_by(source, mission, year, MonthName, Total.troops, nameoftcc) %>%
      summarise(total.tcc = as.character(sum(notroopspertcc), na.rm = TRUE)) %>%
      add_count(source, name = "No.TCC") %>%
      mutate(
        total.tcc = ifelse(is.na(total.tcc), "size unknown", total.tcc),
        overview = paste0(nameoftcc, " (", total.tcc, ")")
      ) %>%
      group_by(source, mission, year, MonthName, Total.troops, No.TCC) %>%
      summarise(details = str_c(overview, collapse = ", ")) %>%
      arrange(desc(year))
  })
  
  byyear_df <- reactive({
    req(input$databy_tcc == "Year")
    
    geopko %>%
      mutate_at(vars(c(
        no.troops, no.tcc, longitude, latitude,
        unmo.dummy, unpol.dummy
      )), as.numeric) %>%
      mutate(hq = as.factor(hq)) %>%
      dplyr::select(
        source, mission, year, month, MonthName,
        no.troops, nameoftcc_1:notroopspertcc_17
      ) %>%
      group_by(source, mission, year, month, MonthName) %>%
      mutate(Total.troops = sum(no.troops, na.rm = T)) %>%
      ungroup() %>%
      pivot_longer(c(nameoftcc_1:notroopspertcc_17), names_to = c(".value", "TCC_id"), names_sep = "_") %>%
      filter(!is.na(nameoftcc)) %>%
      mutate_at(vars(notroopspertcc), as.numeric) %>%
      dplyr::select(-TCC_id) %>%
      mutate(nameoftcc = case_when(
        nameoftcc == "Cote D'ivoire" ~ "Ivory Coast",
        TRUE ~ as.character(nameoftcc)
      )) %>%
      group_by(source, mission, year, month, Total.troops, nameoftcc) %>%
      summarise(total.each.tcc = as.character(sum(notroopspertcc, na.rm = TRUE))) %>%
      add_count(source, name = "No.TCC") %>%
      mutate(
        total.each.tcc = ifelse(total.each.tcc == "0", "size unknown", total.each.tcc),
        overview = paste0(nameoftcc, " (", total.each.tcc, ")")
      ) %>%
      dplyr::select(-nameoftcc, -total.each.tcc) %>%
      group_by(source, mission, year, month, Total.troops, No.TCC) %>%
      summarise(byyear.overview = str_c(overview, collapse = ", ")) %>%
      arrange(desc(year)) %>%
      group_by(mission, year) %>%
      mutate(
        min.troops = as.character(min(Total.troops)),
        max.troops = as.character(max(Total.troops)),
        ave.troops = as.character(round(mean(Total.troops)))
      ) %>%
      group_by(mission, year) %>%
      arrange(desc(No.TCC)) %>%
      dplyr::slice(1) %>%
      mutate(
        ave.troops = ifelse(is.na(ave.troops), "Unknown", ave.troops),
        min.troops = ifelse(is.na(min.troops), "Unknown", min.troops),
        max.troops = ifelse(is.na(max.troops), "Unknown", max.troops)
      ) %>%
      dplyr::select(mission, year, No.TCC, byyear.overview, min.troops, max.troops, ave.troops)
  })
  
  
  
  output$tcc_table <- DT::renderDataTable(
    {
      req(input$databy_tcc)
      if (input$databy_tcc == "Deployment map") {
        DT::datatable(bymap_df(),
                      colnames = c(
                        "Source map", "Mission", "Year", "Month",
                        "Total Troop Count", "Number of TCCs",
                        "Details"
                      ),
                      rownames = FALSE
        )
      }
      else if (input$databy_tcc == "Year") {
        DT::datatable(byyear_df(),
                      colnames = c(
                        "Mission", "Year", "Number of TCCs", "Details",
                        "Min. Troop Count", "Max. Troop Count", "Mean Troop Count"
                      ),
                      rownames = FALSE
        )
      }
    },
    height = 450
  )
  
  #### deployment maps####
  
  # creating list of sf objects to download
  
  sfdf <- reactive({
    req(input$mission_map)
    cclist3 %>% filter(mission %in% input$mission_map)
  })
  
  
  observeEvent(input$mission_map, {
    updateSelectInput(session, "timepoint_map",
                      choices = unique(map_df$joined_date[map_df$mission == input$mission_map])
    )
  })
  
  map_df_temp <- reactive({
    req(input$mission_map)
    req(input$timepoint_map)
    
    map_df %>%
      filter(mission %in% input$mission_map) %>%
      filter(joined_date %in% input$timepoint_map)
  })
  
  size_for_nrow1 <- reactive({
    req(map_df_temp())
    if (NROW(map_df_temp()) == 1) {
      map_df_temp() %>%
        pull(no.troops) %>%
        `^`^(1 / 3)
    }
  })
  # map_zero <- reactive({
  #   map_df_temp() %>% filter(No.troops==0, No.TCC==0)
  # })
  
  output$basecountries <- renderText({
    unique_country <- unique(map_df_temp()$country)
    country_list(unique_country)
  })
  
  
  UNMO_df_temp <- reactive({
    req(input$MO_map)
    map_df_temp() %>% filter(unmo.dummy == 1)
  })
  
  UNPOL_df_temp <- reactive({
    req(input$UNPOL_map)
    map_df_temp() %>% filter(unpol.dummy == 1)
  })
  
  SHQ_df_temp <- reactive({
    req(input$SHQ_map)
    
    map_df_temp() %>% filter(hq == "2")
  })
  
  MHQ_df_temp <- reactive({
    req(input$MHQ_map)
    map_df_temp() %>% filter(hq == "3")
  })
  
  maplist <- reactive({
    sfdf() %>% pull(iso3c)
  })
  
  # mapshapefiles <- reactive({
  #   gadm_sf_loadCountries(c(paste(maplist())), level=1)
  # })
  
  # max_no_tcc <- reactive({
  #   map_df_temp() %>% mutate(No.TCC=ifelse(is.na(No.TCC), 0, No.TCC))
  # })
  
  # g <- guide_legend("title")
  
  output$depmap <- renderPlot(
    {
      req(input$mission_map)
      req(input$timepoint_map)
      input$MHQ_map
      input$SHQ_map
      input$MO_map
      input$UNPOL_map
      
      #mapshapefiles <- gadm_sf_loadCountries(c(maplist()), level = 1)
      
      mapshapefiles <- ne_countries(scale = "medium", returnclass = "sf") %>%
        filter(admin != "Antarctica")
      
      
      max_no_tcc <- map_df_temp() %>% mutate(no.tcc = ifelse(is.na(no.tcc), 0, no.tcc))
      
      p <- ggplot() +
        geom_sf(data = mapshapefiles, fill = "grey92") +
        theme_void() +
        labs(
          title = paste(map_df_temp()$mission, ": ", map_df_temp()$timepoint),
          caption = "Source: Geo-PKO v2.3\n"
        ) +
        geom_blank() +
        geom_point(
          data = map_df_temp(),
          aes(x = longitude, y = latitude, shape = "Blank", color = "Blank"),
          size = 2, stroke = 0.7, fill = "grey60"
        ) +
        scale_shape_manual(
          values = c("Blank" = 22),
          labels = c("Blank" = "Mission sites"),
          name = ""
        ) +
        scale_color_manual(
          values = c("Blank" = "blue"),
          labels = c("Blank" = "Mission sites"),
          name = ""
        ) +
        new_scale_color() +
        new_scale("shape")
      
      if (nrow(map_df_temp()) > 1) {
        if (max(map_df_temp()$no.troops, na.rm = TRUE) > 0) {
          p <- p +
            geom_point(
              data = map_df_temp() %>% filter(!is.na(no.troops & no.tcc)),
              aes(x = longitude, y = latitude, size = no.troops, color = as.integer(no.tcc)),
              shape = 20, alpha = 0.8
            ) +
            scale_size_binned(name = "Size of deployment", range = c(2, 16)) +
            {
              if (max(max_no_tcc$no.tcc) <= 4) {
                scale_color_continuous(
                  low = "thistle3", high = "darkred",
                  guide = "colorbar", name = "No. of Troop-\nContributing Countries",
                  breaks = c(0, 1, 2, 3, 4),
                  limits = c(0, 4)
                )
              }
            } +
            {
              if (max(max_no_tcc$no.tcc) > 4) {
                scale_color_continuous(
                  low = "thistle3", high = "darkred",
                  guide = "colorbar", name = "No. of Troop-\nContributing Countries",
                  breaks = breaks_pretty()
                )
              }
            }
        }
        else {
          p <- p +
            geom_point(
              data = map_df_temp() %>% filter(!is.na(no.troops & no.tcc)),
              aes(x = longitude, y = latitude, color = as.integer(no.tcc)),
              shape = 20, alpha = 0.8
            ) +
            {
              if (max(max_no_tcc$no.tcc) <= 4) {
                scale_color_continuous(
                  low = "thistle3", high = "darkred",
                  guide = "colorbar", name = "No. of Troop-\nContributing Countries",
                  breaks = c(0, 1, 2, 3, 4),
                  limits = c(0, 4)
                )
              }
            } +
            {
              if (max(max_no_tcc$no.tcc) > 4) {
                scale_color_continuous(
                  low = "thistle3", high = "darkred",
                  guide = "colorbar", name = "No. of Troop-\nContributing Countries",
                  breaks = breaks_pretty()
                )
              }
            }
        }
      }
      if (nrow(map_df_temp()) == 1) {
        p <- p + geom_point(
          data = map_df_temp() %>% filter(!is.na(no.troops & no.tcc)),
          aes(x = longitude, y = latitude, color = as.integer(no.tcc), size = "Custom"),
          shape = 20, alpha = 0.8
        ) +
          scale_size_manual(
            name = "Size of deployment", values = c("Custom" = round((max(map_df_temp()$no.troops))^(1 / 3))),
            labels = c("Custom" = paste(max(map_df_temp()$no.troops)))
          ) +
          {
            if (max(max_no_tcc$no.tcc) <= 4) {
              scale_color_continuous(
                low = "thistle3", high = "darkred",
                guide = "colorbar", name = "No. of Troop-\nContributing Countries",
                breaks = c(0, 1, 2, 3, 4),
                limits = c(0, 4)
              )
            }
          } +
          {
            if (max(max_no_tcc$no.tcc) > 4) {
              scale_color_continuous(
                low = "thistle3", high = "darkred",
                guide = "colorbar", name = "No. of Troop-\nContributing Countries",
                breaks = breaks_pretty()
              )
            }
          }
      }
      
      
      p <- p +
        new_scale_color() +
        scale_shape_manual(
          values = c(
            "SHQ" = 3,
            "UNMO" = 24,
            "UNPOL" = 23
          ),
          labels = c("SHQ" = "Sector HQ", "UNMO" = "Military Observers", "UNPOL" = "UN Police"),
          name = ""
        ) +
        scale_color_manual(
          values = c(
            "SHQ" = "orange",
            "UNMO" = "darkblue",
            "UNPOL" = "darkgreen"
          ),
          labels = c("SHQ" = "Sector HQ", "UNMO" = "Military Observers", "UNPOL" = "UN Police"),
          name = ""
        )
      
      
      if (input$MHQ_map) {
        if (length(MHQ_df_temp()$location) > 0) {
          p <- p + geom_point(
            data = MHQ_df_temp(),
            aes(x = longitude, y = latitude, shape = "HQ"),
            shape = 4, color = "red", size = 6
          ) +
            geom_label_repel(
              data = MHQ_df_temp(),
              aes(x = longitude, y = latitude, label = paste0("Mission HQ: ", location)),
              box.padding = 2,
              size = 3,
              fill = alpha(c("white"), 0.7)
            )
        }
        else {
          p <- p + labs(subtitle = "Mission HQs not available for this time period. Please deselect the option.")
        }
      }
      
      if (input$SHQ_map) {
        if (length(SHQ_df_temp()$location) > 0) {
          p <- p + geom_point(
            data = map_df_temp() %>% filter(hq == "2"),
            aes(x = longitude, y = latitude, shape = "SHQ", color = "SHQ"), size = 5
          )
        }
        else {
          p <- p + labs(subtitle = "Sector HQs not available for this time period. Please deselect the option.")
        }
      }
      
      if (input$MO_map) {
        if (length(UNMO_df_temp()$location) > 0) {
          p <- p + geom_point(
            data = map_df_temp() %>% filter(unmo.dummy == 1),
            aes(x = longitude, y = latitude, shape = "UNMO", color = "UNMO"),
            # color="darkblue",
            position = position_jitter(),
            size = 3
          )
        }
        else {
          p <- p + labs(subtitle = "UNMO not found. Please deselect the option.")
        }
      }
      
      if (input$UNPOL_map) {
        if (length(UNPOL_df_temp()$location) > 0) {
          p <- p + geom_point(
            data = map_df_temp() %>% filter(unpol.dummy == 1),
            aes(x = longitude, y = latitude, shape = "UNPOL", color = "UNPOL"),
            position = position_jitter(),
            size = 4
          )
        }
        else {
          p <- p + labs(subtitle = "UNPOL not found. Please deselect the option.")
        }
      }
      
      p <- p +
        theme(
          plot.subtitle = element_text(color = "red"),
          plot.title = element_text(face = "bold", hjust = 0),
          #      plot.caption.position = "plot",
          plot.caption = element_text(hjust = 1),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.box = "vertical"
        )
      
      print(p)
    },
    height = 600
  )
  
  #### map_df_detail####
  
  typecheck_df <- reactive({
    req(input$mission_map)
    req(input$timepoint_map)
    
    map_df_temp() %>%
      tibble::rowid_to_column("ID") %>%
      dplyr::select(
        ID, location, no.troops, no.tcc, rpf:uav, other.type, -rpf.no,
        -inf.no, -fpu.no, -res.no, -fp.no
      ) %>%
      mutate_at(vars(`inf`:`other.type`), as.numeric) %>%
      rowwise() %>%
      mutate(typecheck_var = sum(c_across(`inf`:`other.type`))) %>%
      filter(typecheck_var > 0)
  })
  
  static_map_details <- reactive({
    if (length(typecheck_df() > 0)) {
      map_df_temp() %>%
        tibble::rowid_to_column("ID") %>%
        dplyr::select(
          ID, location, no.troops, no.tcc, rpf:uav, other.type, -rpf.no,
          -inf.no, -fpu.no, -res.no, -fp.no, troop.type
        ) %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(5:23, names_to = "trooptypes", values_to = "binary") %>%
        filter(!(troop.type %in% "0"), binary != "0") %>%
        mutate(trooptypes = case_when(
          trooptypes == "sf" ~ "Special Forces",
          trooptypes == "inf" ~ "Infantry",
          trooptypes == "he.sup" ~ "Helicopter Support",
          trooptypes == "avia" ~ "Aviation",
          trooptypes == "mp" ~ "Military Police",
          trooptypes == "uav" ~ "Unmanned Aerial Vehicles",
          trooptypes == "recon" ~ "Reconnaissance",
          trooptypes == "maint" ~ "Maintenance",
          trooptypes == "med" ~ "Medical",
          trooptypes == "eng" ~ "Engineer",
          trooptypes == "fpu" ~ "Formed Police Unit",
          trooptypes == "fp" ~ "Force Protection",
          trooptypes == "riv" ~ "Riverine",
          trooptypes == "sig" ~ "Signal",
          trooptypes == "trans" ~ "Transport",
          trooptypes == "other.type" ~ "Others",
          trooptypes == "eng" ~ "Engineer",
          trooptypes == "rpf" ~ "Regional Protection Force",
          trooptypes == "demining" ~ "Demining",
          TRUE ~ as.character(trooptypes)
        )) %>%
        group_by(ID, location, no.troops, no.tcc) %>%
        summarize(Troop.Compo = str_c(trooptypes, collapse = ", "),
                  .groups = 'drop') %>%
        mutate(
          no.tcc = ifelse(is.na(no.tcc), "Unknown", no.tcc),
          no.troops = ifelse(is.na(no.troops), "Unknown", no.troops)
        ) %>%
        dplyr::select(-ID) -> details1
      
      map_df_temp() %>%
        tibble::rowid_to_column("ID") %>%
        dplyr::select(
          ID, location, no.troops, no.tcc, rpf:uav, other.type, -rpf.no,
          -inf.no, -fpu.no, -res.no, -fp.no, troop.type
        ) %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(5:23, names_to = "trooptypes", values_to = "binary") %>%
        filter(troop.type %in% "0") %>%
        group_by(ID, location, no.troops, no.tcc) %>%
        summarize(Troop.Compo = "Data on troop types not available for this location", 
                  .groups = 'drop') %>%
        mutate(
          no.tcc = ifelse(is.na(no.tcc), "Unknown", no.tcc),
          no.troops = ifelse(is.na(no.troops), "Unknown", no.troops)
        ) %>%
        dplyr::select(-ID) -> details2
      
      rbind(details1, details2)
    }
    else {
      map_df_temp() %>%
        dplyr::select(location, no.troops, no.tcc) %>%
        mutate(Troop.Compo = "Data on troop types not available for this location") %>%
        mutate(no.tcc = ifelse(is.na(no.tcc), "Unknown", no.tcc))
    }
  })
  
  output$map_df_details <- renderDataTable({
    DT::datatable(static_map_details(),
                  colnames = c("Location", "No. Troops", "No. TCCs", "Troop Types"),
                  rownames = FALSE
    )
  })
  
  #### animated maps####
  anim_sf <- reactive({
    req(input$anim_map)
    cclist3 %>% filter(mission %in% input$anim_map)
  })
  
  anim_df <- eventReactive(input$go_anim, {
    req(input$go_anim)
    req(input$anim_map)
    
    map_df %>%
      filter(mission %in% input$anim_map) %>%
      group_by(mission) %>% 
      arrange(ordered_yrm)
  })
  
  
  
  output$animated <- renderImage(
    {
      req(input$anim_map)
      req(input$go_anim)
      
      outfile <- tempfile(fileext = ".gif")
      
      anim_maplist <- pull(anim_sf(), iso3c)
      anim_max_no_tcc <- anim_df() %>% mutate(no.tcc = ifelse(is.na(no.tcc), 0, no.tcc))
      #anim_mapshapefiles <- gadm_sf_loadCountries(c(anim_maplist), level = 1)
      anim_mapshapefiles <- ne_countries(scale = "medium", returnclass = "sf") %>% filter(iso_a3 %in% anim_maplist)
      mission_name <- anim_df() %>% distinct(mission)
      colourCount <- max(anim_df()$no.tcc)
      getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
      
      anim_p <- ggplot() +
        geom_sf(data = anim_mapshapefiles, fill = "grey92") +
        theme_void() +
        geom_blank() +
        geom_point(
          data = anim_df(),
          aes(x = longitude, y = latitude, shape = "Blank", color = "Blank"),
          size = 2, stroke = 0.7, fill = "grey60"
        ) +
        scale_shape_manual(
          values = c("Blank" = 22),
          labels = c("Blank" = "Mission sites"),
          name = ""
        ) +
        scale_color_manual(
          values = c("Blank" = "grey44"),
          labels = c("Blank" = "Mission sites"),
          name = ""
        ) +
        new_scale_color() +
        new_scale("shape")
      if (nrow(anim_df()) > 1) {
        if (sum(anim_df()$no.troops, na.rm = TRUE) > 0) {
          anim_p <- ggplot() +
            geom_sf(data = anim_mapshapefiles) +
            theme_void() +
            geom_point(
              data = anim_df(), aes(
                x = longitude, y = latitude, size = no.troops,
                color = as.integer(no.tcc), group = ordered_yrm
              ),
              shape = 20, alpha = 0.65
            ) +
            scale_size_binned(name = "Deployment size", range = c(2, 16)) +
            {
              if (max(anim_max_no_tcc$no.tcc) <= 4) {
                list(
                  scale_color_continuous(
                    low = "thistle3", high = "darkred",
                    guide = "colorbar", name = "No. of Troop-\nContributing Countries",
                    breaks = c(1, 2, 3, 4),
                    limits = c(1, 4)
                  )
                )
              }
            } +
            {
              if (max(anim_max_no_tcc$no.tcc) > 4) {
                list(
                  scale_color_continuous(
                    low = "thistle3", high = "darkred",
                    guide = "colorbar", name = "No. of Troop-\nContributing Countries",
                    breaks = breaks_pretty()
                  )
                )
              }
            }
        }
      }
      else {
        anim_p <- anim_p +
          geom_point(
            data = anim_df() %>% filter(!is.na(no.troops & no.tcc)),
            aes(x = longitude, y = latitude, color = as.integer(no.tcc)),
            shape = 20, alpha = 0.65
          ) +
          {
            if (max(anim_max_no_tcc$no.tcc) <= 4) {
              scale_color_continuous(
                low = "thistle3", high = "darkred",
                guide = "colorbar", name = "No. of Troop-\nContributing Countries",
                breaks = c(0, 1, 2, 3, 4),
                limits = c(0, 4)
              )
            }
          } +
          {
            if (max(anim_max_no_tcc$no.tcc) > 4) {
              scale_color_continuous(
                low = "thistle3", high = "darkred",
                guide = "colorbar", name = "No. of Troop-\nContributing Countries",
                breaks = breaks_pretty()
              )
            }
          }
      }
      
      
      anim_p <- anim_p + theme(
        plot.subtitle = element_text(color = "red"),
        plot.title = element_text(face = "bold", hjust = 0),
        #      plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0.5),
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7)
      ) +
        transition_states(states = anim_df()$ordered_yrm, transition_length = 3) +
        labs(
          title = paste0(mission_name, ": ", "{closest_state}"),
          caption = "Source: Geo-PKO v2.3\n"
        ) +
        #     enter_fade() +
        #      exit_fade() +
        ease_aes('linear')
      
      anim_save("outfile.gif", animate(anim_p, fps = 5, width = 700, height = 700, res = 150))
      
      list(
        src = "outfile.gif",
        contentType = "image/gif"
      )
    },
    deleteFile = TRUE
  )
  
  #### lollipop####
  lollipop_df <- reactive({
    req(input$Lollipop_map)
    Years %>%
      filter(mission %in% input$Lollipop_map) %>%
      mutate_at(vars(c(start_date, end_date)), as.numeric)
  })
  
  height_lollipop <- reactive({
    if (nrow(lollipop_df()) < 15) {
      400
    }
    else {
      NROW(lollipop_df()) * 25 + 300
    }
  })
  
  
  output$lollipop <- renderPlot(
    {
      lolli <- ggplot(lollipop_df()) +
        geom_segment(aes(
          x = start_date, xend = end_date,
          y = fct_reorder(location, start_date),
          yend = fct_reorder(location, start_date)
        ), color = "grey") +
        geom_point(aes(x = end_date, y = location),
                   colour = rgb(0.9, 0.3, 0.1, 0.9), size = 3.5
        ) +
        geom_point(aes(x = start_date, y = location),
                   colour = rgb(1.0, 0.6, 0.1, 0.7), size = 3
        ) +
        scale_x_continuous(
          breaks =
            seq(1993, 2024, 1)
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.length.x = unit(0.1, "cm"),
          panel.grid.minor.x = element_blank(),
          panel.spacing.x = unit(1, "lines")
        ) +
        xlab("Years") +
        ylab("") + # title already mentions locations, so no need for name
        labs(
          title = paste0(unique(lollipop_df()$mission), ": ", min(lollipop_df()$start_date, na.rm = TRUE), "-", max(lollipop_df()$end_date, na.rm = TRUE)),
          caption = "Data: Geo-PKO v2.3"
        )
      lolli
    },
    height = height_lollipop
  )
}



shinyApp(ui = ui, server = server)
