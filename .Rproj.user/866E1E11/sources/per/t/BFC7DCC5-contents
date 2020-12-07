#Scripts for data transformation 

library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)
library(readr)
library(readxl)
library(lubridate)

#### file for mapmaker ####
geopko <- read_xlsx("Geo_PKO_v.2.0.xlsx", col_types = c("text"))

map_df <- geopko %>% 
  mutate_at(vars(c(no.troops, no.tcc, longitude, latitude,
                   unmo.dummy, unmo.dummy)), as.numeric) %>% 
  mutate(hq=as.factor(hq))

#appending ISO code to the dataset
ISO_df <- codelist %>% select(cown, iso3c) %>% mutate_all(as.character)

map_df <- left_join(map_df, ISO_df, by=c("cow_code"="cown"))

map_df <- map_df %>%
  mutate(iso3c = case_when(country == "Ethiopia/Eritrea" ~ "ETH, ERI",
                           country == "Kosovo" ~ "XKO",
                           mission == "UNFICYP" ~ "CYP, XNC",
                           country == "Mauritania" ~ "MRT", 
                           country == "Western Sahara" ~ "ESH",
                           TRUE ~ as.character(iso3c)),
         Month = as.numeric(month),
         MonthName = as.character(lubridate::month(Month, label = TRUE, abbr = FALSE))) %>%
  unite(joined_date, c("year","MonthName"), sep=": ", remove=FALSE) %>%
  unite(timepoint, c("year","MonthName"), sep=" ", remove=FALSE)

write_excel_csv(map_df, "Geo_PKO_v2.0_ISO3.csv")

#### file for leaflet ####

geopko2 <-geopko %>% mutate_at(vars(he.sup, inf.no, res.no, 
                                    avia,longitude, latitude), as.numeric) %>%
  mutate(NoTroops = as.numeric(no.troops), 
         UNPOL = as.numeric(unpol.dummy),
         UNMO = as.numeric(unmo.dummy),
         Reserve = as.numeric(res.no),
         Infantry = as.numeric(inf.no),
         Med=as.numeric(med))

geopko2$Av<- (geopko2$avia + geopko2$he.sup)

FrontmapData <- geopko2 %>% 
  select(mission, year, country, location, 
         latitude, longitude, Infantry, 
         NoTroops, Reserve, hq, UNPOL, Med, Av, UNMO) %>%
  group_by(mission, year, location, country) %>% 
  mutate(Av = max(Av, na.rm=TRUE), 
         Med = max(Med, na.rm=TRUE),
         Infantry = as.integer(mean(Infantry, na.rm=TRUE)),
         Reserve = as.integer(mean(Reserve, na.rm=TRUE)),
         UNPOL = max(UNPOL, na.rm=TRUE),
         UNMO = max(UNMO, na.rm=TRUE),
         ave.no.troops = as.integer(mean(NoTroops, na.rm=TRUE))) %>% 
  distinct() 

write_excel_csv(FrontmapData, "FrontMap.csv")

#### TCC dataframe (second map) ####
TCCmapData<-geopko2 %>% select(source:location, latitude, longitude,
                               no.tcc:notroopspertcc_17, hq) %>% 
  pivot_longer(nameoftcc_1:notroopspertcc_17,
               names_to=c(".value", "TCC_id"),
               names_sep="_") %>%
  filter(!is.na(nameoftcc)) %>%  #dropping empty tcc name cells
  mutate_at(vars(notroopspertcc), as.numeric) %>%
  group_by(mission, year, location, latitude, longitude, nameoftcc) %>%
  summarise(count.per.tcc.year=as.character(max(notroopspertcc))) %>%
  mutate(count.per.tcc.year=ifelse(is.na(count.per.tcc.year), "unknown", count.per.tcc.year),
         single.tcc=paste0(nameoftcc, " (",count.per.tcc.year,(")"))) %>%
  add_count(year, location, name="No.TCC")%>%
  group_by(mission, year, location, latitude, longitude, No.TCC) %>%
  summarise(year.overview = str_c(single.tcc, collapse=", "))

write_excel_csv(TCCmapData, "TCCMap.csv")

####Troop type####
geopko2 <-geopko %>% 
  mutate_at(vars(inf.no, res.no, longitude, latitude, eng:mp), as.numeric) %>%
  mutate(NoTroops = as.numeric(no.troops), 
         UNPOL = as.numeric(unpol.dummy),
         UNMO = as.numeric(unmo.dummy),
         Reserve = as.numeric(res.no),
         Infantry = as.numeric(inf.no))
geopko2$av<- (geopko2$avia + geopko2$he.sup)

geopko2$av<- as.numeric(geopko2$av)

TTmapData <- geopko2 %>% 
  select(mission, year, location, latitude, longitude, 
         Infantry, eng, med, sig, av, riv, maint, trans) %>%
  group_by(mission, year, location)%>%  
  mutate(Infantry = as.integer(mean(Infantry, na.rm=TRUE)),
         eng= sum(eng),
         med= sum(med),
         sig= sum(sig),
         av= sum(av),
         riv= sum(riv),
         maint= sum(maint),
         trans= sum(trans)) %>%  
  distinct()%>%
  drop_na(Infantry)

write_excel_csv(TTmapData, "TTMap.csv")

library(readxl)
library(readr)
geopko2 <- readxl::read_xlsx("Geo_PKO_v.2.0.xlsx", col_types="text")

write_excel_csv(geopko2, "Geo_PKO_v.2.0.csv")

geopko3 <- readr::read_csv("Geo_PKO_v.2.0.csv")
