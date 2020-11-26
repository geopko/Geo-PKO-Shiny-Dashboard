#Scripts for data transformation 

library(dplyr)
library(stringr)
library(countrycode)

#### file for mapmaker ####
geopko <- readr::read_csv("Geo_PKO_v2.csv", col_types = cols(.default="c"),
                          locale=readr::locale(encoding="latin1"))

map_df <- geopko %>% 
  mutate_at(vars(c(No.troops, No.TCC, longitude, latitude,
                   UNMO.dummy, UNPOL.dummy)), as.numeric) %>% 
  mutate(HQ=as.factor(HQ))

#appending ISO code to the dataset
ISO_df <- codelist %>% select(cown, iso3c) %>% mutate_all(as.character)

map_df <- left_join(map_df, ISO_df, by=c("cow_code"="cown"))

map_df <- map_df %>%
  mutate(iso3c = case_when(country == "Ethiopia/Eritrea" ~ "ETH, ERI",
                           country == "Kosovo" ~ "XKO",
                           Mission == "UNFICYP" ~ "CYP, XNC",country == "Mauritania" ~ "MRT", country == "Western Sahara" ~ "ESH",
                           TRUE ~ as.character(iso3c)),
        month = as.numeric(month),
         MonthName = as.character(month(month, label = TRUE, abbr = FALSE))) %>%
  unite(joined_date, c("year","MonthName"), sep=": ", remove=FALSE) %>%
  unite(timepoint, c("year","MonthName"), sep=" ", remove=FALSE)

write.csv(map_df2, "Geo_PKO_v2_ISO3.csv")

#### file for leaflet ####

geopko2 <-geopko %>% mutate_at(vars(HeSup, Inf_No, RES_No, Avia, HeSup, longitude, latitude), as.numeric) %>%
  mutate(NoTroops = as.numeric(No.troops), 
         UNPOL = as.numeric(UNPOL.dummy),
         UNMO = as.numeric(UNMO.dummy),
         Reserve = as.numeric(RES_No),
         Infantry = as.numeric(Inf_No))

geopko2$Av<- (geopko2$Avia + geopko2$HeSup)

FrontmapData <- geopko2 %>% 
  select(Mission, year, country, location, 
         latitude, longitude, Infantry, 
         NoTroops, Reserve, HQ, UNPOL, Med, Av, UNMO) %>%
  group_by(Mission, year, location, country) %>% 
  mutate(Av = max(Av, na.rm=TRUE), 
         Med = max(Med, na.rm=TRUE),
         Infantry = as.integer(mean(Infantry, na.rm=TRUE)),
         Reserve = as.integer(mean(Reserve, na.rm=TRUE)),
         UNPOL = max(UNPOL, na.rm=TRUE),
         UNMO = max(UNMO, na.rm=TRUE),
         ave.no.troops = as.integer(mean(NoTroops, na.rm=TRUE))) %>% 
  distinct() 

#### TCC dataframe (second map) ####
TCCmapData<-geopko2 %>% select(Source:location, latitude, longitude,
                               No.TCC:notroopspertcc_17, HQ) %>% 
  pivot_longer(nameoftcc_1:notroopspertcc_17,
               names_to=c(".value", "TCC_id"),
               names_sep="_") %>%
  filter(!is.na(nameoftcc)) %>%  #dropping empty tcc name cells
  mutate_at(vars(notroopspertcc), as.numeric) %>%
  group_by(Mission, year, location, latitude, longitude, nameoftcc) %>%
  summarise(count.per.tcc.year=as.character(max(notroopspertcc))) %>%
  mutate(count.per.tcc.year=ifelse(is.na(count.per.tcc.year), "unknown", count.per.tcc.year),
         single.tcc=paste0(nameoftcc, " (",count.per.tcc.year,(")"))) %>%
  add_count(year, location, name="No.TCC")%>%
  group_by(Mission, year, location, latitude, longitude, No.TCC) %>%
  summarise(year.overview = str_c(single.tcc, collapse=", "))