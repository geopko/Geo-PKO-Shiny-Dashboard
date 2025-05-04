#Scripts for data transformation for v.2.3 update

library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)
library(readr)
library(readxl)
library(lubridate)

#### dataset with ISO codes for mapmaker ####
geopko <- read_xlsx("data/Geo_PKO_v.2.3_location_map.xlsx")

# setting class of numeric variables
map_df <- geopko %>% 
  mutate_at(vars(c(no.troops, no.tcc, longitude, latitude,
                   unmo.dummy, unmo.dummy)), as.numeric) %>% 
  mutate(hq=as.factor(hq))


# Load the old dataset (v2.1) to get ISO3 country codes
df_iso <- read_xlsx("data/Geo_PKO_v.2.1_ISO3.xlsx") %>%
  dplyr::select(country, iso3c) %>%
  distinct()

# Create MonthName and timepoint, and join ISO3 codes
map_df <- map_df %>%
  mutate(
    MonthName = month.name[month],
    timepoint = paste(year, MonthName),
    joined_date = paste(year, MonthName, sep = ": ")
  ) %>%
  left_join(df_iso, by = "country")

# Reorder columns to put timepoint between mission and year
cols <- names(map_df)
mission_index <- which(cols == "mission")

map_df <- map_df %>%
  dplyr::select(
    all_of(cols[1:mission_index]),
    timepoint,
    all_of(cols[(mission_index + 1):length(cols)])
  )


write_xlsx(map_df, "Geo_PKO_v.2.3_ISO3.xlsx")
write_csv(map_df, "Geo_PKO_v.2.3_ISO3.csv")


#### map files for leaflet ####

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
  dplyr::select(mission, year, country, location, 
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

write_csv(FrontmapData, "FrontMap_v.2.3.csv")
write_xlsx(FrontmapData, "FrontMap_v.2.3.xlsx")


#### TCC dataframe (second map) ####
TCCmapData<-geopko2 %>% dplyr::select(source:location, latitude, longitude,
                               no.tcc:notroopspertcc_17, hq) %>% 
  pivot_longer(nameoftcc_1:notroopspertcc_17,
               names_to=c(".value", "TCC_id"),
               names_sep="_") %>%
  #dropping empty no.troops cells 
  filter(notroopspertcc != "NA") %>%
  mutate_at(vars(notroopspertcc), as.numeric) %>%
  mutate(nameoftcc = tolower(nameoftcc)) %>% 
  group_by(mission, year, location, latitude, longitude, nameoftcc) %>%
  summarise(count.per.tcc.year=as.character(max(notroopspertcc), na.rm = TRUE)) %>%
  mutate(nameoftcc = str_to_title(nameoftcc),
         nameoftcc = case_when(nameoftcc == "Uk" ~ "UK", 
                               nameoftcc == "U.k." ~ "UK",
                               nameoftcc == "Cote D'ivoire" ~ "Cote d'Ivoire", 
                               nameoftcc == "Usa" ~ "USA", 
                               nameoftcc == "Caricom" ~ "CARICOM",
                               nameoftcc == "Korea (Rok)" ~ "Korea (ROK)",
                               nameoftcc == "Unknown" ~ "TCC name unknown",
                               TRUE ~ nameoftcc)) %>%
  mutate(count.per.tcc.year=ifelse(is.na(count.per.tcc.year), "size unknown", count.per.tcc.year),
         single.tcc=paste0(nameoftcc, " (",count.per.tcc.year,(")"))) %>%
  add_count(year, location, name="No.TCC")%>%
  group_by(mission, year, location, latitude, longitude, No.TCC) %>%
  summarise(year.overview = str_c(single.tcc, collapse=", ")) 

write_csv(TCCmapData, "data/TCCMap_v.2.3.csv")
write_xlsx(TCCmapData, "data/TCCMap_v.2.3.xlsx")

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
  dplyr::select(mission, year, location, latitude, longitude, 
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

write_csv(TTmapData, "data/TTMap_v.2.3.csv")
write_xlsx(TTmapData, "data/TTMap_v.2.3.xlsx")
