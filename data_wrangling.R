#Scripts for data transformation 

library(dplyr)
library(tidyr)
library(stringr)
library(countrycode)
library(readr)
library(readxl)
library(lubridate)

#### dataset with ISO codes for mapmaker ####
geopko_raw <- read_xlsx("data/Geo_PKO_v.2.1.xlsx", col_types = c("text"))
missingyears <- read_xlsx("data/missingyears.xlsx", col_types = c("text"))

# filtering for any republished maps to remove incorrect data
geopkofilter <- geopko_raw %>%
  filter(!str_detect(source, 'republished'))

# merging with missing years data
geopko <- geopkofilter %>%
  full_join(geopkofilter, missingyears,
  by = c("mission", "year", "location"),
  suffix = c("", ""))

# setting class of numeric variables
map_df <- geopko %>% 
  mutate_at(vars(c(no.troops, no.tcc, longitude, latitude,
                   unmo.dummy, unmo.dummy)), as.numeric) %>% 
  mutate(hq=as.factor(hq))

####appending ISO code to the dataset for the static map makers####
ISO_df <- codelist %>% select(cown, iso3c) %>% mutate_all(as.character)

map_df <- left_join(map_df, ISO_df, by=c("cow_code"="cown"))

# Make sure the system locale is English before performing this step. 
# Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8')
# Sys.setenv(LANG = "en_US.UTF-8")

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

write_excel_csv(map_df, "data/Geo_PKO_v.2.1_ISO3.csv") # saving files
writexl::write_xlsx(map_df, "data/Geo_PKO_v.2.1_ISO3.xlsx")

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
write_excel_csv(FrontmapData, "FrontMap.xlsx")

#### TCC dataframe (second map) ####
TCCmapData<-geopko2 %>% select(source:location, latitude, longitude,
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

write_excel_csv(TCCmapData, "data/TCCMap.csv")
writexl::write_xlsx(TCCmapData, "data/TCCMap.xlsx")

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

write_excel_csv(TTmapData, "data/TTMap.csv")
write_excel_csv(TTmapData, "data/TTMap.xlsx")

### OLD SCRIPT SEGMENTS, FOR TESTING PURPOSE 
library(readxl)
library(readr)
geopko2 <- readxl::read_xlsx("Geo_PKO_v.2.1.xlsx", col_types="text")

write_excel_csv(geopko2, "Geo_PKO_v.2.1.csv")

geopko3 <- readr::read_csv("Geo_PKO_v.2.1.csv")

geopko %>% filter(mission=="MINUSMA") %>% 
  tibble::rowid_to_column("ID") %>%
  select(
    ID, location, no.troops, no.tcc, rpf:uav, other.type, -rpf.no,
    -inf.no, -fpu.no, -res.no, -fp.no
  ) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(5:23, names_to = "trooptypes", values_to = "binary") %>%
  filter(binary == 1) %>%
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
  summarize(Troop.Compo = str_c(trooptypes, collapse = ", ")) %>%
  ungroup() %>%
  mutate(
    no.tcc = ifelse(is.na(no.tcc), "Unknown", no.tcc),
    no.troops = ifelse(is.na(no.troops), "Unknown", no.troops)
  ) %>%
  select(-ID) -> test1


geopko %>% filter(mission=="MINUSMA") %>% 
  tibble::rowid_to_column("ID") %>%
  select(
    ID, location, no.troops, no.tcc, rpf:uav, other.type, -rpf.no,
    -inf.no, -fpu.no, -res.no, -fp.no
  ) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(5:23, names_to = "trooptypes", values_to = "binary") %>%
  filter(binary != 1) %>%
  group_by(ID, location, no.troops, no.tcc) %>%
  summarize(Troop.Compo = "Data on troop types not available for this location") %>%
  ungroup() %>%
  mutate(
    no.tcc = ifelse(is.na(no.tcc), "Unknown", no.tcc),
    no.troops = ifelse(is.na(no.troops), "Unknown", no.troops)
  ) %>%
  select(-ID) -> details2
