library(dplyr)
library(tidyr)
library(stringr)

tcc_df <- geopko %>% select(Source:Location, Latitude, Longitude, 
                            No.TCC:notroopspertcc_17)
tcc_df2 <- tcc_df %>% pivot_longer(nameoftcc_1:notroopspertcc_17, 
                        names_to=c(".value", "TCC_id"),
                        names_sep="_") %>% 
  filter(!is.na(nameoftcc)) %>% 
  mutate_at(vars(notroopspertcc), as.numeric) %>% 
  group_by(Source, Mission, Year, Location, Latitude, Longitude, No.TCC, nameoftcc) %>% 
  summarise(count.per.tcc.year=max(notroopspertcc, na.rm=TRUE)) %>% 
  mutate(single.tcc=paste0(nameoftcc, " (",count.per.tcc.year,(")"))) %>% 
  group_by(Source, Mission, Year, Location, Latitude, Longitude, No.TCC) %>% 
  summarise(year.overview = str_c(single.tcc, collapse=", "))

tcc_df3 <- tcc_df %>% pivot_longer(nameoftcc_1:notroopspertcc_17, 
                                   names_to=c(".value", "TCC_id"),
                                   names_sep="_") %>% 
  filter(!is.na(nameoftcc)) %>% 
  mutate_at(vars(notroopspertcc), as.numeric) %>% 
  group_by(Mission, Year, Location, Latitude, Longitude, nameoftcc) %>% 
  summarise(count.per.tcc.year=as.character(max(notroopspertcc))) %>% 
  mutate(count.per.tcc.year=ifelse(is.na(count.per.tcc.year), "unknown", count.per.tcc.year),
         single.tcc=paste0(nameoftcc, " (",count.per.tcc.year,(")"))) %>% 
  add_count(Year, Location, name="No.TCC")%>% 
  group_by(Mission, Year, Location, Latitude, Longitude, No.TCC) %>% 
  summarise(year.overview = str_c(single.tcc, collapse=", "))

write.csv(tcc_df3, "tcc_dataframe.csv")
              