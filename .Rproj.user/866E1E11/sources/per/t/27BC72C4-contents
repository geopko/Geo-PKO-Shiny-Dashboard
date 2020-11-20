#Scripts for data transformation 

library(dplyr)
library(stringr)
library(countrycode)

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
                           Mission == "UNFICYP" ~ "CYP, XNC",
                           TRUE ~ as.character(iso3c)),
        month = as.numeric(month),
         MonthName = as.character(month(month, label = TRUE, abbr = FALSE))) %>%
  unite(joined_date, c("year","MonthName"), sep=": ", remove=FALSE) %>%
  unite(timepoint, c("year","MonthName"), sep=" ", remove=FALSE)

write.csv(map_df2, "Geo_PKO_v2_ISO3.csv")
