# import data

geopko <- read_csv("geopko.csv", col_types = cols(No.TCC = col_number(), 
                                                  No.troops = col_number(),
                                                  month = col_number()))

# data prep TCC
tcc_df <- geopko %>% select(Source, Mission, year, month, No.troops, 16:44) %>% 
  group_by(Source, Mission, year, month) %>% mutate(Total.troops=sum(No.troops)) %>% ungroup() %>%
  select(1:4, 35, 5:34, -No.troops)

colnames(tcc_df) <- sub("name.of.TCC", "nameofTCC_", colnames(tcc_df))
colnames(tcc_df) <- sub("No.troops.per.TCC", "notroopsperTCC_", colnames(tcc_df))
tcc_df <- tcc_df %>% mutate_at(vars(starts_with("notroopsperTCC")), as.character)
tcc_df <- tcc_df %>% mutate_at(vars(starts_with("nameofTCC")), as.character)


#by year code

output <- tcc_df %>% filter(Mission=="MINUSMA") %>%
    pivot_longer(c(7:34), names_to=c(".value", "TCC_id"), names_sep="_")%>%
    mutate(notroopsperTCC=as.numeric(notroopsperTCC)) %>%
    select(-TCC_id, -No.TCC) %>% 
    mutate(notroopsperTCC=as.numeric(notroopsperTCC)) %>%
    group_by(Source, Mission, year, month, Total.troops, nameofTCC)%>%
    summarise(total.tcc=sum(notroopsperTCC)) %>% 
    add_count(Source, name="No.TCC") %>%
    mutate(overview=paste0(nameofTCC, "-", total.tcc)) %>%
    select(-nameofTCC, -total.tcc) %>%
    group_by(Source, Mission, year, month, Total.troops, No.TCC) %>%
    summarise(details=str_c(overview, collapse=", ")) %>% 
    arrange(desc(year)) %>%
    group_by(Mission, year) %>% mutate(min.troops= min(Total.troops),
                                       max.troops=max(Total.troops),
                                       ave.troops=mean(Total.troops)) %>%
    group_by(year) %>% dplyr::slice(1) %>% select(-Total.troops) 

# creating graph

p1 <- ggplot(output, aes(x=year, y=ave.troops, label=ave.troops)) + geom_line()+geom_point()+
  geom_text()+
  theme_bw()+
  labs(y="Average troop count", x="Year")+
  theme(panel.grid = element_blank()
    
  )
p1

p2 <- e_charts(output, x=year) %>% 
  e_bar(ave.troops)
p2


#map downloader

library(GADMTools)
library(countrycode)

#merge geopko with countrycode
iso2 <- iso %>% select("name", "alpha-3")

geopko2 <- left_join(geopko, iso2, by=c("country"="name"))
geopko2 <- geopko2 %>% mutate("alpha-3" = ifelse(country=="DRC", "COD",
                                               ifelse(country=="Ivory Coast", "CIV", `alpha-3`)))
write.csv(geopko2, "geopko_ccode.csv")

cclist3 <- geopko2 %>% select(Mission, `alpha-3`) %>% distinct() %>% rename(alpha3=`alpha-3`)


cclist2<- distinct(as.data.frame(geopko2$`alpha-3`))
cclist <- distinct(cclist)
ccvector <- cclist2[, 1]

downloadlist <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_",ccvector,"_1_sf.rds")
deslit <- paste0("C://Users/Nguyen Ha/Documents/DPCR/GeoPKO/GeoPKO-Shiny/TCC/Maps/gadm36_",ccvector,"_1_sf.rds")
  
for(i in 1:length(downloadlist)){
  download.file(downloadlist[i], deslit[i], mode="wb")
}

#read the file
library(sf)

maplist <- lapply(deslit, function(x){
  
  
})

nameslist <- paste0("map_",ccvector)

#changing name
names(maplist) <- nameslist

examplelist <- c("LBR","SLE")

map <- gadm_sf_loadCountries(paste0("c\\(",examplelist,"\\)", sep=","), level=1, basefile="./")

example2 <- "MRT"

map <- gadm_sf_loadCountries("CIV", level=1, basefile="./")

example4 <- c("RWA", "UGA", "CAF")

p1 <- gadm_plot(map)
p1 + geom_point(data=minurcat, aes(x=longitude, y=latitude, size=No.troops))

map2 <- ggplot() + geom_sf(data=gadm36_AGO_1_sf)
map2 + geom_sf(data=gadm36_ALA_1_sf) 
map2

minurcat <- geopko %>% filter(Mission == "MINURCAT", year==2008)

map4 <- gadm_sf_loadCountries(c(example4[1]), level=1)

p4 <- ggplot() + geom_sf(data=map$sf)
p4

example1 <- c(maplist$map_CAF, maplist$map_TCD)
p <- ggplot()
for(i in 1:length(example1)){
  print(p + geom_sf(data=example1$i))
  }
p  

#trying a loop
looplist <- c("DZA", "ESH", "MRT")
mapdf <- length(looplist)
binded <- rbind(map2, map3)

newlist <- c("BEL", "LUX", "YEM")
newmapdf <- gadm_sf_loadCountries(c(paste(newlist)), level=1)

ggplot() + geom_sf(data=newmapdf$sf)


####creating by-year dataframe####

geopko_year <- geopko %>% group_by(Mission, year, location) %>%
  mutate(year_ave = round(mean(No.troops, na.rm=TRUE))) %>% 
  select(Mission, year, location, latitude, longitude, year_ave)
  distinct()

  ####
  
  +
    if(input$MHQ_map){
      geom_point(data=map_df_temp() %>% filter(HQ==3, aes=longitude, y=Latitude),
                 shape=4, color="red", size=6)} +
    if(input$SHQ_map){
      p <- p + geom_point(data=map_df_temp() %>% filter(HQ==2), aes(x=Longitude, y= Latitude), shape=3, color="orange", size=5)} +
    if(input$MO_map){
      p <- p + geom_point(data=map_df_temp() %>% filter(UNMO..dummy.==1), aes(x=Longitude, y= Latitude), shape=5, color="darkgreen", size=3)} +
    labs(caption="Source: The GeoPKO dataset v1.2\n Shapefiles from GADM.org")
  theme_void()
  
  +
    geom_point(data=map_df_temp(), aes(x=longitude, y=latitude, size=No.troops, color=No.troops),
               shape=20, alpha = 0.5)+
    scale_size_continuous(name="Size of deployment",range=c(2, 20))+
    scale_color_viridis_c(name="Size of deployment")+
    guides(colour = guide_legend())
  
  
####facet plots####
#creating year df

  facetdf <- geopko %>% select(-starts_with("name.of.TCC"), -starts_with("No.troops.per.TCC"))
facetdf2 <- facetdf %>% group_by(Mission, year, location) %>% mutate(facet_ave=round(mean(No.troops)))  %>% 
  group_by(Mission, year, location) %>%
  arrange(month) %>%
  slice(1)

testfacet <- facetdf2 %>% filter(Mission=="UNOCI")

p2 <- ggplot() + geom_sf(data=map$sf) + 
  geom_point(data=testfacet, aes(x=longitude, y=latitude)) +
  facet_wrap(~ year) +
  theme_void()
p2
