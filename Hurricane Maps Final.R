library(maps)
library(tmap)
library(hurricaneexposuredata)
library(tidyverse)
library(drat)
library(magrittr)
library(sf)
#library(sp)
#library(viridis)

##read data
addRepo("geanders")
data("hurr_tracks")
data("rain")
county <- map_data("county")   
data(county.fips)

##separate polygon column into state and county columns
fips_data <- county.fips %>%
  as_tibble %>% 
  separate(polyname, c("region", "subregion"), "," )

##merge county data with fips data
all_data <- map_data("county") %>% 
  left_join(fips_data)
all_data %>% ggplot(aes(long, lat, group=group)) + 
  geom_polygon(fill="blue", color="gray70") + 
  coord_map()


##map only the states needed
mp_states <- c("texas", "oklahoma", "kansas",
               "iowa", "missouri", "arkansas", "louisiana",
               "alabama", "mississippi", "georgia", "florida",
               "tennessee","kentucky", "indiana", 
               "wisconsin", "michigan", "illinois",
               "ohio", "west virginia", "pennsylvania",
               "south carolina", "north carolina", 
               "virginia", "delaware", "maryland",
               "new jersey", "district of columbia", 
               "new york", "connecticut", "rhode island",
               "massachusetts", "vermont", "new hampshire",
               "maine")

state_data <- map_data('state', region = mp_states)

all_data  %<>% filter(region %in% mp_states)


##Floyd-1999 Map

#filter data for Floyd-1999
floyd_track <- hurr_tracks %>% filter(storm_id=="Floyd-1999")
floyd_rain <- rain %>% filter(storm_id=="Floyd-1999")

## floyd rain
fr <- floyd_rain %>% group_by(fips) %>% summarise(rain = sum(precip))


## code added from group evaluation
fr2 <- as.data.frame(fr)
for (i in 1:dim(fr2)[1]){
  fr2$rain[i] <- fr2$rain[i]%/%25
}
fr2$rain <- ordered(fr2$rain,labels = c("[0,25]","(25,50]","(50,75]","(75,100]","(100,125]","(125,150]","(150,175]","(175,200]","(200,220]"))

## Add rainfall data to all_data
fr2$fips <- as.numeric(fr2$fips)
aa <- left_join(all_data, fr2)
aa <- na.omit(aa)






##Floyd-1999 Map (ggplot2)

ggplot() +
  geom_polygon(data=aa, aes(x=long, y=lat, group=group, fill=rain), color="black", size=0.05) +
  scale_fill_brewer(palette = "Blues", name = "Rainfall(mm)")+
  labs(title = "Floyd-1999")+
  theme(plot.title = element_text(face = "bold", size =14, hjust = 0.5)) +
  theme(legend.position = "right") +
  geom_path(data=floyd_track, aes(x=longitude, y=latitude), color="red", size=0.5)





##Allison-2001

#filter data for Allison-2001
allison_track <- hurr_tracks %>% filter(storm_id=="Allison-2001")
allison_rain <- rain %>% filter(storm_id=="Allison-2001")

ar <- allison_rain %>% group_by(fips) %>% summarise(rain = sum(precip))

ar2 <- as.data.frame(ar)
for (i in 1:dim(ar2)[1]){
  if (ar2$rain[i] < 175){
    ar2$rain[i] <- 0
  }
  else {ar2$rain[i] <- 1}
}
ar2$rain <- ordered(ar2$rain, labels = c("Unexposed","Exposed"))
ar2$fips <- as.numeric(ar2$fips)

bb <- left_join(all_data, ar2)
bb <- na.omit(bb)

# Allison final ggplot
ggplot() +
  geom_polygon(data=bb, aes(x=long, y=lat, group=group, fill=rain), color="black", size=0.05) +
  scale_fill_brewer(palette = "Blues", name = "Rainfall > 175mm")+
  labs(title = "Allison-2001")+
  theme(plot.title = element_text(face = "bold", size =14, hjust = 0.5)) +
  theme(legend.position = "right") +
  geom_path(data=allison_track, aes(x=longitude, y=latitude), color="red", size=0.5)


## tmap Maps

tcounties <- st_as_sf(maps::map('county',mp_states,plot=F,fill=T))
tm_shape(tcounties) + tm_polygons()

# Floyd
t_rain <- left_join(fr2, county.fips)
t_rain <- rename(t_rain, ID = polyname)

t_all <- left_join(tcounties, t_rain)
#tm_shape(t_all) + tm_polygons(col=rain, palette = "Blues")


# Allison
a_rain <- left_join(ar2, county.fips)
a_rain <- rename(a_rain, ID = polyname)
a_all <- left_join(tcounties, a_rain)

#tm_shape(a_all) + tm_polygons(col=rain, palette = "Blues")
