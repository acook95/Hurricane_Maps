library(usmap)
library(maps)
library(tmap)
library(tmaptools)
library(dplyr)
library(hurricaneexposure)
library(hurricaneexposuredata)
library(tidyverse)
library(tidyr)
library(drat)
library(magrittr)

addRepo("geanders")
data("hurr_tracks")
data("rain")



################################################################################
##Floyd-1999 Map

#filter data for Floyd-1999
floyd_track <- hurr_tracks %>% filter(storm_id=="Floyd-1999")
floyd_rain <- rain %>% filter(storm_id=="Floyd-1999")

## floyd rain
floyd_rain %>% group_by(fips) %>% summarise(rain = sum(precip)) -> fr

#################################
# create dataset containing all necessary info for counties

county <- map_data("county")   

data(county.fips)

county.fips %>%
  as_tibble %>% 
  separate(polyname, c("region", "subregion"), "," ) -> dfips

map_data("county") %>% 
  left_join(dfips) ->
  dall

dall %>% ggplot(aes(long, lat, group=group)) + 
  geom_polygon(fill="blue", color="gray70") + 
  coord_map()


### now map only the states needed
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

dall  %<>% filter(region %in% mp_states)

p <- ggplot() + geom_polygon(data=dall, aes(x=long, y=lat, group=group, ),
                             color="gray70", fill="gray90",  size = .1, alpha = .1) +
  
  geom_polygon(data=state_data, aes(x=long, y=lat, group=group),
               color="red", fill = "black", alpha = 0.001, size = .5 ) 
p
#########################

# dall %>% ggplot(aes(long, lat, group=group)) + 
#   geom_polygon(fill="blue", color="gray70") + 
#   coord_map()



### Add rainfall data to dall

## make a vector of rainfall amounts keyed to all the fips in dall
## using the fr 

## function to implement
# look_fips <- function(s,c){
#   return(filter(Counties, State=='AL' & Name=='Bullock')[1])
# }
#


fr3 <- as.data.frame(fr)
fr3$fips <- as.numeric(fr3$fips)

aa <- left_join(dall, fr3)


#ggplot(aa, aes(x=long, y=lat, group=group, fill=rain)) + 
#geom_polygon()+coord_map()
#+geom_path(floyd_track, aes(x=longitude,y=latitude))







##Floyd-1999 Map


#plotting
ggplot() +
  geom_polygon(data=aa, aes(x=long, y=lat, group=group, fill=rain)) +
  geom_path(data=floyd_track, aes(x=longitude, y=latitude), color="red", size=0.5)



################################################################

##Allison-2001 Map

#filter data for Allison-2001 and join datasets together
allison_track <- hurr_tracks %>% filter(storm_id=="Allison-2001")
allison_rain <- rain %>% filter(storm_id=="Allison-2001")

allison_rain %>% group_by(fips) %>% summarise(rain = sum(precip)) -> ar

ar3 <- as.data.frame(ar)
ar3$fips <- as.numeric(ar3$fips)

bb <- left_join(dall, ar3)

#plotting
# ggplot() + geom_polygon(data=state_data, aes(x=long, y=lat, group=group),
#                         color="black", fill="gray90", size = .5 ) +
#   geom_polygon(data=county_data, aes(x=long, y=lat, group=group),
#                color="gray70", fill="gray90",  size = .1, alpha = .1) +
#   geom_path(data=allison_track, aes(x=longitude, y=latitude), color="red", size=0.5)

ggplot() +
  geom_polygon(data=bb, aes(x=long, y=lat, group=group, fill=rain)) +
  geom_path(data=allison_track, aes(x=longitude, y=latitude), color="red", size=0.5)


###################################################################################
# ## usmaps Maps
# #rain_data <- floyd_rain %>% select(fips, precip)
# plot_usmap(data=floyd_data, include = c("TX","OK","KS","LA", "AR",
#                        "MO", "IA","WI", "MI","IL","IN",
#                        "OH", "KY", "TN", "AL", "MS",
#                        "FL", "GA", "SC", "NC", "VA",
#                        "WV", "MD", "DE", "PA", "NJ",
#                        "NY", "CT", "RI", "MA", "VT","NH", "ME"),
#            regions="counties", values = "precip")
# 
# #plot_usmap(regions="counties", data = rain_data, values = precip)


## tmap Maps