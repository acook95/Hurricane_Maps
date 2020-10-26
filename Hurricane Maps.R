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

Counties <- read_csv("Counties.csv")
States <- read_csv("States.csv")
addRepo("geanders")
data("hurr_tracks")
data("rain")



## GGPlot2 Maps
state_data <- map_data('state', region = c('maine','vermont','new hampshire','new york', 'massachusetts','rhode island',
                        'delaware','connecticut','pennsylvania','new jersey','maryland','west virginia',
                        'virginia','ohio','kentucky','north carolina','south carolina','georgia',
                        'alabama','mississippi','florida','louisiana','texas','oklahoma','kansas',
                        'arkansas','tennessee','indiana','wisconsin','illinois','michigan','missouri','iowa'))
county_data <- map_data('county', region = c('maine','vermont','new hampshire','new york', 'massachusetts','rhode island',
                                            'delaware','connecticut','pennsylvania','new jersey','maryland','west virginia',
                                            'virginia','ohio','kentucky','north carolina','south carolina','georgia',
                                            'alabama','mississippi','florida','louisiana','texas','oklahoma','kansas',
                                            'arkansas','tennessee','indiana','wisconsin','illinois','michigan','missouri','iowa'))


##Floyd-1999 Map

#filter data for Floyd-1999 and join two datasets together
floyd_track <- hurr_tracks %>% filter(storm_id=="Floyd-1999")
floyd_rain <- rain %>% filter(storm_id=="Floyd-1999")
floyd_data <- left_join(floyd_track, floyd_rain)


## Experimenting--ignore this chunk
#floyd_rain <- rain %>% filter(storm_id=="Floyd-1999") %>% select(fips, precip) %>% group_by(fips) %>% summarize(precip=mean(precip)) %>% rename(FIPS=fips)
#floyd_rain$FIPS <- as.numeric(floyd_rain$FIPS)
#rain_join <- left_join(Counties, floyd_rain)
#rain_join <- rain_join %>% filter(is.na(precip)==FALSE) %>% rename(fips=FIPS)


#plotting
ggplot() + geom_polygon(data=state_data, aes(x=long, y=lat, group=group),
                         color="black", fill="gray90", size = .5 ) +
  geom_polygon(data=county_data, aes(x=long, y=lat, group=group),
                color="gray70", fill="gray90",  size = .1, alpha = .1) +
  geom_path(data=floyd_track, aes(x=longitude, y=latitude), color="red", size=0.5)
  

##experimenting--ignore this line
#geom_polygon(data = floyd_data, aes(x=longitude, y=latitude, fill=precip))




##Allison-2001 Map

#filter data for Allison-2001 and join datasets together
allison_track <- hurr_tracks %>% filter(storm_id=="Allison-2001")
allison_rain <- rain %>% filter(storm_id=="Allison-2001")
allison_data <- left_join(allison_track, allison_rain)

#plotting
ggplot() + geom_polygon(data=state_data, aes(x=long, y=lat, group=group),
                        color="black", fill="gray90", size = .5 ) +
  geom_polygon(data=county_data, aes(x=long, y=lat, group=group),
               color="gray70", fill="gray90",  size = .1, alpha = .1) +
  geom_path(data=allison_track, aes(x=longitude, y=latitude), color="red", size=0.5)



## usmaps Maps
#rain_data <- floyd_rain %>% select(fips, precip)
plot_usmap(data=floyd_data, include = c("TX","OK","KS","LA", "AR",
                       "MO", "IA","WI", "MI","IL","IN",
                       "OH", "KY", "TN", "AL", "MS",
                       "FL", "GA", "SC", "NC", "VA",
                       "WV", "MD", "DE", "PA", "NJ",
                       "NY", "CT", "RI", "MA", "VT","NH", "ME"),
           regions="counties", values = "precip")

#plot_usmap(regions="counties", data = rain_data, values = precip)


## tmap Maps


