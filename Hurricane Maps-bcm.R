library(usmap)
library(maps)
library(tmap)
library(tmaptools)
library(dplyr)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)

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
fips_data <- map_data('county.fips', region = c('maine','vermont','new hampshire','new york', 'massachusetts','rhode island',
                                             'delaware','connecticut','pennsylvania','new jersey','maryland','west virginia',
                                             'virginia','ohio','kentucky','north carolina','south carolina','georgia',
                                             'alabama','mississippi','florida','louisiana','texas','oklahoma','kansas',
                                             'arkansas','tennessee','indiana','wisconsin','illinois','michigan','missouri','iowa'))


##Floyd-1999 Map
floyd_track <- hurr_tracks %>% filter(storm_id=="Floyd-1999")
floyd_rain <- rain %>% filter(storm_id=="Floyd-1999") %>% 
  select(fips, precip) %>% 
  group_by(fips) %>% 
  summarize(precip=mean(precip))

states <- plot_usmap("states", 
                     color = "red",
                     fill = alpha(0.01)) #this parameter is necessary to get counties to show on top of states

counties <- plot_usmap(data = floyd_rain, 
                       values = "precip",
                       color = "black",
                       size = 0.1)

states

counties


ggplot() + geom_polygon(data=state_data, aes(x=long, y=lat, group=group),
                         color="black", fill="gray90", size = .5 )
# geom_polygon(data=county_data, aes(x=long, y=lat, group=group),
#                 color="gray70", fill="gray90",  size = .1, alpha = .1) +
#   geom_path(data=floyd_track, aes(x=longitude, y=latitude), color="red", size=0.5)

##Allison-2001 Map
allison_track <- hurr_tracks %>% filter(storm_id=="Allison-2001")
allison_rain <- rain %>% filter(storm_id=="Allison-2001")

ggplot() + geom_polygon(data=state_data, aes(x=long, y=lat, group=group),
                        color="black", fill="gray90", size = .5 ) +
  geom_polygon(data=county_data, aes(x=long, y=lat, group=group),
               color="gray70", fill="gray90",  size = .1, alpha = .1) +
  geom_path(data=allison_track, aes(x=longitude, y=latitude), color="red", size=0.5)



# ## usmaps Maps
# rain_data <- floyd_rain %>% select(fips, precip)
# plot_usmap(include = c("TX","OK","KS","LA", "AR", 
#                        "MO", "IA","WI", "MI","IL","IN", 
#                        "OH", "KY", "TN", "AL", "MS",
#                        "FL", "GA", "SC", "NC", "VA",
#                        "WV", "MD", "DE", "PA", "NJ", 
#                        "NY", "CT", "RI", "MA", "VT","NH", "ME"), 
#            regions="counties", color = "gray57") 
# 
# plot_usmap(regions="counties", data = rain_data, values = precip)

# states <- plot_usmap("states", 
#                      color = "red",
#                      fill = alpha(0.01)) #this parameter is necessary to get counties to show on top of states
# 
# counties <- plot_usmap(data = floyd_rain, 
#                        values = "precip",
#                        color = "black",
#                        size = 0.1)
# 
# states
# 
# counties

## tmap Maps


