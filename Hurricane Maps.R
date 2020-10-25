library(usmap)
library(maps)



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
floyd_track <- hurr_tracks %>% filter(storm_id=="Floyd-1999")
floyd_rain <- rain %>% filter(storm_id=="Floyd-1999")

ggplot() + geom_polygon(data=state_data, aes(x=long, y=lat, group=group),
                         color="black", fill="gray90", size = .5 ) +
  geom_polygon(data=county_data, aes(x=long, y=lat, group=group),
                color="gray70", fill="gray90",  size = .1, alpha = .1) +
  geom_path(data=floyd_track, aes(x=longitude, y=latitude), color="red", size=0.5)


##Allison-2001 Map
allison_track <- hurr_tracks %>% filter(storm_id=="Allison-2001")
allison_rain <- rain %>% filter(storm_id=="Allison-2001")

ggplot() + geom_polygon(data=state_data, aes(x=long, y=lat, group=group),
                        color="black", fill="gray90", size = .5 ) +
  geom_polygon(data=county_data, aes(x=long, y=lat, group=group),
               color="gray70", fill="gray90",  size = .1, alpha = .1) +
  geom_path(data=allison_track, aes(x=longitude, y=latitude), color="red", size=0.5)



## usmaps Maps



