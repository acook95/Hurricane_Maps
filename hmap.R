

library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)

addRepo("geanders")


data("hurr_tracks")

data("rain")

head(hurr_tracks)

head(rain)


map_counties(storm = "Floyd-1999", metric = "rainfall") +
    ggtitle("Floyd-1999") +
    theme(plot.title = element_text(hjust = 0.5))



map_rain_exposure(storm ="Allison-2001", 
                  rain_limit = 175, 
                  dist_limit = 500, 
                  days_included =-5:3) +
    ggtitle("Allison-2001") +
    theme(plot.title = element_text(hjust = 0.5))


## colors
## http://sape.inf.usi.ch/quick-reference/ggplot2/colour



############################################################
##### Usmap
############################################################



library(usmap)

plot_usmap(include = c("TX","OK","KS","LA", "AR", 
                       "MO", "IA","WI", "MI","IL","IN", 
                       "OH", "KY", "TN", "AL", "MS",
                       "FL", "GA", "SC", "NC", "VA",
                       "WV", "MD", "DE", "PA", "NJ", 
                       "NY", "CT", "RI", "MA", "VT","NH", "ME"), 
                        fill ="skyblue2",
                        color = "red")


plot_usmap(include = c("TX","OK","KS","LA", "AR", 
                       "MO", "IA","WI", "MI","IL","IN", 
                       "OH", "KY", "TN", "AL", "MS",
                       "FL", "GA", "SC", "NC", "VA",
                       "WV", "MD", "DE", "PA", "NJ", 
                       "NY", "CT", "RI", "MA", "VT","NH", "ME"), 
           regions="counties", color = "gray57") 


plot_usmap(include = c(1L, "05", "09", 10, 11, 12, 13, 17, 18, 19, 
                       20, 21, 22, 23, 24, 25, 28L, 29, 
                       33, 34, 36, 37, 39, 48, 40, 42,44, 45, 47,48,
                       50, 51, 54, 55), regions = "counties")



#################################################################
## Example using usmap --
#################################################################



#Example data (poverty rates)
county_data<-read.csv("https://www.ers.usda.gov/webdocs/DataFiles/48747/PovertyEstimates.csv?v=2529") %>% #
    filter(Area_name != "United States") %>%
    select(FIPStxt, Stabr, Area_name, PCTPOVALL_2018) %>%
    rename(fips = FIPStxt)

states <- plot_usmap("states", 
                     color = "red",
                     fill = alpha(0.01)) #this parameter is necessary to get counties to show on top of states

counties <- plot_usmap(data = county_data, 
                       values = "PCTPOVALL_2018",
                       color = "black",
                       size = 0.1)

states

counties




#################################################################
## ggplot with maps library
###################################################
library(maps) 
MainStates <- map_data("state")
ggplot() + 
    geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                  color="black", fill="lightblue" )

########################################################


AllCounty <- map_data("county")
ggplot() + geom_polygon( data=AllCounty, aes(x=long, y=lat, group=group),
                         color="darkblue", fill="lightblue", size = .1 ) +
    
    geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                  color="black", fill="lightblue",  size = 1, alpha = .3)

###################################################











#############################################
###########################################
###############################################


map_counties(storm = "Allison-2001", metric= "rainfall", days_included = -1:0) +
    ggtitle("Rain Allison")



map_counties(storm = "Allison-2001", metric = "rainfall", days_included = -5:3) +
    ggtitle("rain Allison 2001")



map_counties(storm = "Katrina-2005", metric = "wind")


map_counties("Katrina-2005", metric = "wind", wind_var = "sust_dur")



map_counties("Katrina-2005", metric = "wind", wind_source = "ext_tracks")


map_counties(storm = "Sandy-2012", metric = "distance")

map_distance_exposure(storm = "Sandy-2012", dist_limit = 75)



map_rain_exposure(storm ="Allison-2001", 
                  rain_limit = 175, 
                  dist_limit = 500, 
                  days_included =-5:3)



library(weathermetrics)

map_wind_exposure(storm = "Katrina-2005",
    wind_limit = convert_wind_speed(34, "knots","mps"))


map_event_exposure(storm = "Floyd-1999", event_type = "flood")

map_event_exposure(storm = "Ivan-2004", event_type = "tornado")


map_tracks(storms = "Floyd-1999")



map_tracks(storms = c("Andrew-1992", "Katrina-2005", "Rita-2005"), 
                 alpha = 0.5,
                 plot_points = TRUE,
                 color = "blue")


storms_2018 <- hurr_tracks %>% select(storm_id) %>% 
                                distinct() %>% 
                               mutate(year = str_extract(storm_id, "-[0-9].+")) %>% 
                               filter(year == "-2018")

map_tracks(storms = storms_2018$storm_id)




floyd_map <- map_event_exposure(storm = "Floyd-1999", event_type = "flood")

map_tracks(storms = "Floyd-1999",
    plot_object =floyd_map,
    plot_points =TRUE,
    color ="darkgray"
)

