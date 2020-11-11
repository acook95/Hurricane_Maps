
#    http://shiny.rstudio.com/
# code for shiny app taken from: https://shiny.rstudio.com/gallery/plot-interaction-zoom.html

library(shiny)
library(maps)
library(tidyverse)
library(hurricaneexposure)
library(drat)
library(magrittr)
library(sf)
library(readr)
library(ggplot2)

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

## Floyd-1999
#filter data for Floyd-1999
floyd_track <- hurr_tracks %>% filter(storm_id=="Floyd-1999")
floyd_rain <- rain %>% filter(storm_id=="Floyd-1999")

# organize rain data
fr <- floyd_rain %>% group_by(fips) %>% summarise(rain = sum(precip))

# code added from group review
fr2 <- as.data.frame(fr)
for (i in 1:dim(fr2)[1]){
    fr2$rain[i] <- fr2$rain[i]%/%25
}

# create intervals for rain scale
fr2$rain <- ordered(fr2$rain,labels = c("[0,25]","(25,50]","(50,75]","(75,100]","(100,125]","(125,150]","(150,175]","(175,200]","(200,220]"))

# join rain data with all data
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

## Allison-2001
#filter data for Allison-2001
allison_track <- hurr_tracks %>% filter(storm_id=="Allison-2001")
allison_rain <- rain %>% filter(storm_id=="Allison-2001")

# organize rain data
ar <- allison_rain %>% group_by(fips) %>% summarise(rain = sum(precip))

# code added from group review
ar2 <- as.data.frame(ar)
for (i in 1:dim(ar2)[1]){
    if (ar2$rain[i] < 175){
        ar2$rain[i] <- 0
    }
    else {ar2$rain[i] <- 1}
}

# create labels for rain exposure
ar2$rain <- ordered(ar2$rain, labels = c("Unexposed","Exposed"))

# join rain data with all data
ar2$fips <- as.numeric(ar2$fips)
bb <- left_join(all_data, ar2)
bb <- na.omit(bb)

# Allison map (ggplot2)
ggplot() +
    geom_polygon(data=bb, aes(x=long, y=lat, group=group, fill=rain), color="black", size=0.05) +
    scale_fill_brewer(palette = "Blues", name = "Rainfall > 175mm")+
    labs(title = "Allison-2001")+
    theme(plot.title = element_text(face = "bold", size =14, hjust = 0.5)) +
    theme(legend.position = "right") +
    geom_path(data=allison_track, aes(x=longitude, y=latitude), color="red", size=0.5)


#############################################################################
ui <- fluidPage(
    fluidRow(
        column(width = 6, class = "well",
               h4("FLOYD-1999"),
               plotOutput("plot1", height = 300,
                          dblclick = "plot1_dblclick",
                          brush = brushOpts(
                              id = "plot1_brush",
                              resetOnNew = TRUE
                          )
               )
        ),
        column(width = 6, class = "well",
               h4("ALLISON-2001"),
               plotOutput("plot2", height = 300,
                          dblclick = "plot2_dblclick",
                          brush = brushOpts(
                              id = "plot2_brush",
                              resetOnNew = TRUE
                                     )
                          )
                   )
                   
                   ),
    fluidRow(
        column(width = 6,
               textOutput("text1")),
        column(width = 6,
               textOutput("text2"))
    )
               )
        

server <- function(input, output) {
    
    # -------------------------------------------------------------------
    # Single zoomable plot (on left)
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    output$plot1 <- renderPlot({
        ggplot() +
            geom_polygon(data=aa, aes(x=long, y=lat, group=group, fill=rain), color="black", size=0.05) +
            scale_fill_brewer(palette = "Blues", name = "Rainfall(mm)")+
            labs(title = "Floyd-1999")+
            theme(plot.title = element_text(face = "bold", size =14, hjust = 0.5)) +
            theme(legend.position = "right") +
            geom_path(data=floyd_track, aes(x=longitude, y=latitude), color="red", size=0.5) +
            coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    ranges2 <- reactiveValues(x = NULL, y = NULL)
    
    output$plot2 <- renderPlot({
        ggplot() +
            geom_polygon(data=bb, aes(x=long, y=lat, group=group, fill=rain), color="black", size=0.05) +
            scale_fill_brewer(palette = "Blues", name = "Rainfall > 175mm")+
            labs(title = "Allison-2001")+
            theme(plot.title = element_text(face = "bold", size =14, hjust = 0.5)) +
            theme(legend.position = "right") +
            geom_path(data=allison_track, aes(x=longitude, y=latitude), color="red", size=0.5) +
            coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
    })
    
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot2_dblclick, {
        brush <- input$plot2_brush
        if (!is.null(brush)) {
            ranges2$x <- c(brush$xmin, brush$xmax)
            ranges2$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges2$x <- NULL
            ranges2$y <- NULL
        }
    })
    
    output$text1 <- renderText("Brush and double-click to zoom. Double click again to reset.")
    
    output$text2 <- renderText("Brush and double-click to zoom. Double click again to reset.")
}

shinyApp(ui, server)

