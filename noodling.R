states_tm<-c("WV","FL")
> a <- filter(states2, STUSPS==states_tm)
> View(states2)
> states_tm <- c("TX", "OK")
> a <- filter(states2, STUSPS==states_tm)
> states_tm <- c("TX", "OK")
> a <- filter(states2, STUSPS==states_tm)
a <- filter(states2, STUSPS==c("TX", "OK"))
a <- filter(states2, STUSPS==c("TX", "OK"))
a <- filter(states2, STUSPS %in% states_tm)


tm_shape(states2) +
  tm_borders(col="black", lwd=1) +
  tm_layout(title="Floyd-1999", title.position = c("center", "top")) +
tm_shape(floyd_rain2) +
  tm_borders(col="grey",lwd=0.1) +
  tm_fill("rain", id="fips", title = "Rain (mm)", palette = "Blues")

+
  tm_style_beaver(title = "Floyd-1999", title.position = c("center", "top"))



tm_shape(county2) +
  tm_borders(col="grey", lwd=0.5)


