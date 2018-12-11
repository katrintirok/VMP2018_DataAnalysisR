# tests with rworldmap

library(dismo)  # check also the nice 'rgbif' package! 
laurus <- gbif("Laurus", "nobilis")
locs <- subset(laurus, select = c("country", "lat", "lon"))
head(locs)  # a simple data frame with coordinates

locs <- subset(locs, locs$lat < 90)

coordinates(locs) <- c("lon", "lat")  # set spatial coordinates
plot(locs)

crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data
summary(locs)

plot(locs, pch = 20, col = "steelblue")

library(rworldmap)
# library rworldmap provides different types of global maps, e.g:
data(coastsCoarse)
data(countriesLow)
plot(coastsCoarse, add = T)

locs.gb <- subset(locs, locs$country == "United Kingdom")  # select only locs in UK
plot(locs.gb, pch = 20, cex = 2, col = "steelblue")
title("Laurus nobilis occurrences in UK")
plot(countriesLow, add = T)

locs.sa <- subset(locs, locs$country == "South Africa")
plot(locs.sa, pch = 20, cex = 2, col = 'red')
plot(countriesLow, add = T)

# make map of africa/south africa
worldmap <- getMap(resolution = "high")
dim(worldmap)
plot(worldmap)
# add xlim and ylim 
par(mar=c(0,0,0,0))
plot(worldmap, xlim = c(26, 32), ylim = c(-35, -15), asp = 1) 
# -27.5 - -29.1, 31.8 - 32.9
lines(x = c(32.6,32.6,32.6,32.9), 
      y = c(-29.1,-27.5,-27.5,-27.5), col = 'red')
lines(x = c(31.8,32.9,32.9,32.9), 
      y = c(-29.1,-29.1,-29.1,-27.5), col = 'red')

pdf('mapSA_vmp.pdf', width = 8, height = 5)
par(mar=c(0,0,0,0))
plot(worldmap, xlim = c(23, 32), ylim = c(-35, -15), asp = 1) 
# -27.5 - -29.1, 31.8 - 32.9
lines(x = c(32.6,32.6,32.6,32.9), 
      y = c(-27.6,-27.4,-27.4,-27.4), col = 'red', lwd = 3)
lines(x = c(32.6,32.9,32.9,32.9), 
      y = c(-27.6,-27.6,-27.6,-27.4), col = 'red', lwd = 3)
dev.off()

