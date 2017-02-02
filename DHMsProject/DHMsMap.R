## Try and do this map for DHM.

rm(list = ls())

library(ggplot2)
library(dplyr)
library(maptools)
library(rgeos)

## read in my shapefile
maploc <- "/Users/bjr/GitHub/MapProj/Shp/cb_2015_us_state_5m/"
mapin <- readShapePoly(paste0(maploc, "cb_2015_us_state_5m.shp"))
 str(mapin, 3)

mymap <- fortify(mapin, region = "STUSPS")
head(mymap)

## Remove non-continental states
unique(mymap$id)
mymap <- mymap[mymap$long < 0 & mymap$lat > 20 & !mymap$id %in% c("AK", "HI"),]

## Make some data that will be colors
prefcols <- data.frame(state.abb, col = state.x77[, "Income"])

mymap2 <- merge(mymap, prefcols, by.x = "id", by.y = "state.abb", all.x = TRUE)
## Looks like in order to
mymap2 <- mymap2[order(mymap2$order),]
head(mymap2)

writeloc <- "/Users/bjr/Dropbox/R_Projects/MapSomething/"

dev.new()
pdf(paste0(writeloc, "DHMSMap.pdf"))
##
## coord_map() give us a mercator projection. It's quite nice.
mynewmap <- ggplot(mymap2) + geom_polygon(aes(x = long, y = lat, group = group, fill = col), color = "darkolivegreen") + coord_map()
mynewmap <- mynewmap + scale_fill_gradient(high = "black", low = "lightgray", breaks = seq(6000, 3000, -500), guide = guide_legend(title = "Incomes") )
##
##
## Add labeling. gCentroid is supposed to give us the central
## locations of the polygons. Byid means it separates them out by id
## before returning them.
## labcoords <- gCentroid(mapin, byid = TRUE)
## labs <-data.frame(cbind(data.frame(labcoords), id = mapin@data$iso3166_2))
## ##
## hex <- hex + geom_text(data = labs, aes(label = id, x = x, y = y), color = "white", size = 2)
## ##
mynewmap <- mynewmap + theme(panel.background = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5))  + ggtitle("Benjamin Rogers' Grayscale Demonstration:\nPer Capita Income in Dollars Circa 1974\n")
##
print(mynewmap)
##
## Found out that unlike dev.off(), graphics.off() actually closes all the windows R creates
graphics.off()

