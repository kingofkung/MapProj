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

## Read in DHM's Data to make colors
library(readxl)
dhmdat <- read_excel("/Users/bjr/Dropbox/R_Projects/MapSomething/DHMData/gun ownership by state.xlsx")
colnames(dhmdat)[3] <- "GunOwnersPerK"
dhmdat$GunOwnersPerK
range(dhmdat$GunOwnersPerK)

dhmdat$GunOwnLog <- log(dhmdat$GunOwnersPerK, 10)
range(dhmdat$GunOwnLog)
## hist(dhmdat$GunOwnLog)


statemdf <- data.frame(state.name, state.abb)
dhmdat <- merge(dhmdat, statemdf, by.x = "State", by.y = "state.name")

mymap2 <- merge(mymap, dhmdat, by.x = "id", by.y = "state.abb", all.x = TRUE)
## Looks like in order to
mymap2 <- mymap2[order(mymap2$order),]
head(mymap2)

writeloc <- "/Users/bjr/Dropbox/R_Projects/MapSomething/"

dev.new()
pdf(paste0(writeloc, "DHMSMap.pdf"))
##
## coord_map() give us a mercator projection. It's quite nice.
mynewmap <- ggplot(mymap2) + geom_polygon(aes(x = long, y = lat, group = group, fill = GunOwnLog), color = "darkolivegreen") + coord_map()
mynewmap <- mynewmap + scale_fill_gradient(high = "black", low = "lightgray",
                                           breaks = seq(2.5, 0, -.25),
                                           guide = guide_legend(title = "Logged Gun Owners") )
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
mynewmap <- mynewmap + theme(panel.background = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5))  + ggtitle("For Dr. Haider-Markel: \nLogged Gun Owners Per 1000 People in 2013")
##
print(mynewmap)
##
## Found out that unlike dev.off(), graphics.off() actually closes all the windows R creates
graphics.off()

