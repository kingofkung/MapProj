
## How I made a Kansas Legislative District map.


## Set working directory
setwd("/Users/bjr/GitHub/MapProj/MapDemo/tl_2016_20_sldl/")

## I picked up this shapefile from http://www.census.gov/cgi-bin/geo/shapefiles/index.php
library(maptools)
shpin <- readShapePoly("tl_2016_20_sldl.shp")
str(shpin)

## One of the minuses of ggplot2 is that it absolutely needs a
## data.frame for plotting, and shapefiles come in with a unique
## structure, as seen above. The fortify() command transforms a
## shapefile or other kind of map into a dataframe suitable for
## plotting.
library(ggplot2)

shpfort <- fortify(shpin, region = "NAMELSAD")
str(shpfort)
head(shpfort)

## This is just me making up some data to serve as coloration, but if
## you wanted there to be real data, you'd just need to read it in and
## merge it with your map data.frame. You do need to make sure you
## have a way of identifying which map goes where though!
shpID <- unique(shpfort$id)
prefcols <- data.frame("colid" = shpID, "col" = sample(1:100, length(shpID), replace = TRUE))
head(prefcols)

## And here we merge the two dataframes together. Now if the id is the
## same, you can use the by argument (following the form by =
## "idGoesHere"), instead of by.x and by.y like I do below.
shpfort <- merge(x = shpfort, y = prefcols, by.x = "id", by.y = "colid", all.x = TRUE)
str(shpfort)
head(shpfort)

## This is the initial ggplot command. Note that we start with ggplot, and add layers like geom_polygon() and coord_map()
## coord_map() give us a mercator projection by default. It resolves aspect ratio problems very easily.
shp <- ggplot(shpfort) + geom_polygon(aes(x = long, y = lat, group = group, fill = col, colour = I("black"))) + coord_map()
shp
##
## Change the colors to the ones I want. There are many ways to change
## colors in ggplot2, but this one lets me set a gradient between the
## limits I prefer.
shp <- shp + scale_fill_gradient(high = "Yellow", low = "Blue")
shp
##
## This command is optional, but it gets rid of a bunch of extra stuff
## in the plot.
shp <- shp + theme(panel.background = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank(),
                   legend.position = "none")
shp
## And finally, I add a plot title and subtitle, and center it using
## theme()
shp <- shp + labs(title = "Ben's Mapping Demonstration:", subtitle = "State Legislative Districts in the State of Kansas") + theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
shp
