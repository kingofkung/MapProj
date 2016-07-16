 ## Figured I'd try making a map using shapefiles for the first time in a while.
## install.packages("maptools", dependencies = TRUE)
rm(list = ls()[!ls() %in% c("bra0", "bra1", "bra2", "bra3")])
library(maptools)
maploc <- "/Users/bjr/GitHub/MapProj/Shp/BRA_adm_shp/"
maindir <- "/Users/bjr/Dropbox/R_Projects/MapSomething/RBmaps/"
set.seed(123456)

bra2 <- readShapePoly(paste0(maploc, "BRA_adm2.shp"))

names(bra2)
head(bra)
 str(bra, 5)

library(ggplot2)
brazil <- fortify(bra2, region = "ID_1")
brazil <- brazil[seq(1, nrow(brazil), 10),]
dim(brazil)
head(brazil)


## 7 colors, with predefined proportions
## note to self: Weights don't have to sum to 1.
colprops <- rep(1, 7)

colsamp <- sort(sample(1:7, length(unique(brazil$group)), replace = TRUE, prob = colprops ))
prop.table(table(colsamp))



## These lines sort the counties by color based on latitude or
## longitude.
stripedir <- "vert"

## aggregate longitudes by mean. Useful if we want to group colors and
## sort them by location on the map (i.e., if we want stripes of
## color instead of a mixture).



if(stripedir == "vert"){
    meanlong <- aggregate(brazil$long, by = list(brazil$group), FUN = function(x) mean(x))
    coldf <- data.frame(ctname = unique(brazil$group)[order(meanlong$x)], ctcol = colsamp)
}

if(stripedir == "horiz"){
    meanlat <- aggregate(brazil$lat, by = list(brazil$group), FUN = function(x) mean(x))
    coldf <- data.frame(ctname = unique(brazil$group)[order(-meanlat$x)], ctcol = colsamp)
}

## Get those colors into cts
brazil <- merge(brazil, coldf, by.x = "group", by.y = "ctname", all = TRUE)



### Begin mapping
dev.new()
fileproplong <- diff(range(brazil$long))
fileproplat <-  diff(range(brazil$lat))
pdf(paste0(maindir, "", "Brazil", "Rainbow", stripedir, ".pdf"), width = fileproplong * .8, height = fileproplat, )


map <- ggplot(brazil) + geom_polygon(aes(x = long, y = lat, group = group, fill = ctcol), color = "black")
## changes to the mercator projection, something most people are used to seeing
map <- map + coord_map()

## Change the color scheme of the counties
map <- map + scale_fill_gradientn(colours = rainbow(7))

map <- map + theme(panel.background = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position = "none")

print(map)
graphics.off()
