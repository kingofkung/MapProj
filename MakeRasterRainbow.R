## Create a set of bars equivalent to a rainbow, so I don't have to
## keep messing around with all these probabilities.
rm(list = ls()[!ls() %in% c("bra2", "brafort", "broutFort", "brout", "borderPoly")])
library(maptools)
library(ggplot2)
library(rgeos)
library(maps)

maploc <- "/Users/bjr/GitHub/MapProj/Shp/BRA_adm_shp/"
maindir <- "/Users/bjr/Dropbox/R_Projects/MapSomething/RBmaps/"
set.seed(123456)

if(!exists("bra2")) bra2 <- readShapePoly(paste0(maploc, "BRA_adm2.shp"))

names(bra2)

## Bit of a problem here. I don't want to create brafort every time, since fortifying brazil takes a while, but I do want to create it every time I change the region
if(!exists("brafort")) brafort <- fortify(bra2, region = "ID_1")
brazil <- brafort[seq(1, nrow(brafort), 10),]
dim(brazil)

range(brazil$long)

str(bra2)



## Want to make a box outside Brazil? unionSpatialPolygons joins
## together all the polygons with a certain id. Since ID_0 represents
## being a part of Brazil, this gets all of the polygons inside the
## mainland. Wish there was a way to make it faster though...
if(!exists("borderPoly")) borderPoly <- unionSpatialPolygons(bra2, bra2$ID_0)

bra2simp <- gSimplify(borderPoly, tol = .029)
brenv <- gEnvelope(bra2simp)
brout <- gDifference(brenv, bra2simp, byid = F)
broutFort <- fortify(brout)

## Make the rainbow!
rainseq <- seq(min(broutFort$long)+.1, max(broutFort$long)-.1, by = .01)
barseq <- rep(mean(broutFort$lat), length.out = length(rainseq))
coloration <- sort(rep(1:7, length.out = length(rainseq)))
rainbowdf <- data.frame(rainseq, "barseq" = barseq, coloration)


dev.new()
pdf(paste0(maindir,"BrazilRB.pdf"))
##
u <- ggplot() + geom_tile(data = rainbowdf, aes(x = rainseq, y = barseq, fill = rainbow(7)[coloration]), height = 5 + diff(range(brazil$lat))) + scale_fill_identity()
## Filling with alpha allows us to make/control its transparency.
u <- u + geom_polygon(data = brazil, aes(x = long, y = lat, group = group), color = "white", fill = alpha("white", 0))  + theme(panel.background = element_blank()) + coord_quickmap()
## Layer on part hiding everything that isn't brazil
u <- u + geom_polygon(data = broutFort, aes(x = long, y = lat, group = group), fill = "white")
u <- u + theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())
###
### Add layers to hide additional rainbow atop and below
topaes <- aes(xmin = min(long), xmax = max(long), ymin = max(lat) - 0.035, ymax = max(lat) + 5 )
bottomaes <- aes(xmin = min(long), xmax = max(long), ymax = min(lat), ymin = min(lat) - 0.6)
u <- u + geom_rect(data = broutFort, topaes, fill = "white") + geom_rect(data = broutFort, bottomaes, fill = "white")
u
###
graphics.off()

