## Create a set of bars equivalent to a rainbow, so I don't have to
## keep messing around with all these probabilities.
rm(list = ls()[!ls() %in% c("bra2", "brafort", "broutFort", "brout")])
library(maptools)
library(ggplot2)
library(rgeos)
library(maps)

maploc <- "/Users/bjr/GitHub/MapProj/Shp/BRA_adm_shp/"
maindir <- "/Users/bjr/Dropbox/R_Projects/MapSomething/RBmaps/"
set.seed(123456)

if(!exists("bra2"))bra2 <- readShapePoly(paste0(maploc, "BRA_adm2.shp"))

names(bra2)


if(!exists("brafort")) brafort <- fortify(bra2, region = "ID_2")
brazil <- brafort[seq(1, nrow(brafort), 10),]
dim(brazil)

range(brazil$long)

str(bra2)
if(!exists("tbra")) tbra <- fortify(bra2, region = "ID_0")



## Box outside brazil?
tst <- unionSpatialPolygons(bra2, bra2$ID_0)

bra2simp <- gSimplify(tst, .0284)
brenv <- gEnvelope(bra2simp)
brout <- gDifference(brenv, bra2simp, byid = F)
## brout2 <- gIntersection(brenv, bra2simp,  byid = T, drop_lower_td = T)

broutFort <- fortify(brout)
brFortSh <- broutFort
## brFortSh$order <- 1:nrow(brFortSh)


rainseq <- seq(min(brFortSh$long)+.1, max(brFortSh$long)-.1, by = .01)
barseq <- rep(mean(brFortSh$lat), length.out = length(rainseq))
coloration <- sort(rep(1:7, length.out = length(rainseq)))

rainbowdf <- data.frame(rainseq, "barseq" = barseq, coloration)

dev.new()
pdf(paste0(maindir,"BrazilRB.pdf"))
u <- ggplot() + geom_tile(data = rainbowdf, aes(x = rainseq, y = barseq, fill = rainbow(7)[coloration]), height = 5 + diff(range(brazil$lat))) + scale_fill_identity()
## Filling with alpha allows us to make/control its transparency.
u <- u + geom_polygon(data = brazil, aes(x = long, y = lat, group = group), color = "white", fill = alpha("white", 0))  + theme(panel.background = element_blank())
###
u <- u + geom_polygon(data = broutFort, aes(x = long, y = lat, group = group), fill = "white")
u <- u + theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())
###
###
u + coord_cartesian(ylim = c(min(brFortSh$lat) + 1.85, max(brFortSh$lat) - 1.8))
graphics.off()

## Try something different


ggplot(aes(long, lat, group = group), data = tbra2) + geom_polygon() + theme(panel.background = element_rect("grey"))
