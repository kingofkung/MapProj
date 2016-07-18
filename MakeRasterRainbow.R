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


reg <- "ID_1"
if(!exists("brafort")) brafort <- fortify(bra2, region = reg)
brazil <- brafort[seq(1, nrow(brafort), 10),]
dim(brazil)

range(brazil$long)

str(bra2)
tbra <- fortify(bra2, region = "ID_0")
tbra <- tbra[seq(1, nrow(tbra), 100),]
tattempt <- SpatialPointsDataFrame(data.frame(tbra$long, tbra$lat), data = tbra)


## Box outside brazil?
bra2simp <- gSimplify(bra2, .0284)
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


u <- ggplot() + geom_tile(data = rainbowdf, aes(x = rainseq, y = barseq, fill = rainbow(7)[coloration]), height = 3 + diff(range(brazil$lat))) + scale_fill_identity()

## Filling with alpha allows us to make/control its transparency.
u <- u + geom_polygon(data = brazil, aes(x = long, y = lat, group = group), color = "white", fill = alpha("white", 0))  + theme(panel.background = element_blank())

u + geom_polygon(data = broutFort, aes(x = long, y = lat, group = group), fill = "white")  + coord_cartesian(ylim = c(min(brFortSh$lat), max(brFortSh$lat)))

## Try something different


ggplot(aes(long, lat, group = group), data = tbra) + geom_polygon() + theme(panel.background = element_rect("grey"))
