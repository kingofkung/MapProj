## Create a set of bars equivalent to a rainbow, so I don't have to
## keep messing around with all these probabilities.
rm(list = ls()[!ls() %in% c("stfort", "ctsin", "fips", "brout", "borderPoly")])
library(maptools)
library(ggplot2)
library(rgeos)
library(maps)

## maploc <- "/Users/bjr/GitHub/MapProj/Shp/BRA_adm_shp/"
loc2 <- "/Users/bjr/GitHub/MapProj/Shp/cb_2015_us_county_500k/"
maindir <- "/Users/bjr/Dropbox/R_Projects/MapSomething/RBmaps/"
set.seed(123456)

if(!exists("ctsin")) ctsin <- readShapeSpatial(paste0(loc2, "cb_2015_us_county_500k.shp"))
if(!exists("fips")) fips <- read.csv(paste0("/Users/bjr/GitHub/MapProj/FIPS Codes by state.csv"))


names(ctsin)

statetouse <- "North Carolina"

## For when we have a state we specifically want to use, this gets
## that states name and turns it into an FIPS Code
fptouse <- fips[which(fips$State.Name == statetouse),"FIPS.Code"]
if(nchar(as.character(fptouse)) == 1) fptouse <- paste0(0,fptouse)

## Get rid of noncontinental FIPS.
## fpstonotuse <- fips$FIPS.Code[fips$State.Name %in% c("Alaska", "Hawaii", "Puerto Rico", "Virgin Islands", "American Samoa", "Guam")]
## fpstonotuse <- unlist(lapply(fpstonotuse, function(x) if(nchar(as.character(x)) == 1) x <- paste0(0,x) else x))


cts <- fortify(ctsin[ctsin$STATEFP == fptouse,], region = "NAME")




## Want to make a box outside Brazil? unionSpatialPolygons joins
## together all the polygons with a certain id. Since ID_0 represents
## being a part of Brazil, this gets all of the polygons inside the
## mainland. Wish there was a way to make it faster though...
ctsin$STATEFP %in% fptouse
if(!exists("borderPoly")) borderPoly <- unionSpatialPolygons(ctsin[ctsin$STATEFP == fptouse,], ctsin$STATEFP[ctsin$STATEFP == fptouse])

stenv <- gEnvelope(borderPoly)
stOut <- gDifference(stenv, borderPoly, byid = F)
stOutFort <- fortify(stOut)

## Make the rainbow!
latseq <- seq(min(stOutFort$lat), max(stOutFort$lat), by = .001)
longbarseq <- rep(mean(stOutFort$long) - 1.703, length.out = length(latseq))
coloration1 <- sort(rep(2:0, length.out = length(latseq)/5*3), decreasing = T)
coloration2 <- sort(rep(1:2, length.out = length(latseq) - length(coloration1) ))

colordf <- data.frame(latseq, "longbarseq" = longbarseq, "color" = c(coloration1, coloration2))
##Since we can't use rainbows anymore, we'll need to make our own color data frame
colormap <- data.frame("blue" = c(123,204, 246), "pink" = c(233, 173, 184))

collist <- c("2" = rgb(t(colormap$blue/255)), "1" = rgb(t(colormap$pink/255)),  rgb(1,1,1))
## Weirdly, c() won't take 0 as a name. Let's fix that below
names(collist)[3] <- "0"

colordf$colvals <- NA


for(i in 0:2) colordf$colvals[colordf$color %in% names( collist[i+1] )] <- collist[i+1]
table(colordf$colvals, colordf$color)

dev.new()
pdf(paste0(maindir,statetouse, "cols.pdf"))
## width = (max(cts$long)- min(cts$long))
u <- ggplot() + geom_tile(data = colordf, aes(x = longbarseq , y = latseq, fill = colvals), width = diff(range(stOutFort$long))) + scale_fill_identity()
## Filling with alpha allows us to make/control its transparency.
u <- u + geom_polygon(data = borderPoly, aes(x = long, y = lat, group = group), color = "black", fill = alpha("white", 0))  + coord_quickmap()
## Layer on part hiding everything that isn't brazil
u <- u + geom_polygon(data = stOutFort, aes(x = long, y = lat, group = group), fill = "white")
## u <- u + theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank()) + theme(panel.background = element_blank())
### Add layers to hide additional rainbow atop and below
leftaes <- aes(xmin = min(long), xmax = max(long), ymin = max(lat) - 0.035, ymax = max(lat) + 5 )
## u <- u + geom_rect(data = broutFort, topaes, fill = "white") + geom_rect(data = broutFort, bottomaes, fill = "white")
u
###
graphics.off()

