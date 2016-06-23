 ## Figured I'd try making a map using shapefiles for the first time in a while.
## install.packages("maptools", dependencies = TRUE)
rm(list = ls()[!ls() %in% c("ctsin", "cdsin")])
library(maptools)
maploc <- "/Users/bjr/GitHub/MapProj/Shp/cb_2015_us_county_500k/"
maindir <- "/Users/bjr/Dropbox/R_Projects/MapSomething/RBmaps/"
set.seed(123456)

if(!exists("ctsin")) ctsin <- readShapeSpatial(paste0(maploc, "cb_2015_us_county_500k.shp"))
if(!exists("fips")) fips <- read.csv(paste0("/Users/bjr/GitHub/MapProj/FIPS Codes by state.csv"))

names(ctsin)

statetouse <- "California"

## For when we have a state we specifically want to use, this gets
## that states name and turns it into an FIPS Code
fptouse <- fips[which(fips$State.Name == statetouse),"FIPS.Code"]
if(nchar(as.character(fptouse)) == 1) fptouse <- paste0(0,fptouse)

## Get rid of noncontinental FIPS.
## fpstonotuse <- fips$FIPS.Code[fips$State.Name %in% c("Alaska", "Hawaii", "Puerto Rico", "Virgin Islands", "American Samoa", "Guam")]
## fpstonotuse <- unlist(lapply(fpstonotuse, function(x) if(nchar(as.character(x)) == 1) x <- paste0(0,x) else x))


library(ggplot2)
cts <- fortify(ctsin[ctsin$STATEFP == fptouse,], region = "NAME")

## For when we decide we want to make the whole U.S.
if(statetouse == "Continental"){
    cts <- fortify(ctsin[!ctsin$STATEFP %in% fpstonotuse,], region = "NAME")

                             }
## This gets rid of all of the islands that don't have a FIPS
## code. It's a little hacky, but I was low on ideas. I'm keeping it
## out of the if statement because Alaska just had the Aleutian
## Islands showing up, and I can't have that.
cts <- cts[cts$long < -50,]

unique(cts$id)

## aggregate longitudes by mean. Useful if we want to group colors and
## sort them by location on the map (i.e., if we want stripes of
## color instead of a mixture).

meanlong <- aggregate(cts$long, by = list(cts$group), FUN = function(x) mean(x))
meanlat <- aggregate(cts$lat, by = list(cts$group), FUN = function(x) mean(x))

## 7 colors, with predefined proportions
## note to self: Weights don't have to sum to 1.
colprops <- rep(1, 7)

colsamp <- sort(sample(1:7, length(unique(cts$group)), replace = TRUE, prob = colprops ))
prop.table(table(colsamp))



## These lines sort the counties by color based on latitude or
## longitude.
stripedir <- "horiz"

if(stripedir == "vert") coldf <- data.frame(ctname = unique(cts$group)[order(meanlong$x)], ctcol = colsamp)
if(stripedir == "horiz") coldf <- data.frame(ctname = unique(cts$group)[order(-meanlat$x)], ctcol = colsamp)


## Get those colors into cts
cts <- merge(cts, coldf, by.x = "group", by.y = "ctname", all = TRUE)



 ### Begin mapping
dev.new()
fileproplong <- diff(range(cts$long))
fileproplat <-  diff(range(cts$lat))
pdf(paste0(maindir, "", statetouse, "Rainbow", stripedir, ".pdf"), width = fileproplong * .8, height = fileproplat, )

map <- ggplot(cts) + geom_polygon(aes(x = long, y = lat, group = group, fill = ctcol), data = cts, colour = "black")

## changes to the mercator projection, something most people are used to seeing
map <- map + coord_map()

## Change the color scheme of the counties
map <- map + scale_fill_gradientn(colours = rainbow(7))

map <- map + theme(panel.background = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position = "none")

print(map)
graphics.off()
