## Figured I'd try making a map using shapefiles for the first time in a while.
## install.packages("maptools", dependencies = TRUE)
library(maptools)
loc1 <- "/Users/bjr/GitHub/MapProj/Shp/cb_2015_us_cd114_500k/"
loc2 <- "/Users/bjr/GitHub/MapProj/Shp/cb_2015_us_county_500k/"
rploc <- "/Users/bjr/GitHub/MapProj/"
maindir <- "/Users/bjr/Dropbox/R_Projects/MapSomething/cdcs/"
set.seed(123456)

if(!exists("cdsin")) cdsin <- readShapeSpatial(paste0(loc1, "cb_2015_us_cd114_500k.shp"))
if(!exists("ctsin")) ctsin <- readShapeSpatial(paste0(loc2, "cb_2015_us_county_500k.shp"))
if(!exists("fips")) fips <- read.csv(paste0(rploc, "FIPS Codes by state.csv"))


statetouse <- "Kansas"

fptouse <- fips[which(fips$State.Name == statetouse),"FIPS.Code"]
if(nchar(as.character(fptouse)) == 1) fptouse <- paste0(0,fptouse)



## Get rid of noncontinental FIPS.
## fpstonotuse <- fips$FIPS.Code[fips$State.Name %in% c("Alaska", "Hawaii", "Puerto Rico", "Virgin Islands", "American Samoa", "Guam")]
## fpstonotuse <- unlist(lapply(fpstonotuse, function(x) if(nchar(as.character(x)) == 1) x <- paste0(0,x) else x))


library(ggplot2)
cts <- fortify(ctsin[ctsin$STATEFP == fptouse,], region = "NAME")

## For when we decide we'd prefer to avoid working with a given state
## cts <- fortify(ctsin[!ctsin$STATEFP %in% fpstonotuse,], region = "NAME")
## unique(ctsin$STATEFP[!ctsin$STATEFP %in% fpstonotuse])

cds <- fortify(cdsin[cdsin$STATEFP == fptouse,], region = "CD114FP")
## cds <- fortify(cdsin[!cdsin$STATEFP %in% fpstonotuse,], region = "CD114FP")

## This gets rid of all of the islands that don't have a FIPS
## code. It's a little hacky, but I was low on ideas. I'm keeping it
## out of the if statement because Alaska just had the Aleutian
## Islands showing up, and I can't have that.
cts <- cts[cts$long < -50,]
cds <- cds[cds$long < -50,]


unique(cts$id)

ngrad <- 100


colsamp <- sort(sample(1:ngrad, length(unique(cts$group)), replace = TRUE))

coldf <- data.frame(ctname = unique(cts$group), ctcol = colsamp)

cts <- merge(cts, coldf, by.x = "group", by.y = "ctname", all = TRUE)



 ### Begin mapping
dev.new()
fileproplong <- diff(range(cts$long))
fileproplat <-  diff(range(cts$lat))
pdf(paste0(maindir, "", statetouse, ".pdf"), width = fileproplong * .8, height = fileproplat, )

map <- ggplot(cts) + geom_polygon(aes(x = long, y = lat, group = group, fill = ctcol), data = cts, colour = "black")

## Change the color scheme of the counties
map <- map + scale_fill_gradient(high = 'blue', low = 'green')
## map <- map + scale_fill_gradientn(colours = rainbow(ngrad))

## add the path of the districts over the counties
map <-  map + geom_path(aes(x = long, y = lat, group = group), colour = "red", data = cds, size = .6)

map <- map + coord_map()

map <- map + ggtitle(paste("A Map of", statetouse))

map <- map + theme(panel.background = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), legend.position = "none")

print(map)
graphics.off()
