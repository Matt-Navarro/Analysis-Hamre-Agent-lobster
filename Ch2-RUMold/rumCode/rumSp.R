
# important librarys for spatail analysis
library(sp) 
library(rgdal)
library(raster)
library(sf)

### Set directories ----
#w.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
w.dir <- "G:/My Drive/MEG Students/PhD_Nicole Hamre/R_spatial"
d.dir <- paste(w.dir, "data", sep='/')
s.dir <- paste(w.dir, "shapefiles", sep='/')
r.dir <- paste(w.dir, "rasters", sep='/')
p.dir <- paste(w.dir, "plots", sep='/')


# Read gb cmr poly ----
gb <- readOGR(paste(s.dir, "GeoBay.shp",sep='/')) ## reading shp file
plot(gb) # plotting shp file
crs1 <- proj4string(gb) # check CRS 

# you can explore the attribute table like a normal df
str(gb) 
levels(gb$ZoneName) 

# get poly for each zone --
NPZ <- gb[gb$ZoneName=="National Park Zone",]
HPZ <- gb[gb$ZoneName=="Habitat Protection Zone",]
MUZ <- gb[gb$ZoneName=="Multiple Use Zones",]
SPZ <- gb[gb$ZoneName=="Special Purpose Zone (Mining Exclusion)",]

plot(NPZ)
plot(SPZ)

bruvs <- readOGR(paste(s.dir, "Bruv_CMR.shp",sep='/')) # point data
plot(bruvs)

plot(gb)
points(bruvs)
bruvs 
str(bruvs)
plot(bruvs,pch = 21, bg = bruvs$ZonNm, add=T)
bruvs$ZonNm <- as.factor(bruvs$ZonNm)

dir(r.dir)
bathy <- raster(paste(r.dir, "Geog_250mBathy.tif",sep='/')) # creating raster layer
plot(bathy)
plot(gb, add=T)

# crop rasters
plot(HPZ)
hpzb <- crop(bathy, HPZ)
plot(hpzb)  

# change crs of sp object

crsb <- proj4string(bathy)
proj4string(gb2)

gb2 <- spTransform(gb, crsb)
gb2

# crop a specific extent ---
plot(bathy)
e <- drawExtent()
bathy2 <- crop(bathy, e)
plot(bathy2)


# extract values from a raster file ---
plot(bathy)
plot(bruvs, add=T)
bruvs_bathy <- extract(bathy, bruvs, sp =T)
bruvs_bathy
 
bdf <- as.data.frame(bruvs_bathy)
head(bdf)
