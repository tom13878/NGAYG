# TZA fertilizer assessment
# R code to obtain SPEI index
# http://sac.csic.es/spei/

# INSTALL PACKAGES AND SET WORKING DIRECTORY
BasePackages <- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "plyr", "dplyr")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)

# OPTIONS
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

# SET WORKING DIRECTORY
# Note that rainfall data will be saved automatically in the wd.
wdPath <- "D:\\Data\\IPOP\\GrowingSeason"
setwd(wdPath)
# SOURCE FUNCTIONS

# SUBSET FILES FOR A TARGET COUNTRY AND WRITE AS RASTERSTACK
# Set target country
iso3c <-"TZA"

# CREATE COUNTRY FOLDER AND SET WORKING DIRECTORY
countryPath = paste(getwd(), "/", iso3c, sep="") 
if (!file.exists(countryPath)) dir.create(path = countryPath)

# Obtain country coordinates for target country
Get.country.shapefile.f <- function(iso3c, lev=0, proj = "+proj=longlat +datum=WGS84"){
  
  #download country boundary as spatialpolygonDF (and rewrite as .shp for convenience)
  targetfile <- paste(iso3c, paste("_adm", lev, ".Rdata", sep=""), sep="")
  if(file.exists(paste(countryPath, targetfile, sep="/"))){
    load(paste(countryPath, targetfile, sep="/"))
  } else {
    gadm=getData('GADM', country=iso3c, level=lev, path=countryPath)
  }
  
  # change projection 
  projection <- proj
  country.sp <- spTransform(gadm, CRS(projection))
  return(country.sp)
}

country.map <- Get.country.shapefile.f(iso3c)

# Growing Season Data
# start growing season. Data has a value 65535 for missing as well as 0, set to NA
stwka <- raster(".\\Raw\\ssa-grseason-stwka.asc")
stwka[stwka==65535]<-0
stwka[stwka==0]<-NA
projection(stwka) <- "+proj=longlat +datum=WGS84"
# End growing season
endwka <- raster(".\\Raw\\ssa-grseason-endwka.asc")
endwka[endwka==0]<-NA
projection(endwka) <- "+proj=longlat +datum=WGS84"

# save file
writeRaster(stwka, ".\\Processed\\stwka", overwrite=TRUE)
writeRaster(endwka, ".\\Processed\\endwka", overwrite=TRUE)

# Crop data to country level
stwka.country <- crop(stwka, country.map)
plot(stwka.country)
endwka.country <- crop(endwka, stwka.country) # for some reason the extent of endwka is slightly larger so stwka is used as mask
wka.country <- stack(stwka.country, endwka.country)
plot(wka.country)
