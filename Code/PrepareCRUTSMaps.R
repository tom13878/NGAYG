# PROJECT: DFID
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# R code to prepare spatial monthly rainfall data from CRU
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 


# INSTALL PACKAGES AND SET WORKING DIRECTORY
BasePackages <- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "plyr", "dplyr")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("RCurl", "XML", "R.utils", "rvest")
lapply(AdditionalPackages, library, character.only = TRUE)

# OPTIONS
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

# SET WORKING DIRECTORY
# Note that rainfall data will be saved automatically in the wd.
wdPath <- "D:\\Data\\IPOP\\CRU_TS_3.22"
setwd(wdPath)

# SOURCE FUNCTIONS

# ANALYSIS
# Obtain country shapefile
iso3c <- "TZA"
countryPath = paste(wdPath, "/", iso3c, sep="") 
if (!file.exists(countryPath)) dir.create(path = countryPath)

# Obtain country coordinates for target country
Get.country.shapefile.f <- function(iso3c, lev=1, proj = "+proj=longlat +datum=WGS84"){
  
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


country.map <- Get.country.shapefile.f("TZA")

# # DOWNLOAD CRU DATA
# Superfluous as it appears that there is one dataset which contains data ofor 1901-2013
# cruRootPath <- "http://www.cru.uea.ac.uk/cru/data/hrg/cru_ts_3.22/cruts.1406251334.v3.22/"
# cruRootHtml <- html(cruRootPath) # get html
# cruRootLinks <- xpathSApply(cruRootHtml, "//td/a/@href") # get http links
# cruRootLinks
# 
# getCRUFileNames.f <- function(data){
#   links <- html(cruRootLinks[[data]]) %>% xpathSApply(., "//td/a/@href")
#   return(links)
# }
# 
# downloadCRUFileNames.f <- function(data, link){
#   url <- paste(cruRootLinks[[data]], link, sep = "/")
#   destfile <- paste(wdPath, "Raw", link, sep="/" )
#   download.file(url, destfile)
# }
# # Download PET files
# petFileNames <- getCRUFileNames.f(4)
# lapply(petFileNames, function(x) downloadCRUFileNames.f(4, x))
# 
# # Downloadt PRE files
# petFileNames <- getCRUFileNames.f(4)
# lapply(petFileNames, function(x) downloadCRUFileNames.f(4, x))


# Precipitation period 2001-2013 (comes in two files)
pre <- stack("./raw/cru_ts3.22.1901.2013.pre.dat.nc")
projection(pre) <- "+proj=longlat +datum=WGS84"

# evapotranspiration 2001-2013 (comes in two files)
pet <- stack("./raw/cru_ts3.22.1901.2013.pet.dat.nc")
projection(pet) <- "+proj=longlat +datum=WGS84"

writeRaster(pre, "./Processed/MonthlyPrecipitation190101_201312", overwrite=TRUE)
writeRaster(pet, "./Processed/MonthlyEvapotranspiration190101_201312", overwrite=TRUE)

# Crop data to country level
pre.country <- crop(pre, country.map)
plot(pre.country[[1]])
pet.country <- crop(pet, country.map)
plot(pet.country)[[1]]

