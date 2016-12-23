#'========================================================================================================================================
#' Project:  Test
#' Subject:  Test
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
BasePackages<- c("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "haven")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("GSIF", "SPEI", "haven", "assertive", "countrycode", "sjmisc")
lapply(AdditionalPackages, library, character.only = TRUE)

### SET WORKING DIRECTORY
dataPath <- "C:\\Users\\dijk158\\OneDrive - IIASA\\SurveyData\\Other\\Spatial"
setwd(wdPath)

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### SET WORKING DIRECTORY
dataPath <- "C:\\Users\\dijk158\\OneDrive - IIASA\\SurveyData\\Other\\Spatial"
setwd(dataPath)


### SET COUNTRY AND YEAR
iso3c <- "NGA"

### FUNCTIONS

# Obtain country coordinates for target country
GADM_f <- function(iso3c, lev=0, proj = "+proj=longlat +datum=WGS84"){
  gadm = getData('GADM', country=iso3c, level=lev, path= file.path(dataPath, iso3c))
  # change projection 
  projection <- proj
  country.sp <- spTransform(gadm, CRS(projection))
  return(country.sp)
}

NGA_adm0 <- GADM_f(iso3c, 0)
NGA_adm1 <- GADM_f(iso3c, 1)
NGA_adm2 <- GADM_f(iso3c, 2)

