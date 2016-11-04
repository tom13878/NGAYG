# TZA fertilizer assessment
# R code to obtain SPEI index
# http://sac.csic.es/spei/

# INSTALL PACKAGES AND SET WORKING DIRECTORY
BasePackages <- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "plyr", "dplyr")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("SPEI")
lapply(AdditionalPackages, library, character.only = TRUE)

# OPTIONS
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

# SET WORKING DIRECTORY
# Note that rainfall data will be saved automatically in the wd.
wd.path <- "D:\\Data\\IPOP\\CRU_TS_3.22"

# SOURCE FUNCTIONS

# ANALYSIS
# Obtain data
iso3c <- "TZA"
country.path = paste(wd.path, "/", iso3c, sep="") 
if (!file.exists(country.path)) dir.create(path = country.path)
setwd(wd.path)

# Obtain country coordinates for target country
Get.country.shapefile.f <- function(iso3c){
  
  #download country boundary as spatialpolygonDF (and rewrite as .shp for convenience)
  targetfile <- paste(iso3c, "_adm0.Rdata", sep="")
  if(file.exists(paste(country.path, targetfile, sep="/"))){
    load(paste(country.path, targetfile, sep="/"))
  } else {
    gadm=getData('GADM', country=iso3c, level=0, path=country.path)
  }
  
  # change projection 
  projection <- "+proj=longlat +datum=WGS84"
  country.sp <- spTransform(gadm, CRS(projection))
  return(country.sp)
}

TZA.map <- Get.country.shapefile.f("TZA")

# Download data
# Precipitation period 2001-2013 (comes in two files)
pre <- stack(stack("cru_ts3.22.2001.2010.pre.dat.nc"), stack("cru_ts3.22.2011.2013.pre.dat.nc"))
projection(pre) <- "+proj=longlat +datum=WGS84"

# evapotranspiration 2001-2013 (comes in two files)
pet <- stack(stack("cru_ts3.22.2001.2010.pet.dat.nc"), stack("cru_ts3.22.2011.2013.pet.dat.nc"))
projection(pet) <- "+proj=longlat +datum=WGS84"

# Crop data to country level
pre.TZA <- crop(pre, TZA.map)
pet.TZA <- crop(pet, TZA.map)


# Compute SPEI
# Compute Difference precipitation and pet
D <- pre.TZA - pet.TZA



geo.D <- raster::extract(D, geo.coord) 
geo.D2 <- geo.D[!is.na(geo.D)]
spei(geo.D2, 1)
%>%
  cbind(geo.base,.)

# Crop calendar data
ccPath <- "D:\\Data\\IPOP\\CropCalendar" 
setwd(ccPath)

get_nc_f <- function(nc, var, proj = "+proj=longlat +datum=WGS84"){
  rasterfile <- raster(nc, varname = var)
  projection(rasterfile) <- proj
  return(rasterfile)
  
}

plant_start <- get_nc_f("Maize.crop.calendar.fill.nc", "plant.start")
plant_end <- get_nc_f("Maize.crop.calendar.fill.nc", "plant.end")
harvest_start <- get_nc_f("Maize.crop.calendar.fill.nc", "harvest.start")
harvest_end <- get_nc_f("Maize.crop.calendar.fill.nc", "harvest.end")

cc <- stack(plant_start, plant_end, harvest_start, harvest_end)
cc
plot(cc)
plant_end
harvest_start <- raster("Maize.crop.calendar.fill.nc", varname = "harvest.start")
harvest_end <- harvest_start <- raster("Maize.crop.calendar.fill.nc", varname = "harvest.end")

plant_start
TZA.map <- getData('GADM', country="TZA", level=0)
TZA.map <- spTransform(TZA.map, CRS("+proj=longlat +datum=WGS84"))
cc.TZA <- crop(cc, TZA.map)
plot(cc.TZA)

nbands(check)
bandnr(check)
check


# cpc_get_rawdata(2011, 3, 1, 2011, 5, 31) 

# Read data, add projection and change coordinates to -180/180 instead of 0/360
# The 0/360 projection is often used in CC models and data but not relevant here.
prep.rainfall.f <- function(y, m, d){
  layer.date <- paste("Date", y, m, d, sep="_")
  TMP <- cpc_read_rawdata(y, m, d)
  TMP <- rotate(TMP)
  standardproj<-"+proj=longlat +datum=WGS84"
  projectRaster(TMP, crs=standardproj) # check if this works and if necessary because it might take a long time
  names(TMP) <- layer.date 
  return(TMP)
}

# Read filenames and get dates
filenames <- list.files()
# Select bin datafiles and get date information from binfiles
bindates <- filenames[grep(pattern=".bin", filenames)] %>% str_extract(., "\\d+")
Rainfall <- lapply(bindates, function(x) {
                prep.rainfall.f(as.numeric(str_sub(x, 1, 4)),
                                as.numeric(str_sub(x, 5, 6)),
                                as.numeric(str_sub(x, 7, 8))
                                )}) %>%
                do.call(stack,.)
writeRaster(Rainfall, "DailyRainfall30110301_20110531", overwrite=TRUE)
Rainfall <- brick("DailyRainfall30110301_20110531")

# Crop TZA from world rainfall data map
TZA.map <- getData('GADM', country="TZA", level=0)
TZA.map <- spTransform(TZA.map, CRS("+proj=longlat +datum=WGS84"))
Rainfall.TZA <- crop(Rainfall, TZA.map)
plot(Rainfall.TZA)
