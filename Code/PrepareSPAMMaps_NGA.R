#########################################################################################################
####################################### PROJECT: IPOP ###################################################
#########################################################################################################
# Purpose: Extract data from IFPRI SPAM and sum to regions/district level
#########################################################################################################
#########################################################################################################

# PACKAGES
BasePackages<- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "plyr", "dplyr", "haven", "tidyr")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c()
lapply(AdditionalPackages, library, character.only = TRUE)

# SET WORKING DIRECTORY
wdPath <- "D:\\Data\\IPOP\\SPAM\\"
dataPath <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap\\Analysis\\NGA\\Data"
setwd(wdPath)

# R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

# SOURCE FILES

# FUNCTIONS
# Obtain country coordinates for target country
Get.country.shapefile.f <- function(iso3c, lev=0, proj = "+proj=longlat +datum=WGS84"){
  
  #download country boundary as spatialpolygonDF (and rewrite as .shp for convenience)
  targetfile <- paste(iso3c, paste("_adm", lev, ".Rds", sep=""), sep="")
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

# Function to crop maps to target country
cropmaps.f <-function(file){ 
  setwd("./Raw")
  print(file)
  raster(file) %>%  
    crop(., countryMap) %>%
    writeRaster(., paste(countryPath, paste(iso3c, file, sep="_"), sep="\\"), overwrite=TRUE)
  setwd(wdPath)
}

# SET TARGET COUNTRY
iso3c <-"NGA"

# CREATE COUNTRY FOLDER AND SET WORKING DIRECTORY
countryPath = paste(getwd(), "Processed", iso3c, sep="/") 
if (!file.exists(countryPath)) dir.create(path = countryPath)

# GET COUNTRY MAP
#countryMap <- Get.country.shapefile.f(iso3c, lev=1)
countryMap <- readRDS(file.path(dataPath, "Basemap/GADM_2.8_NGA_adm1.rds"))

# CROP MAIZE MAPS TO TARGET COUNTRY
SPAMfiles <- list.files(paste(wdPath, "RAW", sep="//"))
l_ply(SPAMfiles, cropmaps.f)

# CREATE ZONE MAP
countryMap@data$zone_lsms[countryMap@data$NAME_1 %in% c("Benue", "Federal Capital Territory", "Kogi", "Niger", "Kwara", "Nassarawa", "Plateau")] <- "NORTH CENTRAL"
countryMap@data$zone_lsms[countryMap@data$NAME_1 %in% c("Bauchi", "Taraba", "Adamawa", "Borno", "Gombe", "Yobe")] <- "NORTH EAST"
countryMap@data$zone_lsms[countryMap@data$NAME_1 %in% c("Kaduna", "Katsina", "Zamfara", "Jigawa", "Kano", "Kebbi", "Sokoto")] <- "NORTH WEST"
countryMap@data$zone_lsms[countryMap@data$NAME_1 %in% c("Abia", "Ebonyi", "Imo", "Anambra", "Enugu")] <- "SOUTH EAST"
countryMap@data$zone_lsms[countryMap@data$NAME_1 %in% c("Akwa Ibom", "Cross River", "Rivers", "Bayelsa", "Delta", "Edo")] <- "SOUTH SOUTH"
countryMap@data$zone_lsms[countryMap@data$NAME_1 %in% c("Ekiti", "Ondo", "Oyo", "Ogun", "Osun", "Lagos")] <- "SOUTH WEST"
countryMap@data$zone <- factor(countryMap@data$zone)

# Create new polygon map
zoneCountryMap <- unionSpatialPolygons(countryMap, countryMap@data$zone)
plot(zoneCountryMap)

# Link back zone names
pid <- sapply(slot(zoneCountryMap, "polygons"), function(x) slot(x, "ID"))
p.df <- data.frame(ID=1:length(zoneCountryMap), row.names = pid, zone =  pid)     
zoneCountryMap <- SpatialPolygonsDataFrame(zoneCountryMap, p.df)

# Calculate total (irrigated plus rainfed) production per zone
prodZone <-raster(paste(".\\Processed\\", iso3c, "\\NGA_spam2005v2r0_production_maize_total.tif", sep="")) %>%
                  raster::extract(., zoneCountryMap, df=T) %>% dplyr::rename(Production = NGA_spam2005v2r0_production_maize_total) %>%
                  dplyr::group_by(ID) %>%
                  dplyr::summarize(Production = sum(Production, na.rm=T)) %>%
                  left_join(., p.df)

# Calculate weighted total yield per zone
Zone_NGA <-stack(paste(".\\Processed\\", iso3c, "\\NGA_spam2005v2r0_harvested-area_maize_total.tif", sep=""), 
                  paste(".\\Processed\\", iso3c, "\\NGA_spam2005v2r0_yield_maize_total.tif", sep=""),
                  paste(".\\Processed\\", iso3c, "\\NGA_spam2005v2r0_production_maize_total.tif", sep="")) %>%
  raster::extract(., zoneCountryMap, df=T) %>%
  dplyr::rename(area = NGA_spam2005v2r0_harvested.area_maize_total, 
                yield = NGA_spam2005v2r0_yield_maize_total,
                production = NGA_spam2005v2r0_production_maize_total) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarize(production = sum(production, na.rm=T), yield = sum(area *yield, na.rm=T)/sum(area, na.rm=T)) %>%
  left_join(., p.df)

# Calculate total maize production
# Note that SPAM is calibrated on FAO data for 2004-2006, not 2005.
prodTotalSPAM <- sum(Zone_NGA$production, na.rm=T)

# Load maize production data from FAOSTAT 
prodFAO <- read.csv(paste("Processed", iso3c, "NGA_MaizeProduction.csv", sep="//"))

# Compute scaling factor between SPAM and TZA survey period: 2010-2012 survey takes place in these years
scalingFactor <- prodFAO %>% 
              filter(Year %in% c(2010:2012)) %>%
              dplyr::summarize(scalingFactor = mean(Value, na.rm=T)/prodTotalSPAM)

scalingFactor <- scalingFactor$scalingFactor

# Compute total production per zone for target period
Zone_NGA$TargetProduction <- Zone_NGA$production*scalingFactor

# Write file
write.csv(Zone_NGA, file=file.path(dataPath, "SPAMdata_NGA.csv"), row.names=F)
 