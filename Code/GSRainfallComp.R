# PROJECT: IPOP/CIMMYT/DFID
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# R code to compare rainfall with crop calendar and growth season data
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# INSTALL PACKAGES AND SET WORKING DIRECTORY
BasePackages <- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "plyr", "dplyr", "tidyr")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("lubridate", "GSIF", "SPEI")
lapply(AdditionalPackages, library, character.only = TRUE)

# OPTIONS
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

# SET WORKING DIRECTORY
#wdpath<-"W:/LEI/Internationaal Beleid  (IB)/Projecten/2271000300 IPOP 3 (new code in 2014-228250008)/Spatial analysis/Data/SoilGrids"
wdPath <- "D:\\Dijk158\\Dropbox\\Michiel_research\\MicroIPOPCode\\TZAYG\\spatial"
setwd(wdPath)


# SOURCE FUNCTIONS

# SET COUNTRY
iso3c <- "TZA"

# Download Basemap
basemapPath = paste(wdPath, "Basemap", sep="\\") 
if (!file.exists(basemapPath)) dir.create(path = basemapPath)

# Obtain country coordinates for target country
Get.country.shapefile.f <- function(iso3c, lev=0, proj = "+proj=longlat +datum=WGS84"){
  
  #download country boundary as spatialpolygonDF (and rewrite as .shp for convenience)
  targetfile <- paste(iso3c, paste("_adm", lev, ".Rdata", sep=""), sep="")
  if(file.exists(paste(basemapPath, targetfile, sep="/"))){
    load(paste(basemapPath, targetfile, sep="/"))
  } else {
    gadm=getData('GADM', country=iso3c, level=lev, path=basemapPath)
  }
  
  # change projection 
  projection <- proj
  country.sp <- spTransform(gadm, CRS(projection))
  return(country.sp)
}

country.map <- Get.country.shapefile.f("TZA", 2)

# PREPARE LSMS SPATIAL DATAPOINTS
# Get y2_hhid-GIS link
LSMSPath <- "W:\\LEI\\Internationaal Beleid  (IB)\\Projecten\\2285000066 Africa Maize Yield Gap\\SurveyData\\TZA"

HH.geo <- read.dta(paste(LSMSPath, "./2010/Stata/TZNPS2GEODTA/HH.Geovariables_Y2.dta", sep="\\"),
                   convert.factors = TRUE)
plot.geo <- read.dta(paste(LSMSPath, "./2010/Stata/TZNPS2GEODTA/Plot.Geovariables_Y2.dta", sep="\\"),
                     convert.factors = TRUE)

# Create list of plots, hh, eas and coordinates
geo.base <- left_join(plot.geo, HH.geo) %>%
  transmute(ea_id, lat = lat_modified, lon = lon_modified) %>%
  unique()

# Create spatial points 
standardproj<-"+proj=longlat +datum=WGS84"
geo.coord <- geo.base %>% 
  dplyr::select(lon, lat) %>%
  SpatialPoints(., proj4string=CRS(standardproj))

# REGION INFORMATION
geo.region  <- over(geo.coord, country.map) %>%
  cbind(geo.base,.) %>%
  dplyr::select(-PID, -ID_0, -ISO, -NAME_0, -NL_NAME_2, -VARNAME_2, -TYPE_2, -ENGTYPE_2)  


# GROWING SEASON
GrowingSeasonPath <- "D:\\Data\\IPOP\\GrowingSeason\\Processed"
stwka <-raster(paste(GrowingSeasonPath, "stwka", sep="\\"))
endwka <-raster(paste(GrowingSeasonPath, "endwka", sep="\\"))

# Extract data
# Note that the extent is slightly different between stwka and endwka so we extract data separately
geo.stwka <- raster::extract(stwka, geo.coord) %>%
  cbind(geo.base,.) 
names(geo.stwka)[4] <- "stwka"

geo.endwka <- raster::extract(endwka, geo.coord) %>%
  cbind(geo.base,.) 
names(geo.endwka)[4] <- "endwka"

geo.wka <- left_join(geo.stwka, geo.endwka) %>%
  mutate(stday = stwka*7,
         endday = endwka*7)

# CROP CALENDAR
CropCalendarPath <- "D:\\Data\\IPOP\\CropCalendar\\Processed"
cc <-stack(paste(CropCalendarPath, "CropCalendar", sep="\\"))

# Extract data
# Note that the extent is slightly different between stwka and endwka so we extract data separately
geo.cc <- raster::extract(cc, geo.coord) %>%
  cbind(geo.base,.) %>%
  rename(start_planting = Maize..Start.of.planting, end_planting = Maize..End.of.planting,
         start_harvest = Maize..Start.of.harvest, end_harvest = Maize..End.of.harvest)
#%>%
# gather(cc, day, -ea_id, -lon, -lat) 

# MONTHLY RAINFALL DATA
rainfallPath2 <- "D:\\Data\\IPOP\\CRU_TS_3.22\\Processed"
pet <-brick(paste(rainfallPath2, "MonthlyEvapotranspiration201101_201312", sep="\\"))
pre <-brick(paste(rainfallPath2, "MonthlyPrecipitation201101_201312", sep="\\"))

# extract data
geo.pet <- raster::extract(pet, geo.coord) %>%
  cbind(geo.base,.) %>% 
  gather(date, pet, X2001.01.16:X2013.12.16)

geo.pre <- raster::extract(pre, geo.coord) %>%
  cbind(geo.base,.) %>% 
  gather(date, pre, X2001.01.16:X2013.12.16) %>%
  mutate(date = gsub("X","", date), 
         date = ymd(date),
         year = year(date),
         month = month(date, label=TRUE),
         day = day(date),
         start = ymd(paste(year, 01, 01, sep="-")),
         span = as.integer(date-start))
         

# DAILY RAINFALL DATA
# Extract rainfall data and link to GIS coordinates of plots/housholds
rainfallPath <- "D:\\Data\\IPOP\\Rainfall\\Processed"
rainfall <-brick(paste(rainfallPath, "DailyRainfall20110301_20110531", sep="\\"))

# extract data
geo.rainfall <- raster::extract(rainfall, geo.coord) %>%
  cbind(geo.base,.) %>%
  gather(date, rain, Date_2011_3_1:Date_2011_5_31) %>%
  mutate(date = gsub("Date_","", date), 
         date = ymd(date),
         year = year(date),
         month = month(date, label=TRUE),
         day = day(date))

# BIND ALL SPATIAL INFORMATION
# Compare CropCalendar and GrowingSeason wih rainfall data
# Plot rainfall and dates for selected regions (one of each type)
# Conclusion: start of the harvest is in line with secondary sources
# For all regions it is between half May - half June. As we only
# have monthly rain data, we calculate the SPEI for Mar-May which
# both covers the Masika and Msimu rainfall seasons.
# Growing season data does not compare well with rainfall and crop calendar maps
# and is therefore dropped.

geo.comp <- left_join(geo.region, geo.pre) %>% left_join(., geo.cc) %>% left_join(., geo.wka) 
set <- ddply(geo.comp,.(NAME_1, year, month), function(x) x[1,])
set <- set[complete.cases(set),]
                  
ggplot(set) + geom_line(aes(x = span, y = pre, group = year, colour=factor(year))) + 
                    facet_wrap(~NAME_1) + 
                    
                    geom_vline(aes(xintercept=start_planting)) +
                    geom_text(aes(x=start_planting,y=Inf,label = "start_planting"), vjust = 1, size=3) +
                    
                    #geom_vline(aes(xintercept=end_planting)) +
                    #geom_text(aes(x=end_planting,y=Inf,label = "end_planting", vjust = 1, size=2)) +
                    
                    geom_vline(aes(xintercept=start_harvest)) +
                    geom_text(aes(x=start_harvest,y=Inf,label = "start_harvest"), vjust = 1, size=3) 
                  
                  #geom_vline(aes(xintercept=end_harvest)) +
                  #geom_text(aes(x=end_harvest,y=Inf,label = "end_harvest", vjust = 1, size=2)) +
                  
                  geom_vline(aes(xintercept=stday), colour="green") +
                    geom_text(aes(x=stday, y=Inf,label = "start gs"), vjust = 1, size=3) +
                    
                    geom_vline(aes(xintercept=endday), colour="red") +
                    geom_text(aes(x=endday,y=Inf,label = "end gs"), vjust = 1, size=3)
                  