#'========================================================================================================================================
#' Project:  CIMMYT
#' Subject:  Script to create maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
BasePackages<- c("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
lapply(SpatialPackages, library, character.only = TRUE)
#AdditionalPackages <- c("WDI", "countrycode)
#lapply(AdditionalPackages, library, character.only = TRUE)

### DETERMINE ROOT DIRECTORY
root <- find_root(is_rstudio_project)

### DATAPATH
dataPath <- "C:\\Users\\dijk158\\OneDrive - IIASA\\SurveyData"
dataPath <- "/Users/MMHOME/OneDrive - IIASA/SurveyData"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LOAD DATA
# Can be sourced later
db1 <- readRDS(file.path(root, "Cache/db1.rds"))
db9_harv <- readRDS(file.path(root, "Cache/db9_harv.rds"))
db9_gps <- readRDS(file.path(root, "Cache/db9_gps.rds"))

### DEFINE TARGET CLIMATEZONES
# Select target zones as CLIMATEZONES > 50 plot observations
CZ_target <- db9_gps %>%
  ungroup() %>%
  dplyr::select(CLIMATEZONE, hhid, plotid) %>%
  group_by(CLIMATEZONE) %>%
  summarise(NumberHH = length(unique(hhid)),
            Numberplot = n())
CZ_target <- CZ_target$CLIMATEZONE[CZ_target$Numberplot >= 50] 
CZ_target <- droplevels(CZ_target)

### GYGA MAP PREPARATION
# Load polygon map and cut out tartet country
GYGApath <- "D:\\Data\\IPOP\\GYGA\\"
dsn=paste(GYGApath, "\\CZ_SubSaharanAfrica\\CZ_AFRGYGACNTRY.shp", sep="")
ogrListLayers(dsn)
ogrInfo(dsn, layer="CZ_AFRGYGACNTRY")
GYGA.Africa <- readOGR(dsn, layer = "CZ_AFRGYGACNTRY")
projection(GYGA.Africa) # check projection
GYGA.Africa <- spTransform(GYGA.Africa, CRS("+proj=longlat +datum=WGS84")) 

# Cut out GHA from GYGA map
GYGA_country <- GYGA.Africa[GYGA.Africa$REG_NAME=="Nigeria",]
#plot(GYGA_country)
rm(GYGA.Africa)


# Get GYGA data and add YG and conbine CLIMATEZONE and PY
GYGA_yg_df <- read_excel(file.path(GYGApath, "GygaRainfedMaizeSubSaharanAfrica.xlsx"), sheet=3) %>%
  dplyr::filter(CROP=="Rainfed maize", COUNTRY == "Nigeria") %>%
  mutate(YG = (YW-YA)/YW*100,
         YW2 = YW*1000,
         #CZ_YW = paste0(CLIMATEZONE, " (", format(round(YW), nsmall=0, big.mark=","), " kg/ha)"),
         CZ_YW = paste0(CLIMATEZONE, " (", round(YW), " kg/ha)"),
         CZ_YW = factor(CZ_YW, levels = CZ_YW[order(YW2)]))

# Link yield gap data
# in order to merge data with spatialpolygondataframe the row.names need to be the same.
# For this reason I first merge the additionald data and then create a spatialpolygondataframe
# again ensuring that the rownames are preserved.
GYGA_df <- as(GYGA_country, "data.frame") %>%
  rename(CLIMATEZONE = GRIDCODE)
GYGA_df$id <-row.names(GYGA_df)
GYGA_df <- left_join(GYGA_df, GYGA_yg_df)
row.names(GYGA_df) <- GYGA_df$id
GYGA_country <- SpatialPolygonsDataFrame(as(GYGA_country, "SpatialPolygons"),data=GYGA_df)

# Maps with GYGA yield potential and plot information
# transform shapefile in dataframe for ggplot. rownames are used as ID for the countries. Using ID gives strange results. 
# Additional data is linked back again
GYGA_df_f <- fortify(GYGA_country) %>%
  left_join(., GYGA_df)


### COMBINED POTENTIAL YIELD AND LSMS-ISA MAP
# Prepare mean yield data
yld <- db1 %>%
  ungroup() %>%
  group_by(lon, lat) %>%
  summarize(n=n(),
            av_yld = (sum(yld_gps*area_gps)/sum(area_gps))/1000) %>%
  mutate(av_yld2 = cut(av_yld, breaks=c(0, 1, 2, 3, 7)))


# Map with target climate zones and average yield
GYGA_LSMS <- ggplot() +
  coord_equal() +
  labs(
    #title = "Yield gaps and average yield in Nigeria (%)",
    #subtitle = "check",
    #caption = "Source: LSMS-ISA and Global Yield Gap Atlas (www.yieldgap.org)",
    x="", y="") +
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank()) +
  geom_path(data = GYGA_df_f, aes(x = long, y = lat, group = group), colour="black") +
  geom_polygon(data = filter(GYGA_df_f, !is.na(YW2), !(CLIMATEZONE %in% CZ_target)), aes(x = long, y = lat, group = group), fill = "grey", colour="black") +
  geom_polygon(data = filter(GYGA_df_f, !is.na(YW2), CLIMATEZONE %in% CZ_target), aes(x = long, y = lat, group = group, fill = CZ_YW), colour="black") +
  scale_fill_discrete(name = "Climate zone\n (pot. water lim. yield)") +
  geom_point(data = yld, aes(x = lon, y = lat, size = av_yld2), colour="black") +
  scale_size_manual(name = "Average yield (ton/ha)", values = c(1, 2, 3, 4)) 
  
GYGA_LSMS


