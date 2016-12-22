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

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LOAD DATA
# Can be sourced later
db1 <- readRDS(file.path(root, "Cache/db1.rds"))
db9_harv <- readRDS(file.path(root, "Cache/db9_harv.rds"))
#db_sfaCD_CRE_Z <- readRDS("Cache/db_sfaCD_CRE_Z.rds")
#Prices <- readRDS("Cache/Prices_ETH.rds") %>% rename(Pm = maize, Pn = fertilizer)
#source("Code/waterfall_plot.R")


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
plot(GYGA_country)
rm(GYGA.Africa)
unique(GYGA_country@data$ID)

# Get GYGA data and add YG
GYGA_yg_df <- read_excel(file.path(GYGApath, "GygaNigeria.xlsx"), sheet=3) %>%
  dplyr::filter(CROP=="Rainfed maize") %>%
  mutate(YG = (YW-YA)/YW*100)

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

### GYGA YIELD GAP 
# Draw map
GYGA_YG <- ggplot() +
  geom_polygon(data = filter(GYGA_df_f, is.na(YG)), aes(x = long, y = lat, group = group, fill = YG), colour = "black")+
  geom_polygon(data = filter(GYGA_df_f, is.na(YG)), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  #scale_fill_gradientn(colours = terrain.colors(10)) +
  #scale_fill_gradient(low = "light green", high = "dark green") +
  scale_fill_distiller(palette = "Spectral", name = "%") +
  coord_equal() +
  labs(
    title = "Water-limited yield gap in Nigeria (%)",
    #subtitle = "check",
    caption = "Source: Global Yield Gap Atlas (www.yieldgap.org)",
    x="", y="") +
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank())
GYGA_YG

### COMBINED YIELD GAP AND LSMS-ISA MAP

# Add clases for YW
unique(GYGA_df_f$YW)
GYGA_df_f$YW2 <- cut(GYGA_df_f$YW, breaks=c(5, 7, 9, 11, 13, 15))

# Prepare mean yield data
yld <- db9_harv %>%
  group_by(lon, lat) %>%
  summarize(n=n(),
            av_yld = (sum(Y*area_harv)/sum(area_harv))/1000) %>%
  filter(n>1)
yld$av_yld2 <- cut(yld$av_yld, breaks=c(0, 1, 2, 3, 13))

# Draw map
GYGA_LSMS <- ggplot()+
  geom_polygon(data = filter(GYGA_df_f, !is.na(YW2)), aes(x = long, y = lat, group = group, fill = YW2), colour="black")+
  geom_polygon(data = filter(GYGA_df_f, is.na(YW2)), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  scale_fill_discrete(name = "Potential water\nlimited yield (tons)") +
  geom_point(data = yld, aes(x = lon, y = lat, size = av_yld2), colour="black") +
  scale_size_manual(name = "Average yield (tons)", values = c(1, 2, 3, 4)) +
  coord_equal() +
  labs(
    title = "Yield gaps and average yield in Nigeria (%)",
    #subtitle = "check",
    caption = "Source: LSMS-ISA and Global Yield Gap Atlas (www.yieldgap.org)",
    x="", y="") +
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank())

#GYGA_LSMS
#ggsave(plot = GYGA_LSMS, ".\\Graphs\\GYGA_LSMS.png", height = 150, width = 200, type = "cairo-png", units="mm")


### ADMINISTRATIVE MAPS
# Zonal map with community yield levels
countryMap <- readRDS(file.path(dataPath, "Other/Spatial/NGA/GADM_2.8_NGA_adm1.rds"))

# Rename zones using LSMS names
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Benue", "Federal Capital Territory", "Kogi", "Niger", "Kwara", "Nassarawa", "Plateau")] <- "North Central"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Bauchi", "Taraba", "Adamawa", "Borno", "Gombe", "Yobe")] <- "North East"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Kaduna", "Katsina", "Zamfara", "Jigawa", "Kano", "Kebbi", "Sokoto")] <- "North West"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Abia", "Ebonyi", "Imo", "Anambra", "Enugu")] <- "South East"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Akwa Ibom", "Cross River", "Rivers", "Bayelsa", "Delta", "Edo")] <- "South South"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Ekiti", "Ondo", "Oyo", "Ogun", "Osun", "Lagos")] <- "South West"
countryMap@data$ZONE <- factor(countryMap@data$ZONE)
countryMapData <- countryMap@data

# Fortify spatial data to use with ggplot and join using join functions from dplyr
# The join is on id, make sure all ids are character vectors
countryMap@data <- rename(countryMap@data, id = ID_1)
countryMap@data$id <- as.character(countryMap@data$id)
tf <- fortify(countryMap)
tf2 <- left_join(tf, countryMap@data)

# Use ggplot to plot map of Tanzania, specifying the labels and choosing nice colours
# from the RColorBrewer package
ADM1 <- ggplot() +
  geom_polygon(data = tf2, aes(x = long, y = lat, group = group, fill = ZONE), colour = "black")+
  geom_point(data = yld, aes(x = lon, y = lat, size = (av_yld2)), colour = "black")+
  scale_fill_brewer(name = "Zones", palette = "Set1") +
  scale_size_manual(name="Average yield (tons)", values=c(1.5, 2.5, 3.5, 4,5)) +
  coord_equal()+
  labs(
    title = "Zones and average yield in Nigeria (%)",
    #subtitle = "check",
    caption = "Source: LSMS-ISA",
    x="", y="") +
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank())
#ADM1 

# Save map
#ggsave(plot = ADM1, ".\\Graphs\\ADM1.png", height = 150, width = 200, type = "cairo-png", units="mm")

# Save image for use in Rmarkdown
#save.image("Cache/FigTab.Rdata")

