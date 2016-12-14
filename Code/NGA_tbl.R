#'========================================================================================================================================
#' Project:  Test
#' Subject:  Script to create tables
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
BasePackages<- c("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
#lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("WDI", "countrycode", "frontier", "stargazer")
lapply(AdditionalPackages, library, character.only = TRUE)

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### DATAPATH
dataPath <- "C:\\Users\\dijk158\\OneDrive - IIASA\\SurveyData"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LOAD DATA

# Can be sourced later
# Can be sourced later
db1 <- readRDS(file.path(root, "Cache/db1.rds"))
db9_harv <- readRDS(file.path(root, "Cache/db9_harv.rds"))
#db_sfaCD_CRE_Z <- readRDS("Cache/db_sfaCD_CRE_Z.rds")
#Prices <- readRDS("Cache/Prices_ETH.rds") %>% rename(Pm = maize, Pn = fertilizer)
#source("Code/waterfall_plot.R")
source(file.path(root, "Code/sfaTable.R"))


# Number of unique households
#length(unique(db9$hhid))

# summary statistics
dbsum <- db1 %>% dplyr::select(Yield = yld_harv, Elevation = elevation, 
                                       yesN, Nitrogen = N_harv, SOC = SOC2, phdum, 
                                       Area = area_harv, Assets = asset, crop_count2) 

# dbsum <- db_sfaCD_CRE_Z %>% dplyr::select(Yield = yld, ImprovedSeeds = impr, Slope = slope, 
#                                       yesN, Nitrogen = N, Rain = rain_wq, Irrigation = irrig, SOC = SOC2, phdum, 
#                                       Labour = lab,  Area = area, crop_count2, AEZ, surveyyear2) 

#stargazer(as.data.frame(dbsum), type = "text", digits=2, out=".\\FigTab\\summaryStat.html")


### SFA ANALYSIS
sfaCD1_harv <- sfa(logyld_harv ~ noN + logN_harv + 
                     logasset + logae_harv + 
                     logseedq_harv +
                     logarea_gps +
                     pestherb +
                     irrig +
                     #mech + 
                     antrac + 
                     slope + elevation +
                     SOC2 + phdum2 + 
                     rain_wq + rain_wq2+
                     crop_count2 + surveyyear2,
                   data = db1, maxit = 1500, restartMax = 20, printIter = 1, tol = 0.000001)

summary(sfaCD1_harv, extraPar = TRUE)
lrtest(sfaCD1_harv)

sfaCD1_gps <- sfa(logyld_gps ~ noN + logN_gps + 
                    logasset + logae_gps + 
                    logseedq_gps +
                    logarea_gps +
                    pestherb +
                    irrig +
                    #mech + 
                    antrac + 
                    slope + elevation +
                    SOC2 + phdum2 + 
                    rain_wq + rain_wq2+
                    crop_count2 + surveyyear2,
                  data = db1, maxit = 1500, restartMax = 20, printIter = 1, tol = 0.000001)

summary(sfaCD1_gps, extraPar = TRUE)
lrtest(sfaCD1_gps)


Tbl_sfaCD1_harv <- sfaTable_f(sfaCD1_harv) %>%
  dplyr::select(-variable)
Tbl_sfaCD1_gps <- sfaTable_f(sfaCD1_gps) %>%
  mutate(variable = gsub("_gps", "", variable))

Tbl_sfa <- bind_cols(Tbl_sfaCD1_gps, Tbl_sfaCD1_harv)

olsCD1_gps <- lm(logyld_gps ~ noN + logN_gps + logasset + logae_gps + 
                   logseedq_gps +
                   logarea_gps +
                   mech + antrac + 
                   #inter_crop +
                   slope + elevation +
                   SOC2 + phdum2 + 
                   rain_wq + rain_wq2+
                   crop_count2 + surveyyear2,
                 data = db1)


stargazer(olsCD1_gps, type="text")



