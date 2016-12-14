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
source(file.path(root, "Code/sfaTable.R"))

