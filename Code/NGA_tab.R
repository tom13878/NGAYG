#'========================================================================================================================================
#' Project:  CIMMYT yield gap
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
# Data files
db1 <- readRDS(file.path(root, "Cache/db1.rds"))
db9_harv <- readRDS(file.path(root, "Cache/db9_harv.rds"))
db9_gps <- readRDS(file.path(root, "Cache/db9_gps.rds"))
dbP <- readRDS(file.path(root, "Cache/Pooled_NGA.rds"))
source(file.path(root, "Code/sfaTable.R"))

# Names of variable
var_names <- read_csv(file.path(root, "FigTabMap/var_names.csv"))


### SFA ANALYSIS
sfaCD1_harv <- sfa(logyld_harv ~ noN + logN_gps + 
                     logasset + logae_gps + 
                     logseedq_gps +
                     logarea_gps +
                     pestherb +
                     #irrig +
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
                    #irrig +
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

### CROSSTABLE FARMING SYSTEMS AND CLIMATE ZONE
# with(db9_harv, table(fs, CLIMATEZONE))

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

### TABLE WITH KEY STATISTICS PER CLIMATE ZONE
# Prepare dataframe: select variable add YW to db1, change units and set order of variables
Tab_CZ_df <- dbP %>% dplyr::select(hhid, plotid, surveyyear, YW) %>% 
  na.omit %>% unique() %>%
  left_join(db1,.) %>%
  do(filter(., complete.cases(.))) %>%
  ungroup() %>%
  dplyr::select(CLIMATEZONE, hhid, plotid, yld_gps, yld_harv, yesN, N_gps, area_gps, area_harv, 
                pestherb, antrac, seedq_gps, ae, YW, SOC2, rain_wq, seed_q, 
                slope, elevation, crop_count2, assetph) %>%
  mutate(assetph = assetph/1000,
         YW = YW*1000,
         area_gpsX = ifelse(yesN == 1, area_gps, NA)) 
 
Tab_CZ <- bind_rows(
  Tab_CZ_df %>%
    filter(CLIMATEZONE %in% CZ_target) %>%  
    group_by(CLIMATEZONE) %>%
    summarise(yld_gps = sum(yld_gps*area_gps, na.rm=TRUE)/ sum(area_gps),
              #Yield_UW_gps = mean(yld_gps, na.rm=TRUE),
              yld_harv = sum(yld_harv*area_harv, na.rm=TRUE)/ sum(area_harv),
              #Yield_UW_harv = mean(yld_harv, na.rm=TRUE),
              Ncon = sum(N_gps*area_gpsX, na.rm=TRUE)/sum(area_gpsX, na.rm=TRUE),
              yesN = round(mean(yesN)*100, digits=1),
              area_gps = mean(area_gps, na.rm=TRUE),
              area_harv = mean(area_harv, na.rm=TRUE),
              NumberHH = length(unique(hhid)),
              Numberplot = n(),
              SOC2 = mean(SOC2, na.rm = TRUE),
              rain_wq = mean(rain_wq, na.rm = TRUE),
              seedq_gps = mean(seedq_gps),
              assetph = mean(assetph, na.rm = TRUE),
              ae = mean(ae/area_gps, na.rm = TRUE),
              pestherb = mean(pestherb, na.rm = TRUE),
              antrac = mean(antrac, na.rm = TRUE),
              slope = mean(slope, na.rm = TRUE),
              elevation = mean(elevation, na.rm = TRUE),
              crop_count2 = mean(crop_count2, na.rm = TRUE),
              YW = mean(YW)
    ),
  Tab_CZ_df %>%
    filter(!(CLIMATEZONE %in% CZ_target)) %>%
    summarise(CLIMATEZONE = "Other zones",
              yld_gps = sum(yld_gps*area_gps, na.rm=TRUE)/ sum(area_gps),
              #Yield_UW_gps = mean(yld_gps, na.rm=TRUE),
              yld_harv = sum(yld_harv*area_harv, na.rm=TRUE)/ sum(area_harv),
              #Yield_UW_harv = mean(yld_harv, na.rm=TRUE),
              Ncon = sum(N_gps*area_gpsX, na.rm=TRUE)/sum(area_gpsX, na.rm=TRUE),
              yesN = round(mean(yesN)*100, digits=1),
              area_gps = mean(area_gps, na.rm=TRUE),
              area_harv = mean(area_harv, na.rm=TRUE),
              NumberHH = length(unique(hhid)),
              Numberplot = n(),
              SOC2 = mean(SOC2, na.rm = TRUE),
              rain_wq = mean(rain_wq, na.rm = TRUE),
              seedq_gps = mean(seedq_gps),
              assetph = mean(assetph, na.rm = TRUE),
              ae = mean(ae/area_gps, na.rm = TRUE),
              pestherb = mean(pestherb, na.rm = TRUE),
              antrac = mean(antrac, na.rm = TRUE),
              slope = mean(slope, na.rm = TRUE),
              elevation = mean(elevation, na.rm = TRUE),
              crop_count2 = mean(crop_count2, na.rm = TRUE),
              YW = mean(YW)
    ),
  Tab_CZ_df %>%
    summarise(CLIMATEZONE = "Total",
              yld_gps = sum(yld_gps*area_gps, na.rm=TRUE)/ sum(area_gps),
              #Yield_UW_gps = mean(yld_gps, na.rm=TRUE),
              yld_harv = sum(yld_harv*area_harv, na.rm=TRUE)/ sum(area_harv),
              #Yield_UW_harv = mean(yld_harv, na.rm=TRUE),
              yesN = round(mean(yesN)*100, digits=1),
              Ncon = sum(N_gps*area_gpsX, na.rm=TRUE)/sum(area_gpsX, na.rm=TRUE),
              area_gps = mean(area_gps, na.rm=TRUE),
              area_harv = mean(area_harv, na.rm=TRUE),
              NumberHH = length(unique(hhid)),
              Numberplot = n(),
              SOC2 = mean(SOC2, na.rm = TRUE),
              rain_wq = mean(rain_wq, na.rm = TRUE),
              seedq_gps = mean(seedq_gps),
              assetph = mean(assetph, na.rm = TRUE),
              ae = mean(ae/area_gps, na.rm = TRUE),
              pestherb = mean(pestherb, na.rm = TRUE),
              antrac = mean(antrac, na.rm = TRUE),
              slope = mean(slope, na.rm = TRUE),
              elevation = mean(elevation, na.rm = TRUE),
              crop_count2 = mean(crop_count2, na.rm = TRUE),
              YW = mean(YW)
    )
) %>%
  mutate(sharePlots = Numberplot/Numberplot[CLIMATEZONE == "Total"]*100,
         YG_gps = (1-yld_gps/YW)*100,
         YG_harv = (1-yld_harv/YW)*100) %>%
  gather(variable, value, -CLIMATEZONE) %>%
  spread(CLIMATEZONE, value) %>%
  left_join(., var_names) %>%
  filter(include == "Y") %>%
  arrange(order) %>%
  select(Variable = variable_full, `9301`, `9401`, `9501`, `9701`, `9801`, `9901`, `10401`, `Other zones`, Total)


### CHECK
# # Table with absolute yield gap information per zone
# # Note that by definition, YG_s computed by weighting individual YG_s values is not the same as multiplication of weighted TEYG_s etc.
# # We therefore calculate YG_s as the product of the weighted components.
# ZonalYieldGap_l <- bind_rows(
#   db9 %>% 
#     dplyr::select(Zone = ZONE, ERROR_l, TEYG_l, EYG_l, EUYG_l, TYG_l, YG_l_Ycor, YG_l, area) %>%
#     group_by(Zone) %>%
#     summarize(ERROR_l =(sum((ERROR_l)*area)/sum(area)),
#               TEYG_l = (sum((TEYG_l)*area)/sum(area)),
#               EYG_l = (sum((EYG_l)*area)/sum(area)),
#               EUYG_l = (sum((EUYG_l)*area)/sum(area)),
#               TYG_l = (sum((TYG_l)*area)/sum(area)),
#               YG_l = (sum((YG_l)*area)/sum(area)),
#               YG_l_Ycor = (sum((YG_l_Ycor)*area)/sum(area)),
#               YG_lcheck = (ERROR_l+TEYG_l+EYG_l+EUYG_l+TYG_l)),
#   db9 %>% 
#     dplyr::select(Zone = ZONE, ERROR_l, TEYG_l, EYG_l, EUYG_l, TYG_l, YG_l_Ycor, YG_l, area) %>%
#     summarize(Zone = "Total", 
#               ERROR_l =(sum((ERROR_l)*area)/sum(area)),
#               TEYG_l = (sum((TEYG_l)*area)/sum(area)),
#               EYG_l = (sum((EYG_l)*area)/sum(area)),
#               EUYG_l = (sum((EUYG_l)*area)/sum(area)),
#               TYG_l = (sum((TYG_l)*area)/sum(area)),
#               YG_l = (sum((YG_l)*area)/sum(area)),
#               YG_l_Ycor = (sum((YG_l_Ycor)*area)/sum(area)),
#               YG_lcheck = (ERROR_l+TEYG_l+EYG_l+EUYG_l+TYG_l))) %>%
#   dplyr::select(-ERROR_l,-YG_l, -YG_lcheck)
# 
# ZonalYieldGap_l <- xtable(ZonalYieldGap_l, digits = c(0,0,0,0,0,0,0))
# print(ZonalYieldGap_l, type="html", file=".\\FigTab\\ZonalYG_l.html")
# 
# ZonalYieldGap_l_sh <- ZonalYieldGap_l %>%
#   mutate(
#     TEYG = 100*TEYG_l/YG_l_Ycor,
#     EYG = 100*EYG_l/YG_l_Ycor,
#     EUYG = 100*EUYG_l/YG_l_Ycor,
#     TYG = 100*TYG_l/YG_l_Ycor,
#     YG = 100*(TEYG_l + EYG_l + EUYG_l + TYG_l)/YG_l_Ycor) %>%
#   dplyr::select(-TEYG_l:-YG_l_Ycor)
# 
# 
# ZonalYieldGap_l_sh <- xtable(ZonalYieldGap_l_sh, digits = c(0,0,0,0,0,0,0))
# print(ZonalYieldGap_l_sh, type="html", file=".\\FigTab\\ZonalYG_l_sh.html")
# 
# 
