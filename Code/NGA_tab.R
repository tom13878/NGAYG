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

# Can be sourced later
db1 <- readRDS(file.path(root, "Cache/db1.rds"))
db9_harv <- readRDS(file.path(root, "Cache/db9_harv.rds"))
db9_gps <- readRDS(file.path(root, "Cache/db9_gps.rds"))
source(file.path(root, "Code/sfaTable.R"))


### SFA ANALYSIS
sfaCD1_harv <- sfa(logyld_harv ~ noN + logN_gps + 
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
CZ_target <- CZ_target$CLIMATEZONE[CZ_target$Numberplot > 50] 
CZ_target <- droplevels(CZ_target)

### TABLE WITH KEY STATISTICS PER CLIMATE ZONE
# Add YW to db1
db1 <- dbP %>% dplyr::select(hhid, plotid, surveyyear, YW) %>% 
  na.omit %>% unique() %>%
  left_join(db1,.) %>%
  do(filter(., complete.cases(.)))

Tab_CZ <- bind_rows(
  db1 %>%
  filter(CLIMATEZONE %in% CZ_target) %>%
  ungroup() %>%
  dplyr::select(CLIMATEZONE, hhid, plotid, yld_gps, yld_harv, yesN, N_gps, area_gps, area_harv, 
                Elevation = elevation, SOC = SOC2, Assets = asset, crop_count2,
                irrig, pestherb, antrac, seed_q, ae, YW) %>%
  group_by(CLIMATEZONE) %>%
  summarise(Yield_W_gps = sum(yld_gps*area_gps, na.rm=TRUE)/ sum(area_gps),
            #Yield_UW_gps = mean(yld_gps, na.rm=TRUE),
            Yield_W_harv = sum(yld_harv*area_harv, na.rm=TRUE)/ sum(area_harv),
            #Yield_UW_harv = mean(yld_harv, na.rm=TRUE),
            mean_N_gps = mean(N_gps, na.rm=TRUE),
            Ncon = mean(ifelse(yesN == 1, N_gps, NA), na.rm = TRUE),
            yesN = round(mean(yesN)*100, digits=1),
            area_gps = mean(area_gps, na.rm=TRUE),
            area_harv = mean(area_harv, na.rm=TRUE),
            NumberHH = length(unique(hhid)),
            Numberplot = n(),
            YW = mean(YW*1000)
            ),
  db1 %>%
  filter(!(CLIMATEZONE %in% CZ_target)) %>%
  ungroup() %>%
    dplyr::select(CLIMATEZONE, hhid, plotid, yld_gps, yld_harv, yesN, N_gps, area_gps, area_harv, 
                  Elevation = elevation, SOC = SOC2, Assets = asset, crop_count2,
                  irrig, pestherb, antrac, seed_q, ae, YW) %>%
  summarise(CLIMATEZONE = "Minor zones",
            Yield_W_gps = sum(yld_gps*area_gps, na.rm=TRUE)/ sum(area_gps),
            #Yield_UW_gps = mean(yld_gps, na.rm=TRUE),
            Yield_W_harv = sum(yld_harv*area_harv, na.rm=TRUE)/ sum(area_harv),
            #Yield_UW_harv = mean(yld_harv, na.rm=TRUE),
            mean_N_gps = mean(N_gps, na.rm=TRUE),
            Ncon = mean(ifelse(yesN == 1, N_gps, NA), na.rm = TRUE),
            yesN = round(mean(yesN)*100, digits=1),
            area_gps = mean(area_gps, na.rm=TRUE),
            area_harv = mean(area_harv, na.rm=TRUE),
            NumberHH = length(unique(hhid)),
            Numberplot = n(),
            YW = mean(YW*1000)
            ),
  db1 %>%
    ungroup() %>%
    dplyr::select(CLIMATEZONE, hhid, plotid, yld_gps, yld_harv, yesN, N_gps, area_gps, area_harv, 
                  Elevation = elevation, SOC = SOC2, Assets = asset, crop_count2,
                  irrig, pestherb, antrac, seed_q, ae, YW) %>%
    summarise(CLIMATEZONE = "Total",
              Yield_W_gps = sum(yld_gps*area_gps, na.rm=TRUE)/ sum(area_gps),
              #Yield_UW_gps = mean(yld_gps, na.rm=TRUE),
              Yield_W_harv = sum(yld_harv*area_harv, na.rm=TRUE)/ sum(area_harv),
              #Yield_UW_harv = mean(yld_harv, na.rm=TRUE),
              yesN = round(mean(yesN)*100, digits=1),
              mean_N_gps = mean(N_gps, na.rm=TRUE),
              Ncon = mean(ifelse(yesN == 1, N_gps, NA), na.rm = TRUE),
              area_gps = mean(area_gps, na.rm=TRUE),
              area_harv = mean(area_harv, na.rm=TRUE),
              NumberHH = length(unique(hhid)),
              Numberplot = n(),
              YW = mean(YW*1000)
              )
  ) %>%
  mutate(sharePlots = Numberplot/Numberplot[CLIMATEZONE == "Total"]*100,
         YG_gps = (1-Yield_W_gps/YW)*100,
         YG_harv = (1-Yield_W_harv/YW)*100) %>%
  gather(variable, value, -CLIMATEZONE) %>%
  spread(CLIMATEZONE, value) 
  

### SHARE OF YIELD GAPS db9_gps
Tbl_YG_gps <- bind_rows(
  db9_gps %>%
    ungroup() %>%
    filter(CLIMATEZONE %in% CZ_target) %>%
    dplyr::select(hhid, plotid, area_gps, Ycor, TEY, EY, PFY, PY, ERROR_l, TEYG_l, EYG_l, EUYG_l, TYG_l, YG_l, YG_l_Ycor, CLIMATEZONE, surveyyear) %>%
    group_by(CLIMATEZONE) %>%
    summarize(ERROR_l =(sum((ERROR_l)*area_gps)/sum(area_gps)),
              TEYG_l = (sum((TEYG_l)*area_gps)/sum(area_gps)),
              EYG_l = (sum((EYG_l)*area_gps)/sum(area_gps)),
              EUYG_l = (sum((EUYG_l)*area_gps)/sum(area_gps)),
              TYG_l = (sum((TYG_l)*area_gps)/sum(area_gps)),
              YG_l = (sum((YG_l)*area_gps)/sum(area_gps)),
              YG_l_Ycor = (sum((YG_l_Ycor)*area_gps)/sum(area_gps)),
              YG_lcheck = (ERROR_l+TEYG_l+EYG_l+EUYG_l+TYG_l)),
  db9_gps %>%
    ungroup() %>%
    filter(CLIMATEZONE %in% CZ_target) %>%
    dplyr::select(hhid, plotid, area_gps, Ycor, TEY, EY, PFY, PY, ERROR_l, TEYG_l, EYG_l, EUYG_l, TYG_l, YG_l, YG_l_Ycor, CLIMATEZONE, surveyyear) %>%
    summarize(CLIMATEZONE = "Total",
              ERROR_l =(sum((ERROR_l)*area_gps)/sum(area_gps)),
              TEYG_l = (sum((TEYG_l)*area_gps)/sum(area_gps)),
              EYG_l = (sum((EYG_l)*area_gps)/sum(area_gps)),
              EUYG_l = (sum((EUYG_l)*area_gps)/sum(area_gps)),
              TYG_l = (sum((TYG_l)*area_gps)/sum(area_gps)),
              YG_l = (sum((YG_l)*area_gps)/sum(area_gps)),
              YG_l_Ycor = (sum((YG_l_Ycor)*area_gps)/sum(area_gps)),
              YG_lcheck = (ERROR_l+TEYG_l+EYG_l+EUYG_l+TYG_l))) %>%
  dplyr::select(-ERROR_l,-YG_l, -YG_lcheck)

Tbl_YG_sh_gps <- Tbl_YG_gps %>%
  mutate(
    TEYG = 100*TEYG_l/YG_l_Ycor,
    EYG = 100*EYG_l/YG_l_Ycor,
    EUYG = 100*EUYG_l/YG_l_Ycor,
    TYG = 100*TYG_l/YG_l_Ycor,
    YG = 100*(TEYG_l + EYG_l + EUYG_l + TYG_l)/YG_l_Ycor) %>%
  dplyr::select(-TEYG_l:-YG_l_Ycor) %>%
  setNames(c("Climate zone", "TEYg", "EYg", "FYg", "TYg", "Yg"))


### SHARE OF YIELD GAPS db9_harv
Tbl_YG_harv <- bind_rows(
  db9_harv %>%
    ungroup() %>%
    filter(CLIMATEZONE %in% CZ_target) %>%
    dplyr::select(hhid, plotid, area_harv, Ycor, TEY, EY, PFY, PY, ERROR_l, TEYG_l, EYG_l, EUYG_l, TYG_l, YG_l, YG_l_Ycor, CLIMATEZONE, surveyyear) %>%
    group_by(CLIMATEZONE) %>%
    summarize(ERROR_l =(sum((ERROR_l)*area_harv)/sum(area_harv)),
              TEYG_l = (sum((TEYG_l)*area_harv)/sum(area_harv)),
              EYG_l = (sum((EYG_l)*area_harv)/sum(area_harv)),
              EUYG_l = (sum((EUYG_l)*area_harv)/sum(area_harv)),
              TYG_l = (sum((TYG_l)*area_harv)/sum(area_harv)),
              YG_l = (sum((YG_l)*area_harv)/sum(area_harv)),
              YG_l_Ycor = (sum((YG_l_Ycor)*area_harv)/sum(area_harv)),
              YG_lcheck = (ERROR_l+TEYG_l+EYG_l+EUYG_l+TYG_l)),
  db9_harv %>%
    ungroup() %>%
    filter(CLIMATEZONE %in% CZ_target) %>%
    dplyr::select(hhid, plotid, area_harv, Ycor, TEY, EY, PFY, PY, ERROR_l, TEYG_l, EYG_l, EUYG_l, TYG_l, YG_l, YG_l_Ycor, CLIMATEZONE, surveyyear) %>%
    summarize(CLIMATEZONE = "Total",
              ERROR_l =(sum((ERROR_l)*area_harv)/sum(area_harv)),
              TEYG_l = (sum((TEYG_l)*area_harv)/sum(area_harv)),
              EYG_l = (sum((EYG_l)*area_harv)/sum(area_harv)),
              EUYG_l = (sum((EUYG_l)*area_harv)/sum(area_harv)),
              TYG_l = (sum((TYG_l)*area_harv)/sum(area_harv)),
              YG_l = (sum((YG_l)*area_harv)/sum(area_harv)),
              YG_l_Ycor = (sum((YG_l_Ycor)*area_harv)/sum(area_harv)),
              YG_lcheck = (ERROR_l+TEYG_l+EYG_l+EUYG_l+TYG_l))) %>%
  dplyr::select(-ERROR_l,-YG_l, -YG_lcheck)

Tbl_YG_sh_harv <- Tbl_YG_harv %>%
  mutate(
    TEYG = 100*TEYG_l/YG_l_Ycor,
    EYG = 100*EYG_l/YG_l_Ycor,
    EUYG = 100*EUYG_l/YG_l_Ycor,
    TYG = 100*TYG_l/YG_l_Ycor,
    YG = 100*(TEYG_l + EYG_l + EUYG_l + TYG_l)/YG_l_Ycor) %>%
  dplyr::select(-TEYG_l:-YG_l_Ycor) %>%
  setNames(c("Climate zone", "TEYg", "EYg", "FYg", "TYg", "Yg"))


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
