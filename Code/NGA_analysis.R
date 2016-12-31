#'========================================================================================================================================
#' Project:  CIMMYT Yield gap
#' Subject:  Analyse NGA yield gap
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
BasePackages<- c("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
#lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("WDI", "stargazer", "frontier", "AER", "moments")
lapply(AdditionalPackages, library, character.only = TRUE)

### SET WORKING DIRECTORY
wdPath <- "~\\NGAYG"
setwd(wdPath)

dataPath <- "C:\\Users\\dijk158\\OneDrive - IIASA\\SurveyData"

# SOURCE 
source("Code/winsor.r")
source("Code/waterfall_plot.R")

# OPTIONS
options(scipen=999)

#######################################
############## LOAD DATA ##############
#######################################

# Load pooled data
dbP <- readRDS("Cache/Pooled_NGA.rds")

#######################################
############## CLEANING ###############
#######################################

# Create rel_harv_area variable
dbP$area_farmer[dbP$area_farmer %in% 0] <- NA
dbP$harv_area[dbP$harv_area %in% 0] <- NA
 
dbP <- dbP %>%
  mutate( sh_harv_area = harv_area/area_farmer,
          sh_harv_area = ifelse(sh_harv_area >1, 1, sh_harv_area), # for some farmers the harvested area > plot size. Set to 1
          area_harv = sh_harv_area * area_gps)

# Cleaning and analysis depends strongly on which measure is chosen for area, which is the denominator for many variables.
# there are three possible yield variables. 
# that can be created for the last two waves of data. 
# 1. yld1: above uses the full gps areas as denominator
# 2. yld2: uses harvested area as denominator
# 3. yld3: Uses relative harvest area to correct gps area
# We use 1 and 3 in the analysis

# cap yield at the highest potential yield in NGA (not water limited)
# Do this for both yield levels as in particular yld_harv may result in very large and unrealistically yields
GYGA <- read_excel(file.path(dataPath, "Other/GYGA/GygaRainfedMaizeSubSaharanAfrica.xlsx"), sheet = "Climate zone") %>%
  filter(COUNTRY == "Nigeria")

maxYP <- max(GYGA$YP)*1000
dbP <- dbP %>% 
  rename(area_harv_farmer = harv_area) %>%
  mutate(yld_gps = crop_qty_harv/area_gps,
         yld_farmer = crop_qty_harv/area_farmer,
         yld_harv_farmer = crop_qty_harv/area_harv_farmer,
         yld_harv = crop_qty_harv/area_harv) %>%
  filter(yld_gps <= maxYP & yld_harv <= maxYP) 
  
# Sample includes very small plots <0.005 ha that bias result upwards and very large >35 ha. 
# As we focus on small scale farmers we restrict area size
dbP <- filter(dbP, area_gps >=0.01 & area_gps <=10) %>%
       filter(area_harv >=0.01 & area_harv <=10)

# restrict attention to plots that use N < 700. 700kg/hectare  represents an upper bound limit associated with inorganic fertilizer use in the United States under irrigated corn conditions (Sheahan & Barett 2014) 
dbP <- dbP %>%
  mutate(N_gps = N/area_gps,
         N_harv = N/area_harv) %>%
  filter(N_gps <= 700 & N_harv <= 700)

db0 <- dbP %>% 
  dplyr::select(hhid, plotid, ZONE, REGNAME, DISNAME, CLIMATEZONE, YW, fs,
                SOC, SOC2, ph, ph2, 
                rain_wq, 
                slope, elevation,
                yld_harv, yld_gps,
                yld_farmer, yld_harv_farmer,
                area_gps, area_harv, 
                area_harv_farmer, area_farmer,
                ae,
                seed, seed_q,
                area_tot, 
                implmt_value, lvstk_valu, lvstk2_valu,
                pest, herb,
                N_harv, N_gps, 
                mech, antrac,
                inter_crop,
                irrig,
                crop_count, surveyyear,
                lat, lon)


summary(db0)

#######################################
###### COMPLETE CASES DATABASE ########
#######################################

db0 <- db0 %>%
  do(filter(., complete.cases(.)))


######################################
######## Modify and add variables ####
######################################

# Following Burke
db0$phdum[db0$ph < 55] <- 1
db0$phdum[db0$ph >= 55 & db0$ph <=70] <- 2 # Neutral and best suited for crops
db0$phdum[db0$ph > 70] <- 3
db0$phdum <- factor(db0$phdum)

db0$phdum2[db0$ph2 < 55] <- 1
db0$phdum2[db0$ph2 >= 55 & db0$ph2 <=70] <- 2
db0$phdum2[db0$ph2 > 70] <- 3
db0$phdum2 <- factor(db0$phdum2)

# Crop count > 1
db0$crop_count2[db0$crop_count==1] <- 1
db0$crop_count2[db0$crop_count>1] <- 0

# additional variables
db0 <- db0 %>% 
              mutate ( CLIMATEZONE = as.factor(CLIMATEZONE),
                       logyld_harv = log(yld_harv),
                       logyld_gps = log(yld_gps),
                       yesN = ifelse(N_gps>0, 1,0), # Dummy when plot does not use fertilizer, following approach of Battese (1997)
                       noN = ifelse(N_gps<=0, 1,0), # Dummy when plot does not use fertilizer, following approach of Battese (1997)
                       logN_harv = log(pmax(N_harv, noN)), # maximum of dummy and N following Battese (1997)
                       logN_gps = log(pmax(N_gps, noN)), # maximum of dummy and N following Battese (1997)
                       #lab = harv_lab + harv_lab_hire,
                       #hirelab_sh = harv_lab_hire/(harv_lab_hire + harv_lab)*100,
                       #lab=lab/area,
                       logae_harv = log(ae/area_harv),
                       logae_gps = log(ae/area_gps),
                       asset = implmt_value + lvstk2_valu,
                       assetph=asset/area_tot,
                       logasset = log(assetph+1),
                       #loglab = log(lab+1),
                       logarea_gps = log(area_gps), # area_gps not area because we want to add plot size as proxy for economies of scale
                       rain_wq2 = rain_wq*rain_wq,
                       #rain_year2 = rain_year*rain_year,
                       pestherb = ifelse(herb==1 | pest==1, 1, 0),
                       #ext = ifelse(ext_dummy_pp==1 | ext_dummy_ph ==1, 1, 0),
                       logseedq_harv = log(seed_q/area_harv),
                       logseedq_gps = log(seed_q/area_gps),
                       #lograin = log(rain_year),
                       #sex = as.numeric(ifelse(sex == "MALE", 0, 1)),
                       surveyyear2 = replace(surveyyear==2010, 1, 0))

# Remove observations with extreme N_harv because of very 0 asset and 0 harv_lab
#db0 <- filter(db0, asset!= 0 & lab != 0)
db0 <- filter(db0, N_harv <=700)

# -------------------------------------
# Inflate 2011 prices to 2013 prices: assets
# using inflation rate for 2011 and 2013. These years were selected as the main part of the survey takes place in these years.
# from world bank:
# http://data.worldbank.org/indicator/FP.CPI.TOTL.ZG/countries/TZ?display=graph
# -------------------------------------

inflation <- read.csv(file.path(dataPath,"Other/Inflation/inflation.csv"))
rate2011 <- inflation$inflation[inflation$code=="NG" & inflation$year==2011]/100
rate2012 <- inflation$inflation[inflation$code=="NG" & inflation$year==2012]/100

inflate <- (1 + rate2011)*(1 + rate2012)
db0 <- mutate(db0, asset = ifelse(surveyyear == 2010, asset*inflate, asset))

db0 <- filter(db0, asset >0) # remove few hh with no assets

# Add CRE variables
db0 <- db0 %>%
      group_by(hhid) %>%
      mutate(
        #loglab_bar=mean(loglab, na.rm=TRUE),
             logae_harv_bar=mean(logae_harv, na.rm=TRUE),
             logN_harv_bar=mean(logN_harv, na.rm=TRUE),
             noN_bar=mean(noN, na.rm=TRUE),
             logasset_bar=mean(logasset, na.rm=TRUE),
             logarea_gps_bar=mean(logarea_gps, na.rm=TRUE),
             #irrig_bar=mean(irrig, na.rm = TRUE),
             #legume_bar=mean(legume, na.rm = TRUE),
             pest_bar=mean(pest, na.rm = TRUE),
             herb_bar=mean(herb, na.rm = TRUE),
             pestherb_bar=mean(pestherb, na.rm = TRUE),
             seed_bar=mean(seed, na.rm = TRUE),
             #logseed_q_bar=mean(logseed_q, na.rm=TRUE),
             mech_bar=mean(mech, na.rm = TRUE),
             antrac_bar=mean(antrac, na.rm = TRUE),
             #inter_crop_bar=mean(inter_crop, na.rm = TRUE),
             #ext_bar=mean(ext, na.rm = TRUE),
             crop_count_bar=mean(crop_count2, na.rm=TRUE))

# Select climate zones

# targetZones <- c(9301, 9401,  9501,  9701,  9801,  9901, 10401)
# db0 <- filter(db0, CLIMATEZONE %in% targetZones)            

# drop unused levels
db0 <- droplevels(db0)


# Profit maximizing yield analysis
# Load and merge price data
Prices <- readRDS("Cache/NGA_Prices.rds")

# Merge with panel data
db1 <- left_join(db0, Prices)
summary(select(db1, Pc, Pn)) # Note that 8 plots do not have price date because they miss ZONE, etc info or are duplictes (see price script)

# Drop unused levels (e.g. Zanzibar in zone), which are giving problems with sfa
db1 <- droplevels(db1)

db1 <- db1 %>%
  do(filter(., complete.cases(.)))

# Save file
saveRDS(db1, "Cache/db1.rds")

#######################################
############## ANALYSIS ###############
#######################################

# Cobb Douglas
olsCD1_harv <- lm(logyld_harv ~ noN + logN_harv + logasset + logae_harv + 
               logseedq_harv +
               logarea_gps +
               mech + antrac + 
               #inter_crop +
               slope + elevation +
               SOC2 + phdum2 + 
               rain_wq + rain_wq2 +
               crop_count2 + surveyyear2,
             data = db1)

# Cobb Douglas
olsCD1_gps <- lm(logyld_gps ~ noN + logN_gps + logasset + logae_gps + 
                    logseedq_gps +
                    logarea_gps +
                    mech + antrac + 
                    #inter_crop +
                    slope + elevation +
                    SOC2 + phdum2 + 
                    rain_wq + rain_wq2 +
                   crop_count2 + surveyyear2,
                  data = db1)


stargazer(olsCD1_harv, olsCD1_gps, type="text")

# Assess skewness of OLS - should be left skewed which is confirmed.
hist( residuals(olsCD1_harv), 15)
hist( residuals(olsCD1_gps), 15)
skewness(residuals(olsCD1_harv))
skewness(residuals(olsCD1_gps))


# Frontier estimation
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
                     crop_count2 + surveyyear2,                  ,
                   data = db1, maxit = 1500, restartMax = 20, printIter = 1, tol = 0.000001)

summary(sfaCD1_gps, extraPar = TRUE)
lrtest(sfaCD1_gps)

########################################
### YIELD GAP ANALYSIS FOR AREA_HARV ###
########################################

# Compute profit maximizing Pn per zone and other summary statistics
# Select model
model <- sfaCD1_harv

# Note that MPP cannot be calculated for plots with N=0 and are therefore set to 0.
db2_sfaCD1_harv <- db1 %>% mutate(elastfert = coef(model)["logN_harv"],
                             phconstant = as.numeric(ifelse(phdum==2, coef(model)["phdum22"], 
                                                 ifelse(phdum==3, coef(model)["phdum23"],0))),
                             lnA = coef(model)["(Intercept)"] +
                               (coef(model)["noN"]*noN) +
                               (coef(model)["logseedq_harv"]*logseedq_harv) +
                               (coef(model)["logarea_gps"]*logarea_gps) +
                               (coef(model)["irrig"]*irrig) +
                               (coef(model)["pestherb"]*pestherb) +
                               (coef(model)["antrac"]*antrac) +
                               (coef(model)["slope"]*slope) +
                               (coef(model)["elevation"]*elevation) +
                               (coef(model)["SOC2"]*SOC2) +
                               (phconstant) +
                               (coef(model)["rain_wq"]* rain_wq) +
                               (coef(model)["rain_wq2"]*rain_wq2) +
                               (coef(model)["crop_count2"]*crop_count2) +
                               (coef(model)["surveyyear2"]*surveyyear2),
                             lnA2 = lnA,
                             constantfactor = exp(lnA2)*elastfert*(ae^coef(model)["logae_harv"])*(asset^coef(model)["logasset"]),
                             MPP= as.numeric(ifelse(N_harv==0,NA,exp(lnA2)*elastfert*(ae^coef(model)["logae_harv"])*(asset^coef(model)["logasset"])*(N_harv^(elastfert-1)))),
                             Npm = (Pn/(constantfactor*Pc))^(1/(elastfert-1)),
                             Ndif = N_harv-Npm
                             )                       


sumzone_sfaCD1_harv<- db2_sfaCD1_harv %>%
  group_by(CLIMATEZONE) %>%
  summarize(
    Ncon=mean(ifelse(N_harv>0, N_harv, NA), na.rm = T),
    N_harv=mean(N_harv, na.rm=T),
    Npm=mean(Npm, na.rm=T),
    MPPmean=mean(MPP[!is.infinite(MPP)], na.rm=T),
    MVC=mean((Pc[!is.infinite(MPP)]*MPP[!is.infinite(MPP)])/Pn[!is.infinite(MPP)], na.rm=T),
    Ndif=mean(Ndif, na.rm=T),
    Number=n())


##############################
### Calculate yield levels ###
##############################

# set model
model <- sfaCD1_harv

# 1. TEYG: Technical efficient yield gap

# Calculate yield level at 100% TE on the frontier with given inputs
# We estimate the error using the sfa formula and compute Ycor.
# In sfa, Y = TEY + error(v) - efficiency(u) Kumbakar et al. (2015), A practitioner's guide, p.48-49
# We want to filter out/correct for the error. Ycor = TEY - u = Y + e
# Since we do not know the error we calculate Ycor as TEY - u.
# Efficiency as produced by package frontier is defined as TE = exp(-u) so u is -log(TE)
# As the model is in logs, TEY = exp(ln(TEY))
# Ycor = exp(ln(TEY)) - [-log(TE)]

db3 <- db2_sfaCD1_harv %>%
  ungroup() %>%
  dplyr::select(hhid, plotid, surveyyear, ZONE, REGNAME, DISNAME, CLIMATEZONE, fs, YW, area_harv, crop_count2, lat, lon, lnA, lnA2, noN, yesN, logae_harv, ae, logasset, asset, elastfert, N_harv, Y=yld_harv, Npm) %>%
  mutate(
    Ycor = exp(as.numeric(fitted(model))+log(as.numeric(efficiencies(model)))), 
    err = Ycor-Y,
    TEY = exp(as.numeric(fitted(model))),
    TE = as.numeric(efficiencies(model)),
    resid = as.numeric(resid(model))
  )

# A number of plots have yld higher than the estimated frontier (Y-TEY>0) caused by the random error. 
# A large number of these plots do not use fertilizer. They probably have high yield because of measurement error, better soil properties or unknown factors.
X_above_frontier <- filter(db3, Y-TEY>0)
mean(db3$Y-db3$TEY)


# 2. EY: Economic yield 
# Calculate optimal Npm when using Pn
# Note that noN is still part of lnA2 because we assume that plots without N are structurally different from those with N, for instance better soil.

# It is possible that Npm is larger than Npy, which is not possible from a biophysical perspective.
# We cap Npm at Npy.

# # Based on experimental plot data (see Excel), we set Npy to 300.
# CHECK CHANGE FOR NIGERIA
Npy <- 300

# # Cap Npm
db3 <- mutate(db3, Npm = ifelse(Npm>Npy, Npy, Npm))
 
db4 <- db3 %>%
  mutate(EY = exp(
    lnA2 +
      coef(model)["logae_harv"]*logae_harv +
      coef(model)["logasset"]*logasset +
      elastfert*log(Npm)))

# 3 PFY: Feasible yield
# To improve this part, we could also argue that: (1) hybrid seeds are used, (2) pestices are used, (3) higher levels of capital and labour are used.
# We assume that all farmers use pesticides and increase assets and labour with 10%

# CHECK increase seed QUANTITY

db5 <- db4 %>%
  mutate(PFY = exp(
    lnA + 
      coef(model)["pestherb"] +                   # assume all plots use pesticides and herbicides
      coef(model)["antrac"] +                     # assume all plots use animal traction
      coef(model)["logae_harv"]*logae_harv*1.1 +
      coef(model)["logasset"]*logasset*1.1 +
      elastfert*log(Npy)))

# 4. PY: Potential yield and HFY: Highest farmers yield
# # Merge Yield potential with maize plot database
# db6 <- dbP %>% dplyr::select(hhid, plotid, surveyyear, PY = YW) %>% na.omit %>% 
#   mutate(PY=PY*1000) %>% left_join(db5,.)
# 
# # A large number of plots have missing YW values because region is not covered by GYGA.
# # We assume for the moment that country maximum water limited yield (Yw) is reasonable proxy for missing information.
# # Might scale this down to see the effect.
# GYGA_YW <- max(GYGA$YW)*1000
# db6 <- mutate(db6, PY = ifelse(is.na(PY), GYGA_YW, PY))

# Rename YW into PY and express in kg
db6 <- db5 %>%
  rename(PY = YW) %>%
  mutate(PY = PY*1000) %>%
  group_by(CLIMATEZONE) %>%
  mutate(HFY90 = quantile(Y, probs = 0.9, na.rm = TRUE),
         HFY95 = quantile(Y, probs = 0.95, na.rm = TRUE),
         HFY100 = quantile(Y, probs = 1, na.rm = TRUE))

check <- db5 %>%
  ungroup() %>%
  filter(CLIMATEZONE == "9901") %>%
  mutate(max = quantile(Y, probs = 0.90, na.rm = TRUE))

mean(check$Y)
quantile(check$Y, probs = 0.90, na.rm = TRUE)

#####################################################
### Yield levels consistency check and correction ###
#####################################################

# Because of imputation of TY or measurement error, Yield (Y and Ycor), Technical efficiency yield (TEY), Economic yield (EY) and Unexploitable yield (UY) 
# can be higher than Potential yield (PYcor). We check for this.

X_Y_Ycor_check <- filter(db6, Ycor-Y<0)
X_PY_Y_check <- filter(db6, PY-Y<0)
X_PY_Y_cor_check <- filter(db6, PY-Ycor<0)
X_PY_TE_check <- filter(db6, PY-TEY<0)
X_PY_EY_check <- filter(db6, PY-EY<0)
X_PY_PFY_check <- filter(db6, PY-PFY<0)

# Compare different yield levels
# Picture shows that PFY is too high for plots with the lowest PY. This is probably due to the uniform use of Npf of 120 N/ha.
# It would be better to have zone specific Npf values.
ggplot(data = db6, aes(y = PY, x = PFY)) +
  geom_point() +
  #geom_jitter(position=position_jitter(width=.1, height=0))+
  geom_abline(aes(Y = Ycor), slope=1, intercept=0) +
  coord_fixed() +
  scale_y_continuous(limits=c(0, 10000)) +
  scale_x_continuous(limits=c(0, 10000))

# Compare error and resid
# Not clear what resid is? As the following plot shows, TEYG_s = TE
ggplot(data = db6, aes(y = err, x = resid)) +
  geom_point() 

# Compare Sfa TA scores with mannually computed TEYG_s => identical as they should be
db6a <- mutate(db6, TEYG_s =Ycor/TEY)
ggplot(data = db6a, aes(y = TEYG_s, x = TE)) +
  geom_point()

#  We cap all values at PY because we consider this as an absolute potential and recalculate all gaps.
db7 <- mutate(db6, PFY = ifelse(PY-PFY<0, PY, PFY),
              EY = ifelse(PY-EY<0, PY, EY),
              TEY = ifelse(PY-TEY<0, PY, TEY),
              Ycor = ifelse(PY-Ycor<0, PY, Ycor),
              Y = ifelse(PY-Y<0, PY, Y))

#############################
### Yield gap calculation ###
#############################

# Calculate TYG using UY as reference
db8 <- db7 %>% 
  mutate(
    ERROR_l = Ycor-Y,      # Error gap
    ERROR_s = Y/Ycor,      # Error gap
    TEYG_l = TEY-Ycor,     # Technical efficiency yield gap using Ycor as basis
    TEYG_s = Ycor/TEY,     # Technical efficiency yield gap using Ycor as basis
    EYG_l = EY-TEY,        # Economic yield gap
    EYG_s = TEY/EY,        # Economic yield gap
    EUYG_l = PFY-EY,       # Feasible yield gap
    EUYG_s = EY/PFY,       # Feasible yield gap
    TYG_l = PY-PFY,        # Technology yield gap
    TYG_s = PFY/PY,        # Technology yield gap
    YG_l = PY-Y,           # Yield gap
    YG_s = Y/PY,           # Yield gap
    YG_l_Ycor = PY-Ycor,   # Yield gap with Ycor as reference
    YG_s_Ycor = Ycor/PY)   # Yield gap with Ycor as reference

# Consistency check of yield gaps.
# ERROR
X_ERROR <- filter(db8, ERROR_l<0) # some plots have a negative error, which is normal there will be values above the frontier due to error
mean(db8$ERROR_l)
mean(db8$ERROR_s)

# TEYG
X_TEYG <- filter(db8, TEYG_l<0) # Should be zero
mean(db8$TEYG_s)

# EYG
# A number of plots will have to decrease N use Npm < N. In several cases also plots that do not use N
# will have lower Y when they start using N. This is because there yield can be located above the frontier (based on fertilizer users) because of the positive effect of noN.
# If we believe that these plots are structurally different and do not use fertilizer because of better soils, they will in fact use too much N and have to decrease.
X_EYG <- filter(db8, EYG_l<0)        
mean(db8$EYG_s)

# EUYG
# A number of plots have negative EUYG_l because Npm is larger than Nyw, the nitrogen that is required to achieve Potential yield (Yw).
# Not the case here
# We have corrected this so check should be 0.
X_EUYG <- filter(db8, EUYG_l<0)        
mean(db8$EUYG_s)

# TYG
X_TYG <- filter(db8, TYG_l<0)        
mean(db8$TYG_s)

#YG
X_YG <- filter(db8, YG_l<0)        
X_YG2 <- filter(db8, YG_l_Ycor<0)        

# Check if separate yield gaps add up to total yield gap
X_overall <- db8 %>%
  mutate(check_l = YG_l/(ERROR_l + TEYG_l + EYG_l + EUYG_l + TYG_l), # Note that for a small number of observatios YG_l=0 resulting in 0/0 which is NaN
         check_s = YG_s/(ERROR_s * TEYG_s * EYG_s * EUYG_s * TYG_s),
         check_l2 = YG_l_Ycor/(TEYG_l + EYG_l + EUYG_l + TYG_l),
         check_s2 = YG_s_Ycor/(TEYG_s * EYG_s * EUYG_s * TYG_s))
summary(X_overall)


# Create database with relevant variables for further analysis
db9_harv <- dplyr::select(db8, hhid, plotid, surveyyear, ZONE, REGNAME, fs, lat, lon, crop_count2, area_harv, yesN, Y, N_harv,
                          Ycor, TEY, EY, PFY, HFY90, HFY95, HFY100, PY, ERROR_l, ERROR_s, TEYG_l, TEYG_s, EYG_l, EYG_s, 
                          EUYG_l, EUYG_s, TYG_l, TYG_s, YG_l, YG_s, YG_l_Ycor, YG_s_Ycor)

# Clean up
rm(list =grep("X_", ls(), value = TRUE)) # remove checks
rm(db3, db4, db5, db6, db6a, db7, db8)

# Save data
saveRDS(db9_harv, "Cache/db9_harv.rds")

#######################################
### YIELD GAP ANALYSIS FOR AREA_GPS ###
#######################################

# Compute profit maximizing Pn per zone and other summary statistics
# Select model
model <- sfaCD1_gps

# Note that MPP cannot be calculated for plots with N=0 and are therefore set to 0.
db2_sfaCD1_gps <- db1 %>% mutate(elastfert = coef(model)["logN_gps"],
                                  phconstant = as.numeric(ifelse(phdum==2, coef(model)["phdum22"], 
                                                                 ifelse(phdum==3, coef(model)["phdum23"],0))),
                                  lnA = coef(model)["(Intercept)"] +
                                    (coef(model)["noN"]*noN) +
                                    (coef(model)["logseedq_gps"]*logseedq_gps) +
                                    (coef(model)["logarea_gps"]*logarea_gps) +
                                    (coef(model)["irrig"]*irrig) +
                                    (coef(model)["pestherb"]*pestherb) +
                                    (coef(model)["antrac"]*antrac) +
                                    (coef(model)["slope"]*slope) +
                                    (coef(model)["elevation"]*elevation) +
                                    (coef(model)["SOC2"]*SOC2) +
                                    (phconstant) +
                                    (coef(model)["rain_wq"]* rain_wq) +
                                    (coef(model)["rain_wq2"]*rain_wq2) +
                                    (coef(model)["crop_count2"]*crop_count2) +
                                    (coef(model)["surveyyear2"]*surveyyear2),
                                  lnA2 = lnA,
                                  constantfactor = exp(lnA2)*elastfert*(ae^coef(model)["logae_gps"])*(asset^coef(model)["logasset"]),
                                  MPP= as.numeric(ifelse(N_gps==0,NA,exp(lnA2)*elastfert*(ae^coef(model)["logae_gps"])*(asset^coef(model)["logasset"])*(N_gps^(elastfert-1)))),
                                  Npm = (Pn/(constantfactor*Pc))^(1/(elastfert-1)),
                                  Ndif = N_gps-Npm
)                       


sumzone_sfaCD1_gps<- db2_sfaCD1_gps %>%
  group_by(CLIMATEZONE) %>%
  summarize(
    Ncon=mean(ifelse(N_gps>0, N_gps, NA), na.rm = T),
    N_gps=mean(N_gps, na.rm=T),
    Npm=mean(Npm, na.rm=T),
    MPPmean=mean(MPP[!is.infinite(MPP)], na.rm=T),
    MVC=mean((Pc[!is.infinite(MPP)]*MPP[!is.infinite(MPP)])/Pn[!is.infinite(MPP)], na.rm=T),
    Ndif=mean(Ndif, na.rm=T),
    Number=n())


##############################
### Calculate yield levels ###
##############################

# set model
model <- sfaCD1_gps

# 1. TEYG: Technical efficient yield gap

# Calculate yield level at 100% TE on the frontier with given inputs
# We estimate the error using the sfa formula and compute Ycor.
# In sfa, Y = TEY + error(v) - efficiency(u) Kumbakar et al. (2015), A practitioner's guide, p.48-49
# We want to filter out/correct for the error. Ycor = TEY - u = Y + e
# Since we do not know the error we calculate Ycor as TEY - u.
# Efficiency as produced by package frontier is defined as TE = exp(-u) so u is -log(TE)
# As the model is in logs, TEY = exp(ln(TEY))
# Ycor = exp(ln(TEY)) - [-log(TE)]

db3 <- db2_sfaCD1_gps %>%
  ungroup() %>%
  dplyr::select(hhid, plotid, surveyyear, ZONE, REGNAME, DISNAME, CLIMATEZONE, YW, area_gps, crop_count2, lat, lon, lnA, lnA2, noN, yesN, logae_gps, ae, logasset, asset, elastfert, N_gps, Y=yld_gps, Npm) %>%
  mutate(
    Ycor = exp(as.numeric(fitted(model))+log(as.numeric(efficiencies(model)))), 
    err = Ycor-Y,
    TEY = exp(as.numeric(fitted(model))),
    TE = as.numeric(efficiencies(model)),
    resid = as.numeric(resid(model))
  )

# A number of plots have yld higher than the estimated frontier (Y-TEY>0) caused by the random error. 
# By far most of these are plots that do not use fertilizer. They probably have high yield because of measurement error, better soil properties or unknown factors.
X_above_frontier <- filter(db3, Y-TEY>0)
mean(db3$Y-db3$TEY)


# 2. EY: Economic yield 
# Calculate optimal Npm when using Pn
# Note that noN is still part of lnA2 because we assume that plots without N are structurally different from those with N, for instance better soil.

# It is possible that Npm is larger than Npy, which is not possible from a biophysical perspective.
# We cap Npm at Npy.

# # Based on experimental plot data (see Excel), we set Npy to 150. CHECK CHANGE FOR NIGERIA
Npy <- 300

# # Cap Npm
db3 <- mutate(db3, Npm = ifelse(Npm>Npy, Npy, Npm))

db4 <- db3 %>%
  mutate(EY = exp(
    lnA2 +
      coef(model)["logae_gps"]*logae_gps +
      coef(model)["logasset"]*logasset +
      elastfert*log(Npm)))

# 3 PFY: Feasible yield
# To improve this part, we could also argue that: (1) hybrid seeds are used, (2) pestices are used, (3) higher levels of capital and labour are used.
# We assume that all farmers use pesticides and increase assets and labour with 10%

# CHECK increase seed QUANTITY

db5 <- db4 %>%
  mutate(PFY = exp(
    lnA + 
      coef(model)["pestherb"] +                   # assume all plots use pesticides and herbicides
      coef(model)["antrac"] +                     # assume all plots use animal traction
      coef(model)["logae_gps"]*logae_gps*1.1 +
      coef(model)["logasset"]*logasset*1.1 +
      elastfert*log(Npy)))

# 4. PY: Potential yield
# Merge Yield potential with maize plot database
# Merge Yield potential with maize plot database
# db6 <- dbP %>% dplyr::select(hhid, plotid, surveyyear, PY = YW) %>% na.omit %>% 
#   mutate(PY=PY*1000) %>% left_join(db5,.)
# 
# # A large number of plots have missing YW values because region is not covered by GYGA.
# # We assume for the moment that country maximum water limited yield (Yw) is reasonable proxy for missing information.
# # Might scale this down to see the effect.
# GYGA_YW <- max(GYGA$YW)*1000
# db6 <- mutate(db6, PY = ifelse(is.na(PY), GYGA_YW, PY))

# Rename YW into PY and express in kg
db6 <- db5 %>%
  rename(PY = YW) %>%
  mutate(PY = PY*1000) %>%
  group_by(CLIMATEZONE) %>%
  mutate(HFY = quantile(Ycor, probs = 0.9))

#####################################################
### Yield levels consistency check and correction ###
#####################################################

# Because of imputation of TY or measurement error, Yield (Y and Ycor), Technical efficiency yield (TEY), Economic yield (EY) and Unexploitable yield (UY) 
# can be higher than Potential yield (PYcor). We check for this.

X_Y_Ycor_check <- filter(db6, Ycor-Y<0)
X_PY_Y_check <- filter(db6, PY-Y<0)
X_PY_Y_cor_check <- filter(db6, PY-Ycor<0)
X_PY_TE_check <- filter(db6, PY-TEY<0)
X_PY_EY_check <- filter(db6, PY-EY<0)
X_PY_PFY_check <- filter(db6, PY-PFY<0)

# Compare different yield levels
# Picture shows that PFY is much to high for plots with the lowest PY. This is probably due to the uniform use of Npf of 120 N/ha.
# It would be better to have zone specific Npf values.
ggplot(data = db6, aes(y = PY, x = PFY)) +
  geom_point() +
  #geom_jitter(position=position_jitter(width=.1, height=0))+
  geom_abline(aes(Y = Ycor), slope=1, intercept=0) +
  coord_fixed() +
  scale_y_continuous(limits=c(0, 10000)) +
  scale_x_continuous(limits=c(0, 10000))

# Compare error and resid
# Not clear what resid is? As the following plot shows, TEYG_s = TE
ggplot(data = db6, aes(y = err, x = resid)) +
  geom_point() 

# Compare Sfa TA scores with mannually computed TEYG_s => identical as they should be
db6a <- mutate(db6, TEYG_s =Ycor/TEY)
ggplot(data = db6a, aes(y = TEYG_s, x = TE)) +
  geom_point()

#  We cap all values at PY because we consider this as an absolute potential and recalculate all gaps.
db7 <- mutate(db6, PFY = ifelse(PY-PFY<0, PY, PFY),
              EY = ifelse(PY-EY<0, PY, EY),
              TEY = ifelse(PY-TEY<0, PY, TEY),
              Ycor = ifelse(PY-Ycor<0, PY, Ycor),
              Y = ifelse(PY-Y<0, PY, Y))

#############################
### Yield gap calculation ###
#############################

# Calculate TYG using UY as reference
db8 <- db7 %>% 
  mutate(
    ERROR_l = Ycor-Y,      # Error gap
    ERROR_s = Y/Ycor,      # Error gap
    TEYG_l = TEY-Ycor,     # Technical efficiency yield gap using Ycor as basis
    TEYG_s = Ycor/TEY,     # Technical efficiency yield gap using Ycor as basis
    EYG_l = EY-TEY,        # Economic yield gap
    EYG_s = TEY/EY,        # Economic yield gap
    EUYG_l = PFY-EY,       # Feasible yield gap
    EUYG_s = EY/PFY,       # Feasible yield gap
    TYG_l = PY-PFY,        # Technology yield gap
    TYG_s = PFY/PY,        # Technology yield gap
    YG_l = PY-Y,           # Yield gap
    YG_s = Y/PY,           # Yield gap
    YG_l_Ycor = PY-Ycor,   # Yield gap with Ycor as reference
    YG_s_Ycor = Ycor/PY)   # Yield gap with Ycor as reference

# Consistency check of yield gaps.
# ERROR
X_ERROR <- filter(db8, ERROR_l<0) # Half of observation has a negetive error which is what would be expected # CHECK
mean(db8$ERROR_l)
mean(db8$ERROR_s)

# TEYG
X_TEYG <- filter(db8, TEYG_l<0) # Should be zero
mean(db8$TEYG_s)

# EYG
# A number of plots will have to decrease N use Npm < N. In several cases also plots that do not use N
# will have lower Y when they start using N. This is because there yield can be located above the frontier (based on fertilizer users) because of the positive effect of noN.
# If we believe that these plots are structurally different and do not use fertilizer because of better soils, they will in fact use too much N and have to decrease.
X_EYG <- filter(db8, EYG_l<0)        
mean(db8$EYG_s)

# EUYG
# A number of plots have negative EUYG_l because Npm is larger than Nyw, the nitrogen that is required to achieve Potential yield (Yw).
# Not the case here: CHECK
# We have corrected this so check should be 0.
X_EUYG <- filter(db8, EUYG_l<0)        
mean(db8$EUYG_s)

# TYG
X_TYG <- filter(db8, TYG_l<0)        
mean(db8$TYG_s)

#YG
X_YG <- filter(db8, YG_l<0)        
X_YG2 <- filter(db8, YG_l_Ycor<0)        

# Check if separate yield gaps add up to total yield gap
X_overall <- db8 %>%
  mutate(check_l = YG_l/(ERROR_l + TEYG_l + EYG_l + EUYG_l + TYG_l), # Note that for a small number of observatios YG_l=0 resulting in 0/0 which is NaN
         check_s = YG_s/(ERROR_s * TEYG_s * EYG_s * EUYG_s * TYG_s),
         check_l2 = YG_l_Ycor/(TEYG_l + EYG_l + EUYG_l + TYG_l),
         check_s2 = YG_s_Ycor/(TEYG_s * EYG_s * EUYG_s * TYG_s))
summary(X_overall)


# Create database with relevant variables for further analysis
db9_gps <- dplyr::select(db8, hhid, plotid, surveyyear, ZONE, REGNAME, lat, lon, crop_count2, area_gps, yesN, Y, N_gps, Ycor, TEY, EY, PFY, HFY, PY, ERROR_l, ERROR_s, TEYG_l, TEYG_s, EYG_l, EYG_s, 
                     EUYG_l, EUYG_s, TYG_l, TYG_s, YG_l, YG_s, YG_l_Ycor, YG_s_Ycor)

# Clean up
rm(list =grep("X_", ls(), value = TRUE)) # remove checks
rm(db3, db4, db5, db6, db6a, db7, db8)

