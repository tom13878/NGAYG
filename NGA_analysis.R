#######################################
#### CLEAN DATA PREPARE NGA PANEL #####
#######################################


library(plyr)
library(dplyr)
library(stargazer)
library(broom)
library(DescTools)
library(ggplot2)
library(xtable)
library(frontier)
library(moments)
library(tidyr)
library(openxlsx)

wdPath <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap\\Analysis\\NGA"
setwd(wdPath)

options(scipen=999)

# read in the data
NGA2010_raw <- readRDS("Data/NGA_data_2010.rds")
NGA2012_raw <- readRDS("Data/NGA_data_2012.rds")

#######################################
############## CLEANING ###############
#######################################

# remove plots which have more than 7 crops on them
# add dummy variables for number of crops. In this case
# seven is actually the maximum number of crops

#NGA2010 <- NGA2010[!NGA2010$crop_count > 7,]

# cap yield at 15872 kg/ha, the highest potential yield in NGA (not water limited)
NGA2010 <- filter(NGA2010_raw, yld <= 15872.93341)

# Sample includes very small plots <0.005 ha that bias result upwards and very large >35 ha. 
# As we focus on small scale farmers we restrict area size
NGA2010 <- filter(NGA2010, area_gps >=0.01 & area_gps <=10)

# restrict attention to plots that use N < 400. There is one outlier of around 700 and some super large ones that are removed.
NGA2010 <- filter(NGA2010, N < 1000)

# Select relevant variables and complete cases
NGA2010 <- NGA2010 %>% 
  dplyr::select(plotid2010, eaid2010, hhid2010, zone_lsms, region_lsms, district_lsms, 
                AEZ, fs,
                SOC, SOC2, ph, ph2, RootDepth, 
                rain_year, rain_wq, 
                slope, elevation,
                yld, lab = harv_lab, asset, pest, 
                N, P,
                legume, irrig, 
                SOC, SOC2, ph, ph2, RootDepth,
                crop_count, surveyyear,
                rural, area_gps,
                lat, lon,
                YW) 


#######################################
############## CLEANING2 ##############
#######################################

# remove plots which have more than 7 crops on them
#NGA2012 <- NGA2012[!NGA2012$crop_count > 7,]

# cap yield at 15872 kg/ha, the highest potential yield in NGA (not water limited)
NGA2012 <- filter(NGA2012_raw, yld <= 15872.93341)

# Sample includes very small plots <0.005 ha that bias result upwards and very large >35 ha. 
NGA2012 <- filter(NGA2012, area_gps >=0.01 & area_gps <=10)

# restrict attention to plots that use N < 400. There is one outlier of around 700 and some super large ones that are removed.
NGA2012 <- filter(NGA2012, N < 1000)

# Select relevant variables and complete cases
NGA2012 <- NGA2012 %>% 
  dplyr::select(plotid2012, eaid2012, hhid2012, zone_lsms, region_lsms, district_lsms, 
                AEZ, fs, 
                SOC, SOC2, ph, ph2, RootDepth, 
                rain_year, rain_wq, 
                slope, elevation,
                yld, lab = harv_lab, asset, pest, 
                N, P,
                legume, irrig, 
                SOC, SOC2, ph, ph2, RootDepth,
                crop_count, surveyyear,
                rural, area_gps,
                lat, lon,
                YW)

#######################################
############ BALANCE PANEL ############
#######################################

NGA2010 <- rename(NGA2010, plotid = plotid2010, hhid = hhid2010, eaid = eaid2010)
NGA2012 <- rename(NGA2012, plotid = plotid2012, hhid = hhid2012, eaid = eaid2012)

# Potential yield information that will be linked in later
# Has many NA so need to be extracted.
py <- rbind(NGA2010, NGA2012) %>%
                    select(YW, hhid, eaid, lat, lon, plotid, surveyyear)

NGA2010 <- NGA2010 %>%
            select(-YW) %>%
            do(filter(., complete.cases(.)))

NGA2012 <- NGA2012 %>%
            select(-YW) %>%
            do(filter(., complete.cases(.)))

NGA2010.2 <- NGA2010[NGA2010$hhid %in% NGA2012$hhid,] 
NGA2012.2 <- NGA2012[NGA2012$hhid %in% NGA2010$hhid,] 

# Balanced panel
db0 <- rbind(NGA2010.2, NGA2012.2) 

# Unbalanced panel
db0 <- rbind(NGA2010, NGA2012)

#rm(list=c("NGA2010", "NGA2012", "NGA2010.2", "NGA2012.2"))


######################################
######## Modify and add variables ####
######################################

# Only select main four farming systems
#summary(db0$fs)
#db0 <- filter(db0, fs %in% c("Agro-pastoral", "Cereal-root crop mixed", "Humid lowland tree crop", "Root and tuber crop"))

# CHECK Remove observations with 0 asset and 0 harv_lab
db0 <- filter(db0, asset!= 0 & lab != 0)

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

# Crop count > 1
db0$crop_count2[db0$crop_count==1] <- 1
db0$crop_count2[db0$crop_count>1] <- 0

# additional variables
db0 <- db0 %>% mutate (logyld=log(yld),
                       yesN = ifelse(N>0, 1,0), # Dummy when plot does not use fertilizer, following approach of Battese (1997)
                       noN = ifelse(N<=0, 1,0), # Dummy when plot does not use fertilizer, following approach of Battese (1997)
                       logN = log(pmax(N, noN)), # maximum of dummy and N following Battese (1997)
                       yesP = ifelse(P>0, 1,0), # Dummy when plot does not use fertilizer, following approach of Battese (1997)
                       noP = ifelse(P<=0, 1,0), # Dummy when plot does not use fertilizer, following approach of Battese (1997)
                       logP = log(pmax(P, noP)), # maximum of dummy and N following Battese (1997)
                       logasset = log(asset),
                       loglab = log(lab),
                       logarea = log(area_gps),
                       rain_wq_2 = rain_wq*rain_wq,
                       surveyyear2 = replace(surveyyear==2010, 1, 0))

# Add CRE variables
db0 <- ddply(db0, .(hhid), transform,
             loglab_bar=mean(loglab, na.rm=TRUE),
             logN_bar=mean(logN, na.rm=TRUE),
             noN_bar=mean(noN, na.rm=TRUE),
             logP_bar=mean(logP, na.rm=TRUE),
             noP_bar=mean(noP, na.rm=TRUE),
             area_bar=mean(area_gps, na.rm=TRUE),
             logarea_bar=mean(logarea, na.rm=TRUE),
             irrig_bar=mean(irrig, na.rm = TRUE),
             pest_bar=mean(pest, na.rm = TRUE),
             legume_bar=mean(legume, na.rm = TRUE),
             crop_count_bar=mean(crop_count2, na.rm=TRUE))
             

# # Profit maximizing yield analysis
# # Load and merge price data
# Prices <- readRDS("./Analysis/NZA/Data/Prices.rds")
# Prices <- Prices %>% group_by(type) %>%
#   mutate(price = winsor2(price, 5)) %>% # Winsor does not change prices
#   dplyr::select(zone, region_name_lsms, surveyyear, ea_id, price, type) %>%
#   spread(type, price) %>% 
#   mutate(surveyyear = as.numeric(as.character(surveyyear)))
# 
# # Merge with panel data
# db1 <- left_join(db0, Prices)

db1 <-db0

# Drop unused levels
db1<-droplevels(db1)



#######################################
############## ANALYSIS ###############
#######################################

# Cobb Douglas
olsCD1 <- lm(logyld ~ noN + AEZ:logN + logasset + loglab + 
               logarea +
               irrig +
               fs +
               slope + elevation +
               SOC2 + phdum2 + 
               rain_wq + rain_wq_2+
               crop_count2 + surveyyear2,
             data = db1)


olsCD2 <- lm(logyld ~ noN + AEZ:logN + logasset + loglab + 
               logarea +
               irrig + 
               fs +
               slope + elevation +
               SOC2 + phdum2 + 
               rain_wq + rain_wq_2+
               crop_count2 + surveyyear2 + 
               noN_bar + logN_bar + noP_bar + logP_bar +
               loglab_bar + logarea_bar + 
              irrig_bar + legume_bar+  crop_count_bar,
             data = db1)

stargazer(olsCD1, olsCD2, type="text")

# Assess skewness of OLS - should be left skewed which is confirmed.
hist( residuals(olsCD1), 15)
hist( residuals(olsCD2), 15)
library("moments")
skewness(residuals(olsCD1))
skewness(residuals(olsCD2))

# library(purrr)
# CD <- db0 %>%
#       split(.$fs) %>%
#       map(~ lm(logyld ~ noN, data =. ))
# 
# 
# CD2 <- db0 %>%
#   slice_rows("fs") %>%
#   by_slice(partial(lm, logyld ~ noN))
# 
# CD <- db0 %>%
#   group_by(fs) %>%
#   do(fit = lm(logyld ~ noN, .))
# 
# CD %>% tidy(fit)
# CD %>% glance(fit)
# CD %>% augment(fit)

# 
# # By farming system
# # NB: droplevels is essential as otherwise sfa() does not work
# unique(db1$fs)
# ap <- filter(db0, fs == "Agro-pastoral") %>% droplevels()
# crcm <- filter(db0, fs == "Cereal-root crop mixed") %>% droplevels()
# hltc <- filter(db0, fs == "Humid lowland tree crop") %>% droplevels()
# rtc <- filter(db0, fs == "Root and tuber crop") %>% droplevels()
# 
# # By AEZ
# twsa <- filter(db0, AEZ == "Tropic - warm / semiarid") %>% droplevels()
# twsh <- filter(db0, AEZ == "Tropic - warm / subhumid") %>% droplevels()
# tcsh <- filter(db0, AEZ == "Tropic - cool / subhumid") %>% droplevels()
# twh <- filter(db0, AEZ == "Tropic - warm / humid") %>% droplevels()
# 
# cd_f <- function(df){
#   reg <- lm(logyld ~ noN + logN + logasset + logharv_lab + 
#               logarea +
#               legume + irrig + 
#               slope + elevation +
#               SOC2 + phdum2 + 
#               rain_wq + rain_wq2+
#               crop_count2 + surveyyear2,
#             data = df)
#   return(reg)
# }
# 
# cd_cre_f <- function(df){
#   reg <- lm(logyld ~ noN + fs:logN + logasset + logharv_lab + 
#           logarea +
#           legume + irrig + 
#           slope + elevation +
#           SOC2 + phdum2 + 
#           rain_wq + rain_wq2+
#           crop_count2 + surveyyear2 + 
#           noN_bar + logN_bar + loglab_bar + logarea_bar + 
#           irrig_bar + legume_bar+  crop_count_bar,
#           data = df)
#   return(reg)
# }
# 
# cd_ap <- cd_cre_f(ap)
# cd_crcm <- cd_cre_f(crcm)
# cd_hltc <- cd_cre_f(hltc)
# cd_rtc <- cd_cre_f(rtc)
# stargazer(cd_ap, cd_crcm, cd_hltc, cd_rtc, type = "text")
# 
# cd_twsa <- cd_cre_f(twsa)
# cd_twsh <- cd_cre_f(twsh)
# cd_tcsh <- cd_cre_f(tcsh)
# cd_twh <- cd_cre_f(twh)
# stargazer(cd_twsa, cd_twsh, cd_tcsh, cd_twh, type = "text")
# 


# Frontier estimation
sfaCD1 <- sfa(logyld ~ noN + AEZ:logN + logasset + loglab +
      logarea +
      slope + elevation +
      SOC2 +  phdum2 +
      rain_wq + rain_wq_2+
      fs +
      crop_count2 + surveyyear2,
    data = db1, maxit = 1500, restartMax = 20, printIter = 1, tol = 0.000001)

summary(sfaCD1, extraPar = TRUE)
lrtest(sfaCD1)

sfaCD2 <- sfa(logyld ~ noN + AEZ:logN + logasset + loglab +
                logarea +
                slope + elevation +
                SOC2 + phdum2 +  
                rain_wq + rain_wq_2 +
                crop_count2 + surveyyear2 + 
                noN_bar + logN_bar + loglab_bar + logarea_bar + 
                crop_count_bar,
              data = db1, maxit = 1500, restartMax = 20, printIter = 1, tol = 0.000001)

summary(sfaCD2, extraPar = TRUE)
lrtest(sfaCD2)



# Compute profit maximizing Pn per zone and other summary statistics
# Select model
model <- sfaCD2

# Note that MPP cannot be calculated for plots with N=0 and are therefore set to 0.
db2_sfaCD2 <- db1 %>% mutate(elastfert = ifelse(AEZ=="Tropic - warm /semiarid", coef(model)["AEZTropic - warm / semiarid:logN"], 
                                                ifelse(AEZ=="Tropic - warm / subhumid", coef(model)["AEZTropic - warm / subhumid:logN"],
                                                       ifelse(AEZ=="Tropic - warm / humid", coef(model)["AEZTropic - warm / humid:logN"], 
                                                              coef(model)["AEZTropic - cool / subhumid:logN"]))),
                             phconstant = ifelse(phdum==2, coef(model)["phdum22"], 
                                                 ifelse(phdum==3, coef(model)["phdum23"],0)),
                             lnA = coef(model)["(Intercept)"] +
                               (coef(model)["noN"]*noN) +
                               (coef(model)["logarea"]*logarea) +
                               #(coef(model)["hybrd"]*hybrd) +
                               #(coef(model)["manure"]*manure) +
                               #(coef(model)["herb"]*herb) +
                               #(coef(model)["fung"]*fung) +
                               #(coef(model)["legume"]*legume) +
                               #(coef(model)["irrig"]*irrig) +
                               (coef(model)["slope"]*slope) +
                               (coef(model)["elevation"]*elevation) +
                               (coef(model)["SOC2"]*SOC2) +
                               (phconstant) +
                               (coef(model)["rain_wq"]* rain_wq) +
                               (coef(model)["rain_wq_2"]*rain_wq_2) +
                               (coef(model)["crop_count2"]*crop_count2),
                             lnA2 = lnA,
                             constantfactor = exp(lnA2)*elastfert*(lab^coef(model)["loglab"]),
                             MPP= ifelse(N==0,NA,exp(lnA2)*elastfert*(lab^coef(model)["loglab"])*(N^(elastfert-1)))
                             #Npm = (Pn/(constantfactor*Pm))^(1/(elastfert-1)),
                             #Ndif = N-Npm
                             )                       


sumzone_sfaCD2<- db2_sfaCD2 %>% group_by(fs) %>%
  summarize(
    Ncon=mean(ifelse(N>0, N, NA), na.rm = T),
    N=mean(N, na.rm=T),
    #Npm=mean(Npm, na.rm=T),
    MPPmean=mean(MPP[!is.infinite(MPP)], na.rm=T),
    #MVC=mean((Pm[!is.infinite(MPP)]*MPP[!is.infinite(MPP)])/Pn[!is.infinite(MPP)], na.rm=T),
    #Ndif=mean(Ndif, na.rm=T),
    Number=n())


##############################
### Calculate yield levels ###
##############################

# set model
model <- sfaCD2

# 1. TEYG: Technical efficient yield gap

# Calculate yield level at 100% TE on the frontier with given inputs
# We estimate the error using the sfa formula and compute Ycor.
# In sfa, Y = TEY + error(v) - efficiency(u) Kumbakar et al. (2015), A practitioner's guide, p.48-49
# We want to filter out/correct for the error. Ycor = TEY - u = Y + e
# Since we do not know the error we calculate Ycor as TEY - u.
# Efficiency as produced by package frontier is defined as TE = exp(-u) so u is -log(TE)
# As the model is in logs, TEY = exp(ln(TEY))
# Ycor = exp(ln(TEY)) - [-log(TE)]

db3 <- db2_sfaCD2 %>%
  dplyr::select(eaid, hhid, plotid, surveyyear, region_lsms, district_lsms, zone_lsms, area_gps, crop_count2, lat, lon, lnA, lnA2, noN, yesN, loglab, lab, elastfert, N, Y=yld) %>%
  mutate(
    Ycor = exp(as.numeric(fitted(model))+log(as.numeric(efficiencies(model)))), 
    err = Ycor-Y,
    TEY = exp(as.numeric(fitted(model))),
    TE = as.numeric(efficiencies(model)),
    resid = as.numeric(resid(model))
  )

# A number of plots have yld higher than the estimated frontier (Y-TEY>0) caused by the random error. 
# By far most of these are plots that do not use fertilizer. They probably have high yield because of measurement error, better soil properties or unknown factors.
above_frontier_check <- filter(db3, Y-TEY>0)
mean(db3$Y-db3$TEY)


# 2. EY: Economic yield 
# Calculate optimal Npm when using Pn
# Note that noN is still part of lnA2 because we assume that plots without N are structurally different from those with N, for instance better soil.

# It is possible that Npm is larger than Npy, which is not possible from a biophysical perspective.
# We cap Npm at Npy.

# # Based on experimental plot data (see Excel), we set Npy to 150. NB CHANGE FOR ETHIOPIA
Npy <- 150
# # Cap Npm
# db3 <- mutate(db3, Npm = ifelse(Npm>Npy, Npy, Npm))
# 
# db4 <- db3 %>% 
#   mutate(EY = exp(
#     lnA2 + 
#       coef(model)["loglab"]*loglab +
#       # coef(model)["logasset"]*logasset + 
#       elastfert*log(Npm)))
# 

db4 <- db3

# 3 PFY: Potential farm yield
db5 <- db4 %>%
  mutate(PFY = exp(
    lnA2 + 
      coef(model)["loglab"]*loglab +
      # coef(model)["logasset"]*logasset + 
      elastfert*log(Npy)))

# 4. PY: Potential yield
# CHECK NB: the join still includes a handful of duplicated!!!!!!
# Merge Yield potential with maize plot database
db6 <- py %>% rename(PY = YW) %>% 
  mutate(PY=PY*1000) %>% left_join(db5, .)

# A large number of plots have missing YW values because region is not covered by GYGA.
# We assume for the moment that country maximum water limited yield (Yw) is reasonable proxy for missing information.
# Might scale this down to see the effect.
GYGA_YW <- 1408.07157931127
db6 <- mutate(db6, PY = ifelse(is.na(PY), GYGA_YW, PY))


#####################################################
### Yield levels consistency check and correction ###
#####################################################

# Because of imputation of TY or measurement error, Yield (Y and Ycor), Technical efficiency yield (TEY), Economic yield (EY) and Unexploitable yield (UY) 
# can be higher than Potential yield (PYcor). We check for this.

Y_Ycor_check <- filter(db6, Ycor-Y<0)
PY_Y_check <- filter(db6, PY-Y<0)
PY_Y_cor_check <- filter(db6, PY-Ycor<0)
PY_TE_check <- filter(db6, PY-TEY<0)
#PY_EY_check <- filter(db6, PY-EY<0)
PY_PFY_check <- filter(db6, PY-PFY<0)

# Compare different yield levels
# Picture shows that PFY is much to high for plots with the lowest PY. 
# This is probably due to the uniform use of Npf of 150 N/ha.
# It would be better to have zone specific Npf values.
ggplot(data = db6, aes(y = PY, x = PFY)) +
  geom_point() +
  #geom_jitter(position=position_jitter(width=.1, height=0))+
  geom_abline(aes(Y = Ycor), slope=1, intercept=0) +
  coord_fixed() +
  scale_y_continuous(limits=c(0, 10000)) +
  scale_x_continuous(limits=c(0, 10000))

# Compare error and resid
# Not clear what resid is? As the following plot shows, TEYG_s = TE :CHECK]
ggplot(data = db6, aes(y = err, x = resid)) +
  geom_point() 

# Compare Sfa TA scores with mannually computed TEYG_s => identical as they should be
db6a <- mutate(db6, TEYG_s =Ycor/TEY)
ggplot(data = db6a, aes(y = TEYG_s, x = TE)) +
  geom_point()

#  We cap all values at PY because we consider this as an absolute potential and recalculate all gaps.
db7 <- mutate(db6, PFY = ifelse(PY-PFY<0, PY, PFY),
              #EY = ifelse(PY-EY<0, PY, EY),
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
    RYG_l = PFY - TEY,      # Resource yield gap
    RYG_s = TEY/PFY,         # Resource yield gap
    #EYG_l = EY-TEY,        # Economic yield gap
    #EYG_s = TEY/EY,        # Economic yield gap
    #EUYG_l = PFY-EY,       # Economically unexploitable yield gap
    #EUYG_s = EY/PFY,       # Economically unexploitable yield gap
    TYG2_l = PY-TEY,       # Technology yield gap 2
    TYG2_s = TEY/PY,       # Technology yield gap 2
    TYG_l = PY-PFY,        # Technology yield gap
    TYG_s = PFY/PY,         # Technology yield gap
    YG_l = PY-Y,           # Yield gap
    YG_s = Y/PY,           # Yield gap
    YG_l_Ycor = PY-Ycor,   # Yield gao with Ycor as reference
    YG_s_Ycor = Ycor/PY)   # Yield gap with Ycor as reference

# Consistency check of yield gaps.
# ERROR
ERROR_check <- filter(db8, ERROR_l<0) # Half of observation has a negetive error which is what would be expected # CHECK
mean(db8$ERROR_l)
mean(db8$ERROR_s)

# TEYG
TEYG_check <- filter(db8, TEYG_l<0) # Should be zero
mean(db8$TEYG_s)

# EYG
# A number of plots will have to decrease N use Npm < N. In several cases also plots that do no use N
# will have lower Y when they start using N. This is because there yield can be located above the frontier (based on fertilizer users) because of the positive effect of noN.
# If we believe that these plots are structurally different and do not use fertilizer because of better soils, they will in fact use too much N and have to decrease.
#EYG_check <- filter(db8, EYG_l<0)        
#mean(db8$EYG_s)

# EUYG
# A number of plots have negative EUYG_l because Npm is larger than Nyw, the nitrogen that is required to achieve Potential yield (Yw).
# Not the case here: CHECK
# We have corrected this so check should be 0.
#EUYG_check <- filter(db8, EUYG_l<0)        
#mean(db8$EUYG_s)

# RYG
RYG_check <- filter(db8, RYG_l<0)        
mean(db8$RYG_s)

# TYG
TYG_check <- filter(db8, TYG_l<0)        
mean(db8$TYG_s)

# TYG2
TYG2_check <- filter(db8, TYG2_l<0)        
mean(db8$TYG2_s)

#YG
YG_check <- filter(db8, YG_l<0)        
YG_check2 <- filter(db8, YG_l_Ycor<0)        

# Check if separate yield gaps add up to total yield gap #Check_l has a zero value - CHECK
# Overall_check <- db8 %>%
#   mutate(check_l = YG_l/(ERROR_l + TEYG_l + EYG_l + EUYG_l + TYG_l), # Note that for a small number of observatios YG_l=0 resulting in 0/0 which is NaN
#          check_s = YG_s/(ERROR_s * TEYG_s * EYG_s * EUYG_s * TYG_s),
#          check_l2 = YG_l_Ycor/(TEYG_l + EYG_l + EUYG_l + TYG_l),
#          check_s2 = YG_s_Ycor/(TEYG_s * EYG_s * EUYG_s * TYG_s))
# summary(Overall_check)

Overall_check <- db8 %>%
  mutate(check_l = YG_l/(ERROR_l + TEYG_l + TYG2_l), # Note that for a small number of observatios YG_l=0 resulting in 0/0 which is NaN
         check_s = YG_s/(ERROR_s * TEYG_s * TYG2_s),
         check_l2 = YG_l_Ycor/(TEYG_l + TYG2_l),
         check_s2 = YG_s_Ycor/(TEYG_s * TYG2_s))
summary(Overall_check)

# Create database with relevant variables for further analysis
db9 <- dplyr::select(db8, hhid, plotid, eaid, zone_lsms, region_lsms, lat, lon, crop_count2, area_gps, yesN, Y, N, Ycor, TEY, PY, ERROR_l, ERROR_s, TEYG_l, TEYG_s, 
                     TYG2_l, TYG2_s, YG_l, YG_s, YG_l_Ycor, YG_s_Ycor)



#######################################
####### FIGURES AND TABLES ############
#######################################

# Number of households and plots per year
HH <- db9 %>% 
  summarize(HH = length(unique(hhid)),
            Plots = n())


dbsum <- db2_sfaCD2 %>% dplyr::select(yld, lab, yesN, N, area_gps, SOC2, rain_wq, slope, elevation, crop_count2, AEZ)
sumstat <- stargazer(as.data.frame(dbsum), type = "text", digits=2, out=".\\Graphs\\summaryStat.html")


# Table with key information per region_lsms and subtotals
Yieldsum <- bind_rows(
  db9 %>% 
    dplyr::select(Y, N, yesN, zone_lsms, area_gps) %>%
    group_by(zone_lsms) %>%
    summarize(Yield = mean(Y),
              Yield_w = (sum(Y*area_gps)/sum(area_gps)),
              NitrogenUser = round(mean(yesN)*100, digits=1),
              Number=n()),
  db9 %>%
    dplyr::select(Y, N, yesN, area_gps, zone_lsms) %>%
    summarize(zone_lsms = "Total",  
              Yield = mean(Y),
              Yield_w = (sum(Y*area_gps)/sum(area_gps)),
              NitrogenUser = round(mean(yesN)*100, digits=1),
              Number=n())
) %>% arrange(zone_lsms)

# Table with key information per region and subtotals for pure maize plots
Yieldsum_pure <- bind_rows(
  db9 %>%
    filter(crop_count2==1) %>%
    dplyr::select(Y, N, yesN, zone_lsms, area_gps) %>%
    group_by(zone_lsms) %>%
    summarize(Yield_p = mean(Y),
              Yield_w_p = (sum(Y*area_gps)/sum(area_gps))
    ),
  db9 %>%
    filter(crop_count2==1) %>%
    dplyr::select(Y, N, yesN, zone_lsms, area_gps) %>%
    summarize(zone_lsms = "Total", 
              Yield_p = mean(Y),
              Yield_w_p = (sum(Y*area_gps)/sum(area_gps))
    )
)


Nitrogensum <- bind_rows(
  db9 %>% 
    dplyr::select(Y, N, yesN, zone_lsms) %>%
    filter(yesN ==1) %>%
    group_by(zone_lsms) %>%
    summarize(Nitrogen = mean(N)),
  db9 %>%
    dplyr::select(Y, N, yesN, zone_lsms) %>%
    filter(yesN ==1) %>%
    summarize(zone_lsms= "Total", Nitrogen = mean(N))
) 


Pricessum <- bind_rows(
  Prices %>% 
    dplyr::select(zone_lsms, Pn, Pm) %>% 
    do(filter(., complete.cases(.))) %>%
    group_by(zone_lsms) %>%
    summarize(
      NitrogenPrice = mean(Pn, na.rm=T),
      MaizePrice = round(mean(Pm, na.rm=T), digits=0)),
  Prices %>% 
    dplyr::select(zone_lsms, Pn, Pm) %>% 
    do(filter(., complete.cases(.))) %>%
    summarize(zone_lsms = "Total",
              NitrogenPrice = mean(Pn, na.rm=T),
              MaizePrice = round(mean(Pm, na.rm=T), digits=0))
) 

Zonalsum <- left_join(Yieldsum, Nitrogensum) %>%
  left_join(., Yieldsum_pure) %>%
  left_join(., Pricessum) %>%
  dplyr::select(zone_lsms, Number, Yield_w, Yield_w_p, NitrogenUser, Nitrogen, NitrogenPrice, MaizePrice) %>%
  arrange(zone_lsms)
Zonalsum <- xtable(Zonalsum, digits = c(0,0,0,0,0,0,0,0,0))
print(Zonalsum, type="html", file=".\\Analysis\\ETH\\Graphs\\Zonal.html")


# SFA table
# Easier to cut and paste in Excel
#sfaTable <- as.data.frame(summary(sfaCD2, extraPar = F)$mleParam)
#print(xtable(sfaTable), type="html", file=".\\Analysis\\TZA\\Graphs\\Sfa.html", digits=3)
summary(sfaCD2, extraPar = TRUE)
lrtest(sfaCD2)

# Essentials for powerpoint
sfatable <- as.data.frame(summary(sfaCD2)$mleParam)[,c(1,3)]
# Production function
sfatable_pf <- sfatable[c(1:22),]
# Efficiency function
#sfatable_ef <- sfatable[c(23:29),]

# Table with yield levels
YieldLevels <- bind_rows(
  db9 %>% 
    dplyr::select(zone_lsms = zone_lsms, Y, Ycor, TEY, PY, area_gps) %>%
    group_by(zone_lsms) %>%
    summarize(Y =(sum((Y)*area_gps)/sum(area_gps)),
              Ycor = (sum((Ycor)*area_gps)/sum(area_gps)),
              TEY = (sum((TEY)*area_gps)/sum(area_gps)),
              #EY = (sum((EY)*area_gps)/sum(area_gps)),
              #PFY = (sum((PFY)*area_gps)/sum(area_gps)),
              PY = (sum((PY)*area_gps)/sum(area_gps))
    ),
  db9 %>% 
    dplyr::select(zone_lsms = zone_lsms, Y, Ycor, TEY, PY, area_gps) %>%
    summarize(zone_lsms = "Total", 
              Y =(sum((Y)*area_gps)/sum(area_gps)),
              Ycor = (sum((Ycor)*area_gps)/sum(area_gps)),
              TEY = (sum((TEY)*area_gps)/sum(area_gps)),
              #EY = (sum((EY)*area_gps)/sum(area_gps)),
              #PFY = (sum((PFY)*area_gps)/sum(area_gps)),
              PY = (sum((PY)*area_gps)/sum(area_gps)))) %>%
  dplyr::select(zone_lsms, Y, Ycor, TEY, PY)


#YieldLevels <- xtable(YieldLevels, digits = c(0,0,0,0,0,0,0,0))
#print(YieldLevels, type="html", file=".\\Analysis\\ETH\\Graphs\\YieldLevels.html")


# Table with relative yield gap information per zone
# Note that by definition, YG_s computed by weighting individual YG_s values is not the same as multiplication of weighted TEYG_s etc.
# We therefore calculate YG_s as the product of the weighted components.
ZonalYieldGap_s <- bind_rows(
  db9 %>% 
    dplyr::select(zone_lsms = zone_lsms, ERROR_s, TEYG_s, TYG2_s, YG_s_Ycor, YG_s, area_gps) %>%
    group_by(zone_lsms) %>%
    summarize(ERROR_s =(sum((ERROR_s)*area_gps)/sum(area_gps)),
              TEYG_s = (sum((TEYG_s)*area_gps)/sum(area_gps)),
              #EYG_s = (sum((EYG_s)*area_gps)/sum(area_gps)),
              #EUYG_s = (sum((EUYG_s)*area_gps)/sum(area_gps)),
              TYG2_s = (sum((TYG2_s)*area_gps)/sum(area_gps)),
              YG_s = (sum((YG_s)*area_gps)/sum(area_gps)),
              YG_s_Ycor = (sum((YG_s_Ycor)*area_gps)/sum(area_gps)),
              ERROR = (1-ERROR_s)*100,
              TEYG = (1-TEYG_s)*100,
              #EYG = (1-EYG_s)*100,
              #EUYG = (1-EUYG_s)*100,
              TYG2 = (1-TYG2_s)*100,
              YG = (1-(ERROR_s*TEYG_s*TYG2_s))*100,
              YG_Ycor = (1-(TEYG_s*TYG2_s))*100
    ),
  db9 %>% 
    dplyr::select(zone_lsms = zone_lsms, ERROR_s, TEYG_s, TYG2_s, YG_s_Ycor, YG_s, area_gps) %>%
    summarize(zone_lsms = "Total", 
              ERROR_s =(sum((ERROR_s)*area_gps)/sum(area_gps)),
              TEYG_s = (sum((TEYG_s)*area_gps)/sum(area_gps)),
              #EYG_s = (sum((EYG_s)*area_gps)/sum(area_gps)),
              #EUYG_s = (sum((EUYG_s)*area_gps)/sum(area_gps)),
              TYG2_s = (sum((TYG2_s)*area_gps)/sum(area_gps)),
              YG_s = (sum((YG_s)*area_gps)/sum(area_gps)),
              YG_s_Ycor = (sum((YG_s_Ycor)*area_gps)/sum(area_gps)),
              ERROR = (1-ERROR_s)*100,
              TEYG = (1-TEYG_s)*100,
              #EYG = (1-EYG_s)*100,
              #EUYG = (1-EUYG_s)*100,
              TYG2 = (1-TYG2_s)*100,
              YG = (1-(ERROR_s*TEYG_s*TYG2_s))*100,
              YG_Ycor = (1-(TEYG_s*TYG2_s))*100)) %>%
  dplyr::select(zone_lsms, TEYG, TYG2, YG = YG_Ycor)


ZonalYieldGap_s <- xtable(ZonalYieldGap_s, digits = c(0,0,0,0,0,0,0))
print(ZonalYieldGap_s, type="html", file=".\\Analysis\\ETH\\Graphs\\ZonalYG_s.html")

# Table with absolute yield gap information per zone
# Note that by definition, YG_s computed by weighting individual YG_s values is not the same as multiplication of weighted TEYG_s etc.
# We therefore calculate YG_s as the product of the weighted components.
ZonalYieldGap_l <- bind_rows(
  db9 %>% 
    dplyr::select(zone_lsms = zone_lsms, ERROR_l, TEYG_l, TYG2_l, YG_l_Ycor, YG_l, area_gps) %>%
    group_by(zone_lsms) %>%
    summarize(ERROR_l =(sum((ERROR_l)*area_gps)/sum(area_gps)),
              TEYG_l = (sum((TEYG_l)*area_gps)/sum(area_gps)),
              #EYG_l = (sum((EYG_l)*area_gps)/sum(area_gps)),
              #EUYG_l = (sum((EUYG_l)*area_gps)/sum(area_gps)),
              TYG2_l = (sum((TYG2_l)*area_gps)/sum(area_gps)),
              YG_l = (sum((YG_l)*area_gps)/sum(area_gps)),
              YG_l_Ycor = (sum((YG_l_Ycor)*area_gps)/sum(area_gps)),
              YG_lcheck = (ERROR_l+TEYG_l+TYG2_l)),
  db9 %>% 
    dplyr::select(zone_lsms = zone_lsms, ERROR_l, TEYG_l, TYG2_l, YG_l_Ycor, YG_l, area_gps) %>%
    summarize(zone_lsms = "Total", 
              ERROR_l =(sum((ERROR_l)*area_gps)/sum(area_gps)),
              TEYG_l = (sum((TEYG_l)*area_gps)/sum(area_gps)),
              #EYG_l = (sum((EYG_l)*area_gps)/sum(area_gps)),
              #EUYG_l = (sum((EUYG_l)*area_gps)/sum(area_gps)),
              TYG2_l = (sum((TYG2_l)*area_gps)/sum(area_gps)),
              YG_l = (sum((YG_l)*area_gps)/sum(area_gps)),
              YG_l_Ycor = (sum((YG_l_Ycor)*area_gps)/sum(area_gps)),
              YG_lcheck = (ERROR_l+TEYG_l+TYG2_l))
) %>%
  #dplyr::select(-ERROR_l, -YG_lcheck, - YG_l)
  
  # # Addtional code to compute percentage of YG
  #%>%
  mutate(
    TEYG = 100*TEYG_l/YG_l_Ycor,
    #EYG = 100*EYG_l/YG_l_Ycor,
    #EUYG = 100*EUYG_l/YG_l_Ycor,
    TYG2 = 100*TYG2_l/YG_l_Ycor,
    YG = 100*(TEYG_l + TYG2_l)/YG_l_Ycor) %>%
  dplyr::select(-ERROR_l:-YG_lcheck)


ZonalYieldGap_l <- xtable(ZonalYieldGap_l, digits = c(0,0,0,0,0,0,0))
print(ZonalYieldGap_l, type="html", file=".\\Analysis\\ETH\\Graphs\\ZonalYG_l.html")

# Calculation of potential increase in production when gap is closed on the basis of sample
GapClose1 <- mutate(db9, PROD = Y * area_gps,
                    ERROR_close = ERROR_l*area_gps,
                    TEYG_close = TEYG_l*area_gps,
                    #EYG_close = EYG_l*area_gps,
                    #EUYG_close = EUYG_l*area_gps,
                    TYG2_close = TYG2_l*area_gps,
                    POTPROD = PROD + ERROR_close + TEYG_close + TYG2_close)

# Total increase in yield per year
GapClose1a <- GapClose1 %>%
  #group_by(zone_lsms) %>%
  summarize(PROD = sum(PROD, na.rm=T)/1000000,
            ERROR_close = sum(ERROR_close, na.rm=T)/1000000,
            TEYG_close = sum(TEYG_close, na.rm=T)/1000000,
            #EYG_close = sum(EYG_close, na.rm=T)/1000000,
            #EUYG_close = sum(EUYG_close, na.rm=T)/1000000,
            TYG2_close = sum(TYG2_close, na.rm=T)/1000000,
            POTPROD = sum(POTPROD, na.rm=T)/1000000)


# Calculation of potential increase in production when gap is closed on the basis of SPAM and FAO data.
# As the yield in the LSMS is much lower than the FAO/SPAM yield and also that of the LSMS we apply the different yield shares as found above to the base yield of SPAM.
# We use Ycor as base. This means we assume there was an error (e) and this is corrected for.

SPAMData <- read.csv("./Data/SPAMData_NGA.csv") %>%
  rename(Y_SPAM = yield, PROD = TargetProduction, zone_lsms = zone)


# Closing of yield gaps per zone
# Note that for some regions, notably those with very low potential yield (Central and Western), closing TEY and EY already results in
# Closing the gap. To avoid negative closing the EYG, EUYG and TYG are capped.
# The reason for overshooting can be caused by a variety of factors, including mismeasurement. Most likely is that Nyp is too high for regions
# With a very low potential. The all over impact is low as the involved regions have very limited maize production.


GapClose2 <- db9 %>% 
  group_by(zone_lsms) %>%
  summarize(
    TEYG_s = sum(TEYG_s*area_gps)/sum(area_gps),
    #EYG_s = sum(EYG_s*area_gps)/sum(area_gps),
    #TYG2_s = (sum((TYG2_s)*area_gps)/sum(area_gps)), # TYG_s based on LSMS yield, not used
    #YG_s = (sum((YG_s)*area_gps)/sum(area_gps)), # YG_s based on LSMS yield, not used
    #EUYG_s = sum(EUYG_s*area_gps)/sum(area_gps),
    PY = mean(PY, na.rm=T)) %>% # Average of potential yield from GYGA
  left_join(SPAMData, .) %>%
  mutate(
    TEY_SPAM = Y_SPAM/TEYG_s, # TEY using SPAM yield as reference
    #EY_SPAM = TEY_SPAM/EYG_s, # EY using SPAM yield as reference
    #EY_SPAM = ifelse(PY-EY_SPAM<0, PY, EY_SPAM), # Correct EY if impact of TEYG and EYG results in yield larger than PY.
    #EYG_s_SPAM =  TEY_SPAM/EY_SPAM, # Recalculate EYG_s
    #UY_SPAM = EY_SPAM/EUYG_s, # UY using SPAM yield as reference
    #UY_SPAM = ifelse(PY-UY_SPAM<0, PY, UY_SPAM), # Correct UY if impact of TEYG and EYG results in yield larger than PY.
    #EUYG_s_SPAM =  EY_SPAM/UY_SPAM, # Recalculate UEYG_s
    TYG2_s_SPAM = TEY_SPAM/PY, # Recalculate TYG_s 
    TEYG_SPAM = ifelse(PY-TEY_SPAM<0, PY, TEY_SPAM), # Correct TEY if impact of TEYG and EYG results in yield larger than PY.
    TYG2_s_SPAM =  TEYG_SPAM/PY, # Recalculate TYG_s_SPAM
    TEYG_s_SPAM = Y_SPAM/TEYG_SPAM,
    check = TEYG_s_SPAM*TYG2_s_SPAM, #check if multiplication of different parts is the same as total
    YG_s = Y_SPAM/PY, # YG_s using SPAM yield as reference
    PTEYG = PROD/TEYG_s_SPAM, # Total production when TEYG is closed
    #PEYG = PTEYG/EYG_s_SPAM, # Total production when EYG is closed
    #PEUYG = PEYG/EUYG_s_SPAM, # Total production when EUYG is closed
    PTYG = PTEYG/TYG2_s_SPAM, # Total production when TYG is closed
    POTPROD = PROD/YG_s, # Total production when YG is closed
    TEYG_close = PTEYG - PROD, # Additional production when closing TEYG
    #EYG_close = PEYG - PTEYG, # Additional production when closing EYG
    #EUYG_close = PEUYG - PEYG, # Additional production when closing EUYG
    TYG_close = POTPROD - PTEYG) %>%
  mutate(check2 = TEYG_close + TYG_close+PROD)

GapClose2a <- GapClose2 %>% 
  summarize(PROD = sum(PROD/1000000, na.rm = T), # in million tons
            TEYG_close = sum(TEYG_close/1000000, na.rm = T),
            #EYG_close = sum(EYG_close/1000000, na.rm = T),
            TYG_close = sum(TYG_close/1000000, na.rm = T),
            #EUYG_close = sum(EUYG_close/1000000, na.rm = T), 
            POTPROD = sum(POTPROD/1000000, na.rm = T)) %>%
  mutate(check2 = TEYG_close +  TYG_close+PROD)

# Method above gives problem because TEYG is so small, calculation of TEY > PY
# Alternative measure is to match on the basis of prod, not yield.
# In other words, we assume that the actual producion is produced by farmers in our sample that have lower yield than in SPAM
# instead of assuming that the gaps can be applied on the yield from SPAM

SPAMData <- rename(SPAMData, TARGETPROD = PROD)
GapClose3 <- left_join(GapClose1a, SPAMData) %>%
              mutate(ERROR_close = ERROR_close *(TARGETPROD/PROD),
                     TEYG_close =TEYG_close *(TARGETPROD/PROD),
                     TYG2_close =TYG2_close *(TARGETPROD/PROD),
                     POTPROD = PROD + ERROR_close + TEYG_close + TYG2_close)

# Total increase in yield per year
GapClose3a <- GapClose3 %>%
  #group_by(zone_lsms) %>%
  summarize(PROD = sum(TARGETPROD, na.rm=T)/1000000,
            ERROR_close = sum(ERROR_close, na.rm=T)/1000000,
            TEYG_close = sum(TEYG_close, na.rm=T)/1000000,
            #EYG_close = sum(EYG_close, na.rm=T)/1000000,
            #EUYG_close = sum(EUYG_close, na.rm=T)/1000000,
            TYG2_close = sum(TYG2_close, na.rm=T)/1000000,
            POTPROD = sum(POTPROD, na.rm=T)/1000000)

# http://www.r-bloggers.com/waterfall-plots-in-r/
# Create database for waterfall plot
# Add error, which is very small to target production
wf.df <- GapClose2a %>% 
  dplyr::select(PROD,  TEYG_close, TYG_close, POTPROD)

wf.df <- as.data.frame(t(wf.df)) %>%
  mutate(category =c("Actual production", " Closing \n technical efficiency \n yield gap", 
                     " Closing \n technical \n yield gap", "Potential \n production"),
         sector = category) %>%
  rename(value = V1)

# Create waterfall plot
source("./Code/waterfall_plot.R")
cbPalette <- c("#009E73", "#CC79A7", "#0072B2", "#D55E00", "black", "#999999")
waterfall_f(wf.df)

## Determines the spacing between columns in the waterfall chart
offset <- 0.3

waterfall <- waterfall_f(wf.df, offset=offset) +
  scale_fill_manual(guide="none", values=cbPalette)+
  labs(x="", y="Maize production (million tons)") +
  scale_y_continuous(breaks=seq(0, 30, 5), labels = comma) +
  theme_classic() 


print(waterfall)
library(Cairo)
ggsave(plot = waterfall, ".\\Graphs\\Waterfall.png", height = 150, width = 200, type = "cairo-png", units="mm")


# Distribution of relative yield gaps
db11 <- db9%>%
  dplyr::select(zone_lsms, ERROR_s, TEYG_s, TYG2_s, YG_s_Ycor) %>%
  gather(yieldgap, value, ERROR_s:YG_s_Ycor) %>%
  filter(yieldgap!="ERROR_s") %>% 
  droplevels() %>%
  mutate(value = (1-value)*100,
         yieldgap = factor(yieldgap, levels = c("TEYG_s", "TYG2_s", "YG_s_Ycor")))


# Note that average error=0 and therefore not interesting to show.
# Show plot with minimum hinges and no outliers.
boxplot <-ggplot(data=db11, aes(x=yieldgap, y=value)) +
  #geom_boxplot(fill=c("#D55E00", "#009E73", "#0072B2", "#CC79A7", "#999999"), outlier.colour = NA) +
  geom_boxplot(fill=c("#0072B2", "#CC79A7", "#999999"), outlier.colour = "black") +
  stat_boxplot(geom ='errorbar') +
  guides(fill=FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  theme_classic() +
  #geom_jitter(position=position_jitter(width=.1, height=0)) +
  labs(x="", y="Yield gap (%)")+ 
  scale_x_discrete(breaks=c("TEYG_s", "TYG2_s", "YG_s_Ycor"), 
                   labels=c("Techical efficiency \n yield gap", 
                            "Technical \n yield gap", "Yield gap")) 
#facet_wrap(~zone) +
#scale_y_continuous(breaks=seq(0, 100, 25)) 

# Rescale axes.
#sts <- boxplot.stats(db11$value)$stats  # Compute lower and upper whisker limits
#boxplot = boxplot + coord_cartesian(ylim = c(-5,max(sts)*1.05))
boxplot
ggsave(plot = boxplot, ".\\Graphs\\Distribution.png", height = 150, width = 200, type = "cairo-png", units="mm")

# Distribution of absolute yield gaps
db12 <- db9%>%
  dplyr::select(zone_lsms, ERROR_l, TEYG_l, TYG2_l, YG_l_Ycor) %>%
  gather(yieldgap, value, ERROR_l:YG_l_Ycor) %>%
  filter(yieldgap!="ERROR_l") %>% 
  droplevels() %>%
  mutate(yieldgap = factor(yieldgap, levels = c("TEYG_l", "TYG2_l", "YG_l_Ycor")))

# Note that average error=0 and therefore not interesting to show.
# Show plot with minimum hinges and no outliers.
boxplot2 <-ggplot(data=db12, aes(x=yieldgap, y=value)) +
  geom_boxplot(fill=c("#0072B2", "#CC79A7", "#999999"), outlier.colour = NA) +
  #geom_boxplot(fill=c("#D55E00", "#009E73", "#0072B2", "#CC79A7", "#999999"), outlier.colour = "black") +
  stat_boxplot(geom ='errorbar') +
  guides(fill=FALSE) +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
  theme_classic() +
  labs(x="", y="Yield gap (%)")+ 
  scale_x_discrete(breaks=c("TEYG_l", "TYG2_l", "YG_l_Ycor"), 
                   labels=c("Techical efficiency \n yield gap", 
                            "Technical \n yield gap", "Yield gap"))
#+ scale_y_continuous(breaks=seq(0, 100, 25)) 

# Rescale axes.
boxplot2
ggsave(plot = boxplot2, ".\\Graphs\\Distribution2.png", height = 150, width = 200, type = "cairo-png", units="mm")


# Maps with GYGA yield potential and plot information
# transform shapefile in dataframe for ggplot. rownames are used as ID for the countries. Using ID gives strange results. 
# Additional data is linked back again
library(rgdal)
library(sp)
library(raster)
library(foreign)
library(gdata)
#library(ggthemes)
library(cairo)

# GYGA DATA
GYGApath <- "D:\\Data\\IPOP\\GYGA\\"

dsn=paste(GYGApath, "\\CZ_SubSaharanAfrica\\CZ_AFRGYGACNTRY.shp", sep="")
ogrListLayers(dsn)
ogrInfo(dsn, layer="CZ_AFRGYGACNTRY")
GYGA.Africa<-readOGR(dsn, layer = "CZ_AFRGYGACNTRY")
projection(GYGA.Africa) # check projection
GYGA.Africa <- spTransform(GYGA.Africa, CRS("+proj=longlat +datum=WGS84"))

# Get GYGA
GYGA.country.yield.data <- read.xlsx(paste(GYGApath, "GygaNigeria.xlsx", sep="\\"), sheet=3)

# Select data for maize
GYGA.country.yield.data <- subset(GYGA.country.yield.data, CROP=="Rainfed maize")

# Cut out ETH from GYGA map
GYGA.country <- GYGA.Africa[GYGA.Africa$REG_NAME=="Nigeria",]

# Link yield gap data
# in order to merge data with spatialpolygondataframe the row.names need to be the same.
# For this reason I first merge the additionald data and then create a spatialpolygondataframe
# again ensuring that the rownames are preserved.

GYGA.country.data <- as(GYGA.country, "data.frame")
GYGA.country.data$id <-row.names(GYGA.country.data)
GYGA.country.data <- merge(GYGA.country.data, GYGA.country.yield.data[,c(1:8)], by.x=c("GRIDCODE"), by.y=c("CLIMATEZONE"), all.x=TRUE, sort=FALSE)
row.names(GYGA.country.data)<-GYGA.country.data$id
GYGA.country <- SpatialPolygonsDataFrame(as(GYGA.country, "SpatialPolygons"),
                                         data=GYGA.country.data)

# Maps with GYGA yield potential and plot information
# transform shapefile in dataframe for ggplot. rownames are used as ID for the countries. Using ID gives strange results. 
# Additional data is linked back again
GYGA.country.fort<- fortify(GYGA.country) 
GYGA.country.fort <- merge(GYGA.country.fort, GYGA.country.data, by="id")
GYGA.country.fort$yieldclass <- cut(GYGA.country.fort$YW, breaks=c(6, 8.5, 11, 13.5, 16, 19))
meanYield <- ddply(db9,.(lon, lat), summarize, meanYield = (sum(Y*area_gps)/sum(area_gps))/1000)
meanYield$meanYield2 <- cut(meanYield$meanYield, breaks=c(0, 1, 2, 11))

GYGA_LSMS <- ggplot()+
  geom_polygon(data=GYGA.country.fort, aes(x=long, y=lat, group=group, fill=yieldclass), colour="black")+
  geom_polygon(data=subset(GYGA.country.fort, is.na(YW)), aes(x=long, y=lat, group=group), fill="white", colour="black") +
  scale_fill_discrete(name="Potential water\nlimited yield (tons)") +
  geom_point(data=meanYield, aes(x=lon, y=lat, size=(meanYield2)), colour="black")+
  scale_size_manual(name="Average yield (tons)", values=c(1, 2, 3, 4)) +
  coord_equal()+
  labs(x="", y="")+
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank())

GYGA_LSMS
ggsave(plot = GYGA_LSMS, ".\\Graphs\\GYGA_LSMS.png", height = 150, width = 200, type = "cairo-png", units="mm")

# Zonal map with community yield levels
# read in map of tanzania as a SpatialPolygonsDataFrame
library(raster)
library(maptools)
countryMap <- getData('GADM', country = "NGA", level = 1) 

# Rename zones using LSMS names
countryMap@data$zone_lsms[countryMap@data$NAME_1 %in% c("Benue", "Federal Capital Territory", "Kogi", "Niger", "Kwara", "Nassarawa", "Plateau")] <- "NORTH CENTRAL"
countryMap@data$zone_lsms[countryMap@data$NAME_1 %in% c("Bauchi", "Taraba", "Adamawa", "Borno", "Gombe", "Yobe")] <- "NORTH EAST"
countryMap@data$zone_lsms[countryMap@data$NAME_1 %in% c("Kaduna", "Katsina", "Zamfara", "Jigawa", "Kano", "Kebbi", "Sokoto")] <- "NORTH WEST"
countryMap@data$zone_lsms[countryMap@data$NAME_1 %in% c("Abia", "Ebonyi", "Imo", "Anambra", "Enugu")] <- "SOUTH EAST"
countryMap@data$zone_lsms[countryMap@data$NAME_1 %in% c("Akwa Ibom", "Cross River", "Rivers", "Bayelsa", "Delta", "Edo")] <- "SOUTH SOUTH"
countryMap@data$zone_lsms[countryMap@data$NAME_1 %in% c("Ekiti", "Ondo", "Oyo", "Ogun", "Osun", "Lagos")] <- "SOUTH WEST"
countryMap@data$zone <- factor(countryMap@data$zone)
countryMapData <- countryMap@data

#  fortify spatial data to use with ggplot and join using join functions from dplyr
#    The join is on id, make sure all ids are character vectors
countryMap@data <- rename(countryMap@data, id = ID_1)
countryMap@data$id <- as.character(countryMap@data$id)
tf2 <- left_join(tf, countryMap@data)
# Use ggplot to plot map of Tanzania, specifying the labels and choosing nice colours
#    from the RColorBrewer package
library(RColorBrewer)
#display.brewer.all()

p4 <- ggplot()+
  geom_polygon(data=tf2, aes(x=long, y=lat, group=group, fill=zone_lsms), colour="black")+
  geom_point(data=meanYield, aes(x=lon, y=lat, size=(meanYield2)), colour="black")+
  scale_fill_brewer(name = "Zones", palette = "Set1") +
  scale_size_manual(name="Average yield (tons)", values=c(1.5, 2.5, 3.5, 4,5)) +
  coord_equal()+
  labs(x="", y="")+
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank())
p4 

ggsave(plot = p4, ".\\Graphs\\datamap.png", height = 150, width = 200, type = "cairo-png", units="mm")

#save.image(file=".\\Report\\IMAGINE_GHA\\ETH_ws.RData")


