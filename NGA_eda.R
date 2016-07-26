#######################################
#### EXPLORATORY ANALYSIS #############
#######################################

library(plyr)
library(dplyr)
library(ggplot2)
library(stargazer)
library(haven)
library(tidyr)
library(xtable)
library(DescTools)

wdPath <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap\\Analysis\\NGA"
setwd(wdPath)

options(scipen=999)

#######################################
############## READ DATA ##############
#######################################

NGA2010 <- readRDS("Data/NGA_data_2010.rds")
NGA2012 <- readRDS("Data/NGA_data_2012.rds")


#######################################
####### EXPLORATORY ANALYSIS###########
#######################################

# Check relation nitrogen, use, yield and area size.

# Area
hist(db0$area_gps)
Freq(db0$area_gps)

# Labour
hist(db0$harv_lab)
Freq(db0$harv_lab)

# Fertilizer
hist(db0$N)
Freq(db0$N)

hist(db0$P)
Freq(db0$P)


PercTable(db0$AEZ2, db0$soil05,  margins=c(1,2),  rfrq="110", freq=T) # Very limited number of plots that are irrigated => exclude
PercTable(db0$dumN, db0$soil05,  margins=c(1,2),  rfrq="110", freq=T) # Very limited number of plots that are irrigated => exclude
PercTable(db0$region, db0$soil05,  margins=c(1,2),  rfrq="110", freq=T) # Very limited number of plots that are irrigated => exclude
PercTable(db0$zone, db0$soil05,  margins=c(1,2),  rfrq="110", freq=T) # Very limited number of plots that are irrigated => exclude

# nutrient availability definition: http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/HTML/SoilQuality.html?sb=10

PercTable(db0$AEZ2, db0$legume,  margins=c(1,2), rfrq="110", freq=TRUE) 
PercTable(db0$AEZ2, db0$herb,  margins=c(1,2), rfrq="110", freq=TRUE) 
PercTable(db0$AEZ2, db0$fung,  margins=c(1,2), rfrq="110", freq=TRUE) 
PercTable(db0$AEZ2, db0$irrig,  margins=c(1,2), rfrq="110", freq=TRUE) 
PercTable(db0$AEZ2, db0$manure,  margins=c(1,2), rfrq="110", freq=TRUE) # only 0.1 use fungicide, drop
PercTable(db0$phdum2, db0$dumN,  margins=c(1,2), rfrq="110", freq=TRUE) 
# 
# Freq(db0$phdum)
# 
# Fertilize use per region
NUse <- db0 %>% group_by(region) %>%
  dplyr::summarize( Numbplots = n(),
                    NumbNUse = sum(N>0, na.rm = T),
                    shNuse = mean(ifelse(NumbNUse >5 & N>0, NumbNUse/Numbplots*100, 0), na.rm = T),
                    avN = mean(N, na.rm = T),
                    avNcon = mean(ifelse(NumbNUse >5 & N>0, N, NA), na.rm = T),
                    HH = length(unique(holder_id)))

NUseAEZ <- db0 %>% group_by(AEZ2) %>%
  dplyr::summarize( Numbplots = n(),
                    NumbNUse = sum(N>0, na.rm=T),
                    shNuse = mean(ifelse(NumbNUse >5 & N>0, NumbNUse/Numbplots*100, 0), na.rm = T),
                    avN = mean(N, na.rm = T),
                    avNcon = mean(ifelse(NumbNUse >5 & N>0, N, NA), na.rm = T),
                    HH = length(unique(holder_id)))

Freq(db0$soil05)
soilcheck <- filter(db0, soil05 %in% c(3))
Freq(db0$fallow)

# # Application of P
# PercTable(db0$AEZ2, db0$P!=0,  margins=c(1,2), rfrq="110", freq=TRUE) # Limited users
# 
# # N and P are almost always used in a fixed composition because of type of fertilizer used => do not include P because of multicolinearity.
# ggplot(data=db0, aes(x = N, y = P)) +
#   geom_point() + theme_bw() + labs(title = 'N - P relationship') +
#   stat_smooth() + facet_wrap(~surveyyear)
# 

df <- filter(db0, yld <18000 & area_gps >0.01 & area_gps <=10)

ggplot(data = filter(db1, N>0), aes(x = N, y = yld)) +
  geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
  stat_smooth() + facet_wrap(~fs)


ggplot(data = df, aes(x = N, y = yld)) +
  geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
  stat_smooth()

ggplot(data=filter(db0, N>0 & N <400 ), aes(x = N, y = yld)) +
  geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
  stat_smooth() + facet_wrap(~AEZ2)


ggplot(data= db0, aes(x = yld, y = SOC)) +
  geom_boxplot() + theme_bw() + labs(title = 'yield response to nitrogen') 
+
  facet_wrap(~AEZ2)

table(soil05 ~ phdum)

# ggplot(data= filter(db0, dumN==1), aes(x = phdum, y = yld)) +
#   geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
#   stat_smooth() +  facet_wrap(~AEZ2)
# 
# # Correlation matrix
# library(Hmisc)
# WhichNumerics<- function(df){sapply(df, class) %in% c("integer", "numeric")}
# m <- cor(db0[, WhichNumerics(db0)], use="pairwise.complete.obs")
# PlotCorr(m)
# 
# check <- subset(db0, phdum2 == "1", select=SOC2)
# xtabs(~ AEZ2 + phdum2, data=db0)
# library(car)
# vif(OLS1)

# Freq(db0$soil)

# cap yield at 16040 kg/ha, the highest potential yield in TZA
CS2013 <- filter(CS2013, yld <=18071.79) 

# Sample includes very small plots <0.005 ha that bias result upwards and very large >35 ha. 
# As we focus on small scale farmers we restrict area size
CS2013 <- filter(CS2013, area >=0.1 & area <=10)

# restrict attention to plots that use N < 400. There is one outlier of around 700 and some super large ones that are removed.
CS2013 <- filter(CS2013, N < 400)

# Drop unused levels
CS2013 <- droplevels(CS2013)

