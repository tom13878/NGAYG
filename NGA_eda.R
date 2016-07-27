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


#######################################
####### EXPLORATORY ANALYSIS###########
#######################################

# Check relation nitrogen, use, yield and area size.

# Area
hist(dbP$area_gps)
Freq(dbP$area_gps)

# Labour
hist(dbP$harv_lab)
Freq(dbP$harv_lab)

# Fertilizer
hist(dbP$N)
Freq(dbP$N)

hist(dbP$P)
Freq(dbP$P)


PercTable(dbP$AEZ2, dbP$soil05,  margins=c(1,2),  rfrq="110", freq=T) # Very limited number of plots that are irrigated => exclude
PercTable(dbP$dumN, dbP$soil05,  margins=c(1,2),  rfrq="110", freq=T) # Very limited number of plots that are irrigated => exclude
PercTable(dbP$region, dbP$soil05,  margins=c(1,2),  rfrq="110", freq=T) # Very limited number of plots that are irrigated => exclude
PercTable(dbP$zone, dbP$soil05,  margins=c(1,2),  rfrq="110", freq=T) # Very limited number of plots that are irrigated => exclude

# nutrient availability definition: http://webarchive.iiasa.ac.at/Research/LUC/External-World-soil-database/HTML/SoilQuality.html?sb=10

PercTable(dbP$AEZ2, dbP$legume,  margins=c(1,2), rfrq="110", freq=TRUE) 
PercTable(dbP$AEZ2, dbP$herb,  margins=c(1,2), rfrq="110", freq=TRUE) 
PercTable(dbP$AEZ2, dbP$fung,  margins=c(1,2), rfrq="110", freq=TRUE) 
PercTable(dbP$AEZ2, dbP$irrig,  margins=c(1,2), rfrq="110", freq=TRUE) 
PercTable(dbP$AEZ2, dbP$manure,  margins=c(1,2), rfrq="110", freq=TRUE) # only 0.1 use fungicide, drop
PercTable(dbP$phdum2, dbP$dumN,  margins=c(1,2), rfrq="110", freq=TRUE) 
# 
# Freq(dbP$phdum)
# 
# Fertilize use per region
NUse <- dbP %>% group_by(region) %>%
  dplyr::summarize( Numbplots = n(),
                    NumbNUse = sum(N>0, na.rm = T),
                    shNuse = mean(ifelse(NumbNUse >5 & N>0, NumbNUse/Numbplots*100, 0), na.rm = T),
                    avN = mean(N, na.rm = T),
                    avNcon = mean(ifelse(NumbNUse >5 & N>0, N, NA), na.rm = T),
                    HH = length(unique(holder_id)))

NUseAEZ <- dbP %>% group_by(AEZ2) %>%
  dplyr::summarize( Numbplots = n(),
                    NumbNUse = sum(N>0, na.rm=T),
                    shNuse = mean(ifelse(NumbNUse >5 & N>0, NumbNUse/Numbplots*100, 0), na.rm = T),
                    avN = mean(N, na.rm = T),
                    avNcon = mean(ifelse(NumbNUse >5 & N>0, N, NA), na.rm = T),
                    HH = length(unique(holder_id)))

Freq(dbP$soil05)
soilcheck <- filter(dbP, soil05 %in% c(3))
Freq(dbP$fallow)

# # Application of P
# PercTable(dbP$AEZ2, dbP$P!=0,  margins=c(1,2), rfrq="110", freq=TRUE) # Limited users

#N and P are almost always used in a fixed composition because of type of fertilizer used => do not include P because of multicolinearity.
ggplot(data=dbP, aes(x = N, y = P)) +
  geom_point() + theme_bw() + labs(title = 'N - P relationship') +
  stat_smooth() + facet_wrap(~surveyyear)
# 

df <- filter(dbP, yld <18000 & area_gps >0.01 & area_gps <=10)

ggplot(data = filter(dbP, N>0), aes(x = N, y = yld)) +
  geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
  stat_smooth() + facet_wrap(~fs)


ggplot(data = dbP, aes(x = N, y = yld)) +
  geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
  stat_smooth()

ggplot(data=filter(dbP, N>0 & N <400 ), aes(x = N, y = yld)) +
  geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
  stat_smooth() + facet_wrap(~AEZ2)


ggplot(data= dbP, aes(x = yld, y = SOC)) +
  geom_boxplot() + theme_bw() + labs(title = 'yield response to nitrogen') 
+
  facet_wrap(~AEZ2)

table(soil05 ~ phdum)

# ggplot(data= filter(dbP, dumN==1), aes(x = phdum, y = yld)) +
#   geom_point() + theme_bw() + labs(title = 'yield response to nitrogen') +
#   stat_smooth() +  facet_wrap(~AEZ2)
# 
# # Correlation matrix
# library(Hmisc)
# WhichNumerics<- function(df){sapply(df, class) %in% c("integer", "numeric")}
# m <- cor(dbP[, WhichNumerics(dbP)], use="pairwise.complete.obs")
# PlotCorr(m)
# 
# check <- subset(dbP, phdum2 == "1", select=SOC2)
# xtabs(~ AEZ2 + phdum2, data=dbP)
# library(car)
# vif(OLS1)

# Freq(dbP$soil)

# cap yield at 16040 kg/ha, the highest potential yield in TZA
CS2013 <- filter(CS2013, yld <=18071.79) 

# Sample includes very small plots <0.005 ha that bias result upwards and very large >35 ha. 
# As we focus on small scale farmers we restrict area size
CS2013 <- filter(CS2013, area >=0.1 & area <=10)

# restrict attention to plots that use N < 400. There is one outlier of around 700 and some super large ones that are removed.
CS2013 <- filter(CS2013, N < 400)

# Drop unused levels
CS2013 <- droplevels(CS2013)

