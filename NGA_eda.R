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
library(sjPlot)
library(xda)
library(car)

#######################################
####### EXPLORATORY ANALYSIS###########
#######################################

# summary statistics
stargazer(as.data.frame(dbP), type = "text") # as.data.frame needed because file are tbl format.
numSummary(dbP)
charSummary(dbP)

# Check individual variables.

# Area
sjp.frq(dbP$logarea, 
        type = "dens", 
        normal.curve = TRUE, 
        normal.curve.alpha = .3)
Freq(dbP$area_gps)

# Labour
sjp.frq(dbP$loglab, 
        type = "dens", 
        normal.curve = TRUE, 
        normal.curve.alpha = .3)
        binwidth=50)


# Fertilizer
sjp.frq(dbP$logN, 
        type = "dens", 
        normal.curve = TRUE, 
        normal.curve.alpha = .3)


# Visual relationship between variables
Plot(dbP[c("yld", "N")], "yld")
bivariate(dbP, "yld", "N")
names(dbP)

# Scatter point and smoothed line
ggplot(data = dbP, aes(x = N, y = yld)) + geom_point() + geom_smooth()

# Boxplots
ggplot(data = dbP, aes(x = factor(seed), y = yld)) + geom_boxplot()
ggplot(data = dbP, aes(x = factor(mech), y = yld)) + geom_boxplot()
ggplot(data = dbP, aes(x = factor(antrac), y = yld)) + geom_boxplot()

# Cross tables for dichotomous variables
PercTable(dbP$mech, dbP$antrac,  margins=c(1,2),  rfrq="110", freq=T) # Very limited number of plots that are irrigated => exclude


