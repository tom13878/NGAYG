############################
#### PREPARE NGA PANEL #####
############################

############################################
############## READ THE DATA ###############
############################################

source("D:\\Data\\Github\\NGAYG\\Code\\NGA_2010.r")
source("D:\\Data\\Github\\NGAYG\\Code\\NGA_2012.r")

#######################################
############## PACKAGES ETC ###########
#######################################

wdPath <- "D:\\Data\\Github\\NGAYG"
setwd(wdPath)

library(tidyverse)
library(stargazer)
library(haven)
library(xtable)

options(scipen=999)

#######################################
###### POOLED DATABASE ################
#######################################

# get all name variables that are common to the three waves
good <- Reduce(intersect, list(names(NGA2010), names(NGA2012)))

# select only those names common in both waves
NGA2010_2 <- NGA2010[, good]
NGA2012_2 <- NGA2012[, good]

# new full dataset
dbP <- rbind(NGA2010_2, NGA2012_2)

rm(good, NGA2010, NGA2010_2, NGA2012, NGA2012_2)

# Write file
saveRDS(dbP, "Cache/Pooled_NGA.rds")
