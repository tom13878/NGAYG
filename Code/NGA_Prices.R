#######################################
###### ANALYSIS of ETH price data #####
#######################################

# Compare prices with other prices and check if they are realistic!

wdPath <- "D:\\Data\\Github\\NGAYG"
setwd(wdPath)

dataPath <- "C:\\Users\\dijk158\\OneDrive - IIASA\\SurveyData"

library(dplyr)
library(ggplot2)
library(stargazer)
library(haven)
library(tidyr)
library(xtable)
library(DescTools)
library(sandwich)
library(lmtest)
library(assertive)
library(sjmisc)
library(lazyeval)

options(scipen=999)

# winsor code
source("Code/winsor.R")

#######################################
############## LOAD DATA ##############
#######################################

# Load pooled data 
dbP <- readRDS("Cache/Pooled_NGA.rds")

# read in nitrogen conversion file
conv <- read.csv(file.path(dataPath, "Other/Fertilizer/Fert_comp.csv")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% c("UREA", "DAP"))



# Note that we only know on which field fertilizer is used, not if they are maize plots.
# We decide to calculate the average fertilizer price over maize plots (i.e. plots were among others maize is grown) only as it is possible that because of subsidies or other policies 
# the price of the same type of fertilizer (e.g. UREA) can differ between type of crop, even in the same region
# Inflation is not a problem if we use relative prices (pmaize/pfert), which are assumed to have same inflation.

# read in the fertilizer data and combine in one file
# 2010
fert2010_1 <- read_dta(file.path(dataPath, "/NGA/2010/Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta")) %>%
  transmute(hhid, plotid, typ=toupper(as.character(as_factor(s11dq14))), qty=s11dq15, valu=s11dq18) %>%
  do(filter(., complete.cases(.)))
fert2010_2 <- read_dta(file.path(dataPath, "/NGA/2010/Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta")) %>%
  transmute(hhid, plotid, typ=toupper(as.character(as_factor(s11dq25))), qty=s11dq26, valu=s11dq29) %>%
  do(filter(., complete.cases(.)))

# key prepared to add info on ZONE, REGION and DISTRICT
key_2010 <- filter(dbP, surveyyear == 2010) %>%
            select(hhid, ZONE, EAID, REGNAME,  DISNAME,  hhid, plotid, surveyyear) %>%
            unique() %>% 
            do(filter(., complete.cases(.)))

fert2010 <- rbind(fert2010_1, fert2010_2) %>%
  remove_all_labels() %>%
  left_join(key_2010,.) %>%
  do(filter(., complete.cases(.)))

# 2012
fert2012_1 <- read_dta(file.path(dataPath, "/NGA/2012/Post Planting Wave 2/Agriculture/sect11d_plantingw2.dta")) %>%
  transmute(hhid, plotid, typ = toupper(as.character(as_factor(s11dq15))), qty=s11dq16, valu=s11dq19) 
fert2012_2 <- read_dta(file.path(dataPath, "/NGA/2012/Post Planting Wave 2/Agriculture/sect11d_plantingw2.dta")) %>%
  transmute(hhid, plotid, typ=toupper(as.character(as_factor(s11dq27))), qty=s11dq28, valu=s11dq29) 

# key prepared to add info on ZONE, REGION and DISTRICT
key_2012 <- filter(dbP, surveyyear == 2012) %>%
  select(hhid, ZONE, EAID, REGNAME,  DISNAME,  hhid, plotid, surveyyear) %>%
  unique() %>% 
  do(filter(., complete.cases(.)))

fert2012 <- rbind(fert2012_1, fert2012_2) %>%
  remove_all_labels() %>%
  left_join(key_2012,.) %>%
  do(filter(., complete.cases(.)))

# Combine both surveyyears, remove composite manure and other, and rename NPK
fert <- bind_rows(fert2010, fert2012) %>%
  filter(!(typ %in% c("COMPOSITE MANURE", "OTHER (SPECIFY)"))) %>%
  mutate(typ = ifelse(typ == "NPK", "generic NPK (NGA)", typ))

# provide a nitrogen component value for npk and urea
conv <- read.csv(file.path(dataPath,"Other/Fertilizer/Fert_comp.csv")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100)

# join the fertilizer information with the conversion
fert <- left_join(fert, conv) %>%
  mutate(Vfert=valu/qty,
               Qn=qty*n,
               Qp=qty*p,
         price = Vfert/n)

base <- dbP %>% 
  dplyr::select(ZONE, REGNAME, DISNAME, surveyyear) %>%
  unique() %>%
  na.omit

# Values are winsored aggregates are presented for at least 5 values
# market  prices
fertmar <- fert %>%
  filter(price > 0) %>%
  group_by(surveyyear) %>%
  mutate(price = winsor2(price))
  


medianPrice_f <- function(df, level, group, type){
  prices <- df %>% 
    group_by_(.dots = c(group)) %>%
    dplyr::summarize(
      number = sum(!is.na(price)),
      price = median(price, na.rm=T)) %>%
    filter(number>=5) %>%
    mutate(level = level) %>%
    select(-number) 
  #prices <- setNames(prices, c(group, "price", "level")) 
  out <- left_join(base, prices) %>%
    mutate(type = type)
  return(out)
}

fpCountry <- fertmar %>% 
  group_by(surveyyear) %>%
  dplyr::summarize(price = median(price, na.rm=T)) %>%
  mutate(level = "country") 
fpCountry <- left_join(base, fpCountry) %>%
  mutate(type = "Pn")

fpZone <- medianPrice_f(fertmar, "zone", c("surveyyear", "ZONE"), "Pn")
fpRegion <- medianPrice_f(fertmar, "region", c("surveyyear", "ZONE", "REGNAME"), "Pn")
fpDistrict <- medianPrice_f(fertmar, "district", c("surveyyear", "ZONE", "REGNAME", "DISNAME"), "Pn")

fertMarPrice <- bind_rows(fpDistrict, fpRegion, fpZone, fpCountry) %>%
  na.omit %>%
  spread(level, price) %>%
  mutate(regPrice = ifelse(!is.na(district), district, 
                           ifelse(!is.na(region), region,
                                  ifelse(!is.na(zone), zone, country))),
         source = ifelse(!is.na(district), "district", 
                         ifelse(!is.na(region), "region",
                                ifelse(!is.na(zone), "zone", "country"))),
         product = "fertilizer") %>%
  select(-country, -zone, -region, -district)

rm(fpCountry, fpZone, fpRegion, fpDistrict, fertmar)


# Maize prices
maize <- dbP %>% 
  dplyr::select(ZONE, REGNAME, DISNAME, surveyyear, price = crop_price) %>%
  mutate(price = winsor2(price))

fpCountry <- maize %>%
  group_by(surveyyear) %>%
  dplyr::summarize(price = median(price, na.rm=T)) %>%
  mutate(level = "country")
fpCountry <- left_join(base, fpCountry) %>%
  mutate(type = "Pc")

fpZone <- medianPrice_f(maize, "zone", c("surveyyear", "ZONE"), "Pc")
fpRegion <- medianPrice_f(maize, "region", c("surveyyear", "ZONE", "REGNAME"), "Pc")
fpDistrict <- medianPrice_f(maize, "district", c("surveyyear", "ZONE", "REGNAME", "DISNAME"), "Pc")

maizePrice <- bind_rows(fpDistrict, fpRegion, fpZone, fpCountry) %>%
  na.omit %>%
  spread(level, price) %>%
  mutate(regPrice = ifelse(!is.na(district), district, 
                           ifelse(!is.na(region), region,
                                  ifelse(!is.na(zone), zone, country))),
         source = ifelse(!is.na(district), "district", 
                         ifelse(!is.na(region), "region",
                                ifelse(!is.na(zone), "zone", "country"))),
         product = "maize") %>%
  select(-country, -zone, -region, -district)

rm(fpCountry, fpZone, fpRegion, fpDistrict)

# Combine fert price data files 
regPrice <- bind_rows(fertMarPrice, maizePrice) %>% ungroup

# Create price file at plot level.
# Again, we winsor the prices for each type of price and per surveyyear
plotPrice <- select(dbP, hhid, plotid, ZONE, REGNAME, DISNAME, surveyyear, Pn = WPn, Pc = crop_price) %>%
  gather(type, plotPrice, Pn, Pc) %>%
  mutate(plotPrice = ifelse(plotPrice == 0, NA, plotPrice)) %>% # remove one value with zero price
  group_by(type, surveyyear) %>%
  mutate(plotPrice =winsor2(plotPrice)) %>%
  ungroup() %>% unique

# Substitute regional prices when plot level price is not available
price <- left_join(plotPrice, regPrice) %>%
  filter(!(hhid == "310014" & plotid == 1)) %>% # CHECK WHY THIS ONE IS DUPLICATE
  unique() %>% # should not be necessary but never know
  mutate(price = ifelse(is.na(plotPrice), regPrice, plotPrice)) %>%
  select(-source, -regPrice, -plotPrice, -product) %>%
  spread(type, price) %>%
  do(filter(., complete.cases(.))) # CHECK WHY SOME PLOTS DO NOT HAVE A ZONE, ETC


# Plot
ggplot(data = as.data.frame(price), aes(x = factor(surveyyear), y = Pn)) + geom_boxplot() + facet_wrap(~ZONE)
summary(price)

# save data
saveRDS(price, "Cache/NGA_prices.rds")


