#######################################
############ NIGERIA 2010 #############
#######################################

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/NGA/2010/"
}

library(haven)
library(dplyr)

options(scipen=999)

#######################################
############## SETTINGS ###############
#######################################

iso3c <- "NGA"
surveyyear<-2010


#######################################
############## LOCATION ###############
#######################################

# There is a fix but probably not yet in CRAN version
# https://github.com/hadley/haven/issues/86
loc <-  read_dta(file.path(dataPath, "Post Harvest Wave 1/Agriculture/secta1_harvestw1.dta")) %>%
  transmute(ZONE = toupper(as_factor(zone)), REGIONNAME = toupper(as_factor(state)), REGIONCODE = state, 
            DISCODE = lga, lga, hhid, plotid, rural = sector) %>%
  mutate(rural = ifelse(rural == 2, 1, 0)) 

# lga has duplicate lables, i.e. one label has multiple levels, which is not allowed. This is corrected.
# Create label-number table
lga = attr(loc$lga, 'labels')
name_lga <- names(lga)
link <- data.frame(lga, DISNAME = name_lga)
count <- as.data.frame(table(link$DISNAME))
# Six labels names with multiple labels in lga
loc <- left_join(loc, link) %>%
  mutate(DISNAME = toupper(factor(DISNAME))) %>%
  dplyr::select(-lga)
rm(link, count, lga, name_lga)

#######################################
############### OUTPUT ################
#######################################

oput <- read_dta(file.path(dataPath, "Post Harvest Wave 1/Agriculture/secta3_harvestw1.dta")) %>%
  dplyr::transmute(hhid, plotid, ZONE = toupper(as_factor(zone)), crop=sa3q2, crop_qty_harv=sa3q6a, qty_unit=sa3q6b,
                   harv_area = sa3q5a, harv_area_unit = toupper(as_factor(sa3q5b)),
                   main_buyer=sa3q10, qty_sold=sa3q11a, qty_sold_unit=sa3q11b, qty_sold_naira=sa3q12)

oput$qty_unit <- as.integer(oput$qty_unit)
oput$qty_sold_unit <- as.integer(oput$qty_sold_unit)

# Convert area_harv into ha using conversion factors presented in survey
conv_area <- read.csv(file.path(paste0(dataPath,"/../.."), "Other/plot_size/area_conv_NGA.csv")) %>%
  select(-code, -ZONECODE) %>%
  mutate(unit = toupper(unit))

oput <- left_join(oput, conv_area, by=c("harv_area_unit"="unit", "ZONE" = "ZONE")) %>%
  mutate(harv_area = harv_area*conv) %>%
  dplyr::select(-ZONE, -harv_area_unit, -conv)

# -------------------------------------
# Not clear what is a legume here
# -------------------------------------

legumes <- c("PIGEON PEA", "SOYA BEANS", "LOCUST BEAN")

oput_x <- group_by(oput, hhid, plotid) %>%
  summarise(crop_count=length(crop[!is.na(crop)]),
            legume=ifelse(any(crop %in% legumes), 1, 0))

oput <- left_join(oput, oput_x); rm(oput_x)



# select on maize and remove observations with quantity NA or 0
oput_maize <- oput[oput$crop %in% 1080 & ! is.na(oput$crop_qty_harv) & !oput$crop_qty_harv %in% 0,]

# unit labelled vector does not come through. Make conversion
# factor using information from survey

unit_code <- c(1, 2, 3, 11, 12, 13, 14, 21, 22, 23, 24, 31,
               32, 33, 34, 41, 42, 43, 51, 52, 53, 61,
               62, 63, 71, 72, 73, 74, 81, 82, 83,
               91, 92, 93, 94, 95)
weight <- c(1, 0.001, 1, 20, 50, 100, 120, 15, 30, 50, 75, 10, 25, 40, 75,
            5, 8, 15, 3, 5, 8, 15, 25, 40, 60, 85, 110, 150,
            1500, 2000, 2500, 10, 20, 25, 50, 200)

cnvrt <- data.frame(unit_code, weight)

oput_maize$qty_unit <- as.integer(oput_maize$qty_unit)
oput_maize <- left_join(oput_maize, cnvrt, by=c("qty_unit"="unit_code"))
oput_maize <- dplyr::mutate(oput_maize, qty_kg = crop_qty_harv*weight)

oput_maize <- dplyr::select(oput_maize, hhid, plotid, crop_qty_harv, harv_area, crop_count, legume)

rm(list=c("cnvrt", "legumes", "oput", "unit_code", "weight"))

#######################################
############## CHEMICAL ###############
#######################################

chem <- read_dta(file.path(dataPath, "Post Planting Wave 1/Agriculture/sect11c_plantingw1.dta")) %>%
  dplyr::select(hhid, plotid,
                pest=s11cq1, pest_q=s11cq2a, pest_q_unit=s11cq2b,
                free_pest_q=s11cq7a, free_pest_q_unit=s11cq7b,
                herb=s11cq10, herb_q=s11cq11a, herb_q_unit=s11cq11b,
                free_herb_q=s11cq16a, free_herb_q_unit=s11cq16b)

chem$pest_q_unit <- as_factor(chem$pest_q_unit)
chem$free_pest_q_unit <- as_factor(chem$free_pest_q_unit)
chem$herb_q_unit <- as_factor(chem$herb_q_unit)
chem$free_herb_q_unit <- as_factor(chem$free_herb_q_unit)

chem$pest_q <- ifelse(chem$pest_q_unit %in% c("litre", "kilogram"), chem$pest_q,
                      ifelse(chem$pest_q_unit %in% "gram", chem$pest_q*0.001, NA))
chem$free_pest_q <- ifelse(chem$free_pest_q_unit %in% c("litre", "kilogram"), chem$free_pest_q,
                           ifelse(chem$free_pest_q_unit %in% "gram", chem$free_pest_q*0.001, NA))
chem$herb_q <- ifelse(chem$herb_q_unit %in% c("litre", "kilogram"), chem$herb_q,
                      ifelse(chem$herb_q_unit %in% "gram", chem$herb_q*0.001, NA))
chem$free_herb_q <- ifelse(chem$free_herb_q_unit %in% c("litre", "kilogram"), chem$free_herb_q,
                           ifelse(chem$free_herb_q_unit %in% "gram", chem$free_herb_q*0.001, NA))

chem <- dplyr::select(chem, -pest_q_unit, -free_pest_q_unit, -herb_q_unit, -free_herb_q_unit)

chem[is.na(chem)] <- 0

chem <- transmute(chem, hhid, plotid, pest, herb,
                  pest_q=pest_q + free_pest_q,
                  herb_q=herb_q + free_herb_q)

# COMMERCIAL FERTILIZER
fert1 <- read_dta(file.path(dataPath, "Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta")) %>%
  dplyr::select(hhid, plotid, typ=s11dq14, qty=s11dq15, valu=s11dq18)
fert2 <- read_dta(file.path(dataPath, "Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta")) %>%
  dplyr::select(hhid, plotid, typ=s11dq25, qty=s11dq26, valu=s11dq29)

# FREE OR LEFT OVER FERTILIZER
freeFert <-  read_dta(file.path(dataPath, "Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta")) %>%
  dplyr::select(hhid, plotid, typ=s11dq7, qty=s11dq8)
leftOverFert <- read_dta(file.path(dataPath, "Post Planting Wave 1/Agriculture/sect11d_plantingw1.dta")) %>%
  dplyr::select(hhid, plotid, typ=s11dq3, qty=s11dq4)

# make factor variables into characters for easier joining
fert1$typ <- toupper(as.character(as_factor(fert1$typ)))
fert2$typ <- toupper(as.character(as_factor(fert2$typ)))
freeFert$typ <- toupper(as.character(as_factor(freeFert$typ)))
leftOverFert$typ <- toupper(as.character(as_factor(leftOverFert$typ)))

# to match conversion table make NPK, generic NPK (NGA)
fert1$typ <- gsub("NPK", "generic NPK (NGA)", fert1$typ)
fert2$typ <- gsub("NPK", "generic NPK (NGA)", fert2$typ)
freeFert$typ <- gsub("NPK", "generic NPK (NGA)", freeFert$typ)
leftOverFert$typ <- gsub("NPK", "generic NPK (NGA)", leftOverFert$typ)

# for now set composite manure and other values to NA
bad <- c("composite manure", "other (specify)")
fert1$typ <- ifelse(fert1$typ %in% bad, NA, fert1$typ)
fert2$typ <- ifelse(fert2$typ %in% bad, NA, fert2$typ)
freeFert$typ <- ifelse(freeFert$typ %in% bad, NA, freeFert$typ)
leftOverFert$typ <- ifelse(leftOverFert$typ %in% bad, NA, leftOverFert$typ)

# provide a nitrogen component value for npk and urea 
conv <- read.csv(file.path(paste0(dataPath,"/../.."), "Other/Fertilizer/Fert_comp.csv")) %>%
  transmute(typ=Fert_type2, n=N_share/100, p=P_share/100) %>%
  filter(typ %in% unique(fert1$typ))

fert1 <- left_join(fert1, conv)
fert2 <- left_join(fert2, conv)
freeFert <- left_join(freeFert, conv)
leftOverFert <- left_join(leftOverFert, conv)

fert <- rbind(fert1, fert2)

# make calculations for commercial fertilizer
fert <- mutate(fert,
               Vfert=valu/qty,
               Qn=qty*n,
               Qp=qty*p)

fert$Pn <- fert$Vfert/fert$n

fert <- group_by(fert, hhid, plotid) %>%
  summarise(N=sum(Qn, na.rm=TRUE),
            P=sum(Qp, na.rm=TRUE),
            WPn=sum((Qn/N)*Pn, na.rm=TRUE))

# now add back in the left over or free fert which does not have a price

otherFert <- rbind(freeFert, leftOverFert)

otherFert <- mutate(otherFert,
                    QnO=qty*n,
                    QpO=qty*p)

otherFert <- group_by(otherFert, hhid, plotid) %>%
  summarise(NO=sum(QnO, na.rm=TRUE),
            PO=sum(QpO, na.rm=TRUE))

# join the commercial and other fertilizers on quantity
# no change to price though!

fert <- left_join(fert, otherFert)
fert <- mutate(fert,
               N=N+NO,
               P=P+PO,
               WPn) %>%
  dplyr::select(hhid, plotid, N, P, WPn)

# and join with other chemical variables
chem <- left_join(chem, fert)

rm(list=c("bad", "fert", "fert1", "fert2", "freeFert", "leftOverFert", "otherFert", "conv"))

#######################################
############### AREAS #################
#######################################

# world bank provides a complete set of
# area measurements
areas <- read_dta(file.path(paste0(dataPath,"/../.."), "Other/Plot_size/areas_nga_y1_imputed.dta")) %>%
  dplyr::select(hhid=case_id, plotid=plotnum,
                area_gps=area_gps_mi_50)

areas$area_gps <- ifelse(areas$area_gps %in% 0, NA, areas$area_gps)

areaTotal <- group_by(areas, hhid) %>%
  summarise(area_tot = sum(area_gps, na.rm=TRUE))

areaTotal$area_tot <- ifelse(areaTotal$area_tot %in% 0, NA, areaTotal$area_tot)

# Add farmer area
areas2 <- read_dta(file.path(dataPath, "Post Planting Wave 1/Agriculture/sect11a1_plantingw1.dta")) %>%
  dplyr::transmute(hhid, plotid, ZONE = toupper(as_factor(zone)), area_farmer = s11aq4a, area_farmer_unit = toupper(as_factor(s11aq4b))) %>%
  left_join(., conv_area, by=c("area_farmer_unit"="unit", "ZONE" = "ZONE")) %>%
  mutate(area_farmer = area_farmer*conv) %>%
  dplyr::select(-ZONE, -area_farmer_unit, -conv)

rm(conv_area)
#######################################
############### LABOUR ################
#######################################

# days spent on plot for hired and family labour
# only available for harvest. no planting/weeding
# information for 2010

lab <- read_dta(file.path(dataPath, "Post Harvest Wave 1/Agriculture/secta2_harvestw1.dta")) %>%
  dplyr::select(hhid, plotid, sa2q1a1:sa2q9) %>%
  transmute(hhid, plotid,
            id1=sa2q1a1, lab1=sa2q1a2*sa2q1a3,
            id2=sa2q1b1, lab2=sa2q1b2*sa2q1b3,
            id3=sa2q1c1, lab3=sa2q1c2*sa2q1c3,
            id4=sa2q1d1, lab4=sa2q1d2*sa2q1d3,
            hirM=sa2q2*sa2q3,
            hirF=sa2q5*sa2q6,
            hirC=sa2q8*sa2q9
  )

# make all NA values zero
lab[is.na(lab)] <- 0

# sum all labour across a single plot - all measured in days
lab <- transmute(lab, hhid, plotid,
                 harv_lab=lab1 + lab2 + lab3 + lab4 +
                   hirM + hirF + hirC)

# add a placeholder for plant_lab which is 
# available in the 2012 survey

lab$plant_lab <- NA
lab <- lab[, c("hhid", "plotid", "plant_lab", "harv_lab")]


#######################################
############### Assets ################
#######################################

# -------------------------------------
# Agricultural assets - section A4 post harvest
# only in post harvest questionnaire
# -------------------------------------

implmt <- read_dta(file.path(dataPath, "Post Harvest Wave 1/Agriculture/secta42_harvestw1.dta")) %>%
  dplyr::select(hhid, itemcode=item_cd, qty=item_seq, valu=sa4q4) %>%
  filter(!qty %in% 0, !is.na(qty), !valu %in% 0, !is.na(valu)) %>%
  transmute(hhid, valu=qty*valu) %>%
  group_by(hhid) %>%
  summarise(implmt_value=sum(valu))

# -------------------------------------
# Livestock assets were recorded post
# planting and post harvest
# -------------------------------------

# POST PLANTING

lvstk <- read_dta(file.path(dataPath, "Post Planting Wave 1/Agriculture/sect11i_plantingw1.dta")) %>%
  dplyr::select(hhid, lvstk=item_cd, qty=s11iq2, valu=s11iq3) %>%
  mutate(prc=valu/qty)

# select only the larger animals - codes are
# in the survey but basically cows, pigs sheep and camels
big <- c(101, 102, 103, 104, 105, 106, 107,
         108, 109, 110, 111, 112, 122)

lvstk <- lvstk[lvstk$lvstk %in% big,]

lvstk <- group_by(lvstk, lvstk) %>% mutate(valu_avg=mean(prc, na.rm=TRUE)*qty)
lvstk$valu <- ifelse(is.na(lvstk$valu), lvstk$valu_avg, lvstk$valu)


# calculate per houshold livestock wealth
lvstk <- group_by(lvstk, hhid) %>%
  summarise(lvstk_valu=sum(valu*qty))

# POST HARVEST

lvstk2 <- read_dta(file.path(dataPath, "Post Harvest Wave 1/Agriculture/secta6_harvestw1.dta")) %>%
  dplyr::select(hhid, lvstk=animal_cd, qty=sa6q2, valu=sa6q3) %>%
  filter(!is.na(qty), !qty %in% 0) %>%
  mutate(prc=valu/qty)

# select only the larger animals - codes are
# in the survey but basically cows, pigs sheep and camels
big <- c(101, 102, 103, 104, 105, 106, 107,
         108, 109, 110, 111, 112, 122)

lvstk2 <- lvstk2[lvstk2$lvstk %in% big,]

lvstk2 <- group_by(lvstk2, lvstk) %>% mutate(valu_avg=mean(prc, na.rm=TRUE)*qty)
lvstk2$valu <- ifelse(is.na(lvstk2$valu), lvstk2$valu_avg, lvstk2$valu)

# calculate per houshold livestock wealth
lvstk2 <- group_by(lvstk2, hhid) %>%
  summarise(lvstk2_valu=sum(valu*qty))

rm("big")

#######################################
################ GEO ##################
#######################################

geo <- readRDS(file.path(dataPath, "../../Other/Spatial/NGA/NGA_geo_2010.rds")) 

#######################################
########### SOCIO/ECONOMIC ############
#######################################

# note that there exists a post planting
# and post harvest household questionnaire

# WDswitch
se <- read_dta(file.path(dataPath, "Post Planting Wave 1/Household/sect1_plantingw1.dta")) %>%
  dplyr::select(hhid, indiv, sex=s1q2, status=s1q3, age=s1q4)
se$sex <- as_factor(se$sex)
se$status <- as_factor(se$status)
se <- filter(se, status %in% "head")

# education - no variable for years of schooling
# but we do have level achieved
# WDswitch
ed <- read_dta(file.path(dataPath, "Post Planting Wave 1/Household/sect2_plantingw1.dta")) %>%
  dplyr::select(hhid, indiv, educ=s2q7)
ed$educ <- as_factor(ed$educ)

se <- left_join(se, ed)
se <- dplyr::select(se, -indiv, -status)

# all of the 2012 variables are upper case
levels(se$educ)[levels(se$educ)=="quaranic integrated"] <- "INTEGRATED QUARANIC"
levels(se$sex) <- toupper(levels(se$sex))
levels(se$educ) <- toupper(levels(se$educ))

rm(ed)
#######################################
########### MISCELLANEOUS #############
#######################################

# -------------------------------------
# Intercropping variable has lots of
# options - make summy variables for
# all of them
# -------------------------------------

cropping <- read_dta(file.path(dataPath, "Post Planting Wave 1/Agriculture/sect11f_plantingw1.dta")) %>%
  dplyr::select(hhid, plotid, cropcode, cropin=s11fq2)

# find only maize - crop code 1080
cropping <- filter(cropping, cropcode %in% 1080)

cropping <- dplyr::mutate(cropping,
                          monoCrop=ifelse(cropin %in% 1, 1, 0),
                          inter_crop=ifelse(cropin %in% 2, 1, 0),
                          relayCrop=ifelse(cropin %in% 3, 1, 0),
                          mixCrop=ifelse(cropin %in% 4, 1, 0),
                          alleyCrop=ifelse(cropin %in% 5, 1, 0),
                          stripCrop=ifelse(cropin %in% 4, 1, 0))

cropping <- dplyr::select(cropping, -cropcode, -cropin)

# ------------------------------------
# irrigation variable
# ------------------------------------

irrig <- read_dta(file.path(dataPath, "Post Planting Wave 1/Agriculture/sect11b_plantingw1.dta")) %>%
  dplyr::select(hhid, plotid, irrig=s11bq24)

irrig$irrig <- ifelse(irrig$irrig %in% 1, 1, 0)


#####################ZUZANA#####################################################################################
#remove all
#rm(list = ls(pattern = "*"))
#####################AGRICULTURE QUESTIONNAIRE#############

# ------------------------------------
# land property rights
# ------------------------------------

plotrights <- read_dta(file.path(dataPath, "Post Planting Wave 1/Agriculture/sect11b_plantingw1.dta")) %>%
  dplyr::select(hhid, plotid, plot_purchasval=s11bq5, plot_right_sell=s11bq11, plot_right_coll=s11bq12, plot_rightboth_others=s11bq13,  plot_mktval=s11bq15 )

plotrights$plot_right_sell <- ifelse(plotrights$plot_right_sell %in% 1, 1, 0)
plotrights$plot_right_coll <- ifelse(plotrights$plot_right_coll %in% 1, 1, 0)
plotrights$plot_rightboth_others <- ifelse(plotrights$plot_rightboth_others %in% 1, 1, 0)

#Plot right defined as the possibility either to sell or use land as a collateral by any household member
plotrights$plot_right = plotrights$plot_right_sell +plotrights$plot_right_coll + plotrights$plot_rightboth_others
plotrights$plot_right[plotrights$plot_right >=2] = 1

plotrights <- dplyr::select(plotrights, -plot_purchasval, -plot_right_sell, -plot_right_coll, -plot_rightboth_others)

# ------------------------------------
# purchased seed
# ------------------------------------

seed <- read_dta(file.path(dataPath, "Post Planting Wave 1/Agriculture/sect11e_plantingw1.dta")) %>%
  dplyr::select(hhid, plotid, cropcode = s11eq2, cropname=s11eq1, purchasedseed=s11eq14)

#filter only maize!
seed <- dplyr::filter(seed, cropcode %in% "1080")
seed$purchasedseed <- ifelse(seed$purchasedseed %in% 1, 1, 0)
seed <- dplyr::select(seed,  -cropname)

#get rid of duplicates!
seedsum <- group_by(seed, hhid, plotid, cropcode) %>%
  summarise(purch_seed = max(purchasedseed))
seed = seedsum

rm(list=c( "seedsum"))

# ------------------------------------
# extension - post planting
# ------------------------------------

extension1 <- read_dta(file.path(dataPath, "Post Planting Wave 1/Agriculture/sect11l1_plantingw1.dta")) %>%
  dplyr::select(hhid, ext_topic=topic_cd, ext= s11lq1, ext_source= s11lq2)

extension1$ext <- ifelse(extension1$ext  %in% 1, 1, 0)

#Merge extension topics into categories: 
#agronomic (new seed, pest control, fertilizer, irrigation, composting - codes 1-5)
#economic (marketing/crop sales, access to credit - codes 6,8)
#other (tobacco, forestry, animal, fishery, others - codes 7,9-12)

#extension topic=1 only if ext=1
extension1 <- dplyr::mutate(extension1,
                            ext_topic_agro=ifelse(((ext_topic %in% (1:5)) & (ext %in% 1)), 1, 0),
                            ext_topic_econ=ifelse(((ext_topic %in% 6|ext_topic %in% 8) & (ext %in% 1)), 1, 0) )

extension1 <- dplyr::mutate(extension1,
                            ext_topic_other=ifelse(((ext_topic %in% 7|ext_topic %in% (9:12))& (ext %in% 1)), 1, 0))                         


#Merge extension sources into categories: 
#public extension (government, field school and agri extension course ?, codes 1,7,9)
#private extension (code 2)
#farmer association (code 5)
#community (village, leed farmer, peer farmer code 8,10,11)
#electronic media (code 12)
#other (NGO, fishing extension, paper media codes 4, 6,13,14)

extension1 <- dplyr::mutate(extension1,
                            ext_source_public=ifelse((ext_source %in% 1 |ext_source %in% 7 |ext_source %in% 9), 1, 0),
                            ext_source_private=ifelse(ext_source %in% 2, 1, 0), 
                            ext_source_coop=ifelse(ext_source %in% 5, 1, 0), 
                            ext_source_community=ifelse((ext_source %in% 8 |ext_source %in% 10 |ext_source %in% 11), 1, 0),
                            ext_source_electronic=ifelse(ext_source %in% 12, 1, 0),
                            ext_source_other=ifelse((ext_source %in% (3:4) |ext_source %in% 6 |ext_source %in% 13 |ext_source %in% 14), 1, 0))

extension1$check = extension1$ext_source_public  + extension1$ext_source_private + extension1$ext_source_coop +extension1$ext_source_community + extension1$ext_source_electronic + extension1$ext_source_other

#create variables that specifically look at extension services from public and from electronic sources
extension1 <- dplyr::mutate(extension1,
                            ext_agro_public=ifelse((ext_topic_agro %in% 1 & ext_source_public %in% 1 ), 1, 0), 
                            ext_econ_public=ifelse((ext_topic_econ %in% 1 & ext_source_public %in% 1 ), 1, 0), 
                            ext_agro_electronic=ifelse((ext_topic_agro %in% 1 & ext_source_electronic %in% 1 ), 1, 0), 
                            ext_econ_electronic=ifelse((ext_topic_econ %in% 1 & ext_source_electronic %in% 1 ), 1, 0))

extension1 <- dplyr::select(extension1, -ext_topic, -ext_source,-check)

#check for NA values
findNA=row(extension1)[which(is.na(extension1))]


#sum over various sources and topics
extension1sum <- group_by(extension1, hhid) %>%
  summarise(ext_dummy=max(ext), ext_topic_agro=max(ext_topic_agro), ext_topic_econ=max(ext_topic_econ), ext_topic_other=max(ext_topic_other),
            ext_source_public=max(ext_source_public), ext_source_private=max(ext_source_private), ext_source_coop=max(ext_source_coop),
            ext_source_community=max(ext_source_community), ext_source_electronic=max(ext_source_electronic), ext_source_other=max(ext_source_other),
            ext_agro_public=max(ext_agro_public), ext_econ_public=max(ext_econ_public), ext_agro_electronic=max(ext_agro_electronic), ext_econ_electronic=max(ext_econ_electronic))

findNA=row(extension1sum)[which(is.na(extension1sum))]

#write.csv(extension1sum, "extension1sum.csv")
          
#upload number of visits (second part of extension questionnaire)###############################
extension2 <- read_dta(file.path(dataPath, "Post Planting Wave 1/Agriculture/sect11l2_plantingw1.dta")) %>%
  dplyr::select(hhid, ext_source=source_cd, ext_visits= s11l2q3)

#replace NA observations with zero
extension2$ext_visits[is.na(extension2$ext_visits)] <- 0

#Merge extension sources into categories as before (for some sources, extension visits are not defined, such as electronic) 
#public extension (government, field school and agri extension course ?, codes 1,7,9)
#private extension (code 2)
#farmer association (code 5)
extension2 <- dplyr::mutate(extension2,
                            ext_source_public=ifelse((ext_source %in% 1 |ext_source %in% 7 |ext_source %in% 9), 1, 0),
                            ext_source_private=ifelse(ext_source %in% 2, 1, 0), 
                            ext_source_coop=ifelse(ext_source %in% 5, 1, 0)) 

#create variables that specifically look at number of extension visits from public and private sources
extension2$ext_visits_public = 0
extension2$ext_visits_public[extension2$ext_source_public == 1] <- extension2$ext_visits[extension2$ext_source_public == 1]
extension2$ext_visits_private = 0
extension2$ext_visits_private[extension2$ext_source_private == 1] <- extension2$ext_visits[extension2$ext_source_private == 1]

extension2 <- dplyr::select(extension2,-ext_source, -ext_source_public,-ext_source_private, -ext_source_coop)

#check for NA values
findNA=row(extension2)[which(is.na(extension2))]

#sum over extension visits
extension2sum <- group_by(extension2, hhid) %>%
  summarise(ext_visits_total=sum(ext_visits), ext_visits_public=sum(ext_visits_public), ext_visits_private=sum(ext_visits_private))

#merge both extension files
ext_pp <- full_join(extension2sum, extension1sum) %>% 
  setNames(paste0(names(.), "_pp")) 
names(ext_pp)[names(ext_pp)=="hhid_pp"]<-"hhid"

#write.csv(ext_pp, "ext_pp.csv")

#------------------------------------
# extension - post harvest
# ------------------------------------

#upload extension files from POST-HARVEST
extension1_ph <- read_dta(file.path(dataPath, "Post Harvest Wave 1/Agriculture/secta5a_harvestw1.dta")) %>%
  dplyr::select(hhid, ext_topic=topic_cd, ext= sa5aq1, ext_source= sa5aq2a)

#convert ext to 0-1
extension1_ph$ext <- ifelse(extension1_ph$ext  %in% 1, 1, 0)

#Merge extension topics into categories: 
#agronomic (new seed, pest control, fertilizer, irrigation, composting - codes 1-5)
#economic (marketing/crop sales, access to credit - codes 6,8)
#other (tobacco, forestry, animal, fishery, others - codes 7,9-13)

#CORRECTION 2010: extension topic=1 only if ext=1
extension1_ph <- dplyr::mutate(extension1_ph,
                               ext_topic_agro=ifelse(((ext_topic %in% (1:5)) & (ext %in% 1)), 1, 0),
                               ext_topic_econ=ifelse(((ext_topic %in% 6|ext_topic %in% 8) & (ext %in% 1)), 1, 0) )

extension1_ph <- dplyr::mutate(extension1_ph,
                               ext_topic_other=ifelse(((ext_topic %in% 7|ext_topic %in% (9:13))& (ext %in% 1)), 1, 0))                         


#Merge extension sources into categories: 
#public extension (government, field school and agri extension course ?, codes 1,7,9)
#private extension (code 2)
#farmer association (code 5)
#community (village, leed farmer, peer farmer code 8,10,11)
#electronic media (code 12)
#other (NGO, fishing extension, paper media codes 4, 6,13,14)

extension1_ph <- dplyr::mutate(extension1_ph,
                               ext_source_public=ifelse((ext_source %in% 1 |ext_source %in% 7 |ext_source %in% 9), 1, 0),
                               ext_source_private=ifelse(ext_source %in% 2, 1, 0), 
                               ext_source_coop=ifelse(ext_source %in% 5, 1, 0), 
                               ext_source_community=ifelse((ext_source %in% 8 |ext_source %in% 10 |ext_source %in% 11), 1, 0),
                               ext_source_electronic=ifelse(ext_source %in% 12, 1, 0),
                               ext_source_other=ifelse((ext_source %in% (3:4) |ext_source %in% 6 |ext_source %in% 13 |ext_source %in% 14), 1, 0))

extension1_ph$check = extension1_ph$ext_source_public  + extension1_ph$ext_source_private + extension1_ph$ext_source_coop +extension1_ph$ext_source_community + extension1_ph$ext_source_electronic + extension1_ph$ext_source_other

#create variables that specifically look at extension services from public and from electronic sources
extension1_ph <- dplyr::mutate(extension1_ph,
                               ext_agro_public=ifelse((ext_topic_agro %in% 1 & ext_source_public %in% 1 ), 1, 0), 
                               ext_econ_public=ifelse((ext_topic_econ %in% 1 & ext_source_public %in% 1 ), 1, 0), 
                               ext_agro_electronic=ifelse((ext_topic_agro %in% 1 & ext_source_electronic %in% 1 ), 1, 0), 
                               ext_econ_electronic=ifelse((ext_topic_econ %in% 1 & ext_source_electronic %in% 1 ), 1, 0))

extension1_ph <- dplyr::select(extension1_ph, -ext_topic, -ext_source,-check)

#check for NA values
findNA=row(extension1_ph)[which(is.na(extension1_ph))]

#sum over various sources and topics
extension1sum_ph <- group_by(extension1_ph, hhid) %>%
  summarise(ext_dummy = max(ext), ext_topic_agro=max(ext_topic_agro), ext_topic_econ=max(ext_topic_econ), ext_topic_other=max(ext_topic_other),
            ext_source_public=max(ext_source_public), ext_source_private=max(ext_source_private), ext_source_coop=max(ext_source_coop),
            ext_source_community=max(ext_source_community), ext_source_electronic=max(ext_source_electronic), ext_source_other=max(ext_source_other),
            ext_agro_public=max(ext_agro_public), ext_econ_public=max(ext_econ_public), ext_agro_electronic=max(ext_agro_electronic), ext_econ_electronic=max(ext_econ_electronic))

#upload second part of extension questionnaire - post harveest
extension2_ph <- read_dta(file.path(dataPath, "Post Harvest Wave 1/Agriculture/secta5b_harvestw1.dta")) %>%
  dplyr::select(hhid, ext_source=source_cd, ext_visits= sa5bq3)

#replace NA observations with zero
extension2_ph$ext_visits[is.na(extension2_ph$ext_visits)] <- 0

#Merge extension sources into categories as before (for some sources, extension visits are not defined, such as electronic) 
#public extension (government, field school and agri extension course ?, codes 1,7,9)
#private extension (code 2)
#farmer association (code 5)
extension2_ph <- dplyr::mutate(extension2_ph,
                               ext_source_public=ifelse((ext_source %in% 1 |ext_source %in% 7 |ext_source %in% 9), 1, 0),
                               ext_source_private=ifelse(ext_source %in% 2, 1, 0), 
                               ext_source_coop=ifelse(ext_source %in% 5, 1, 0)) 

#create variables that specifically look at number of extension visits from public and private sources
extension2_ph$ext_visits_public = 0
extension2_ph$ext_visits_public[extension2_ph$ext_source_public == 1] <- extension2_ph$ext_visits[extension2_ph$ext_source_public == 1]
extension2_ph$ext_visits_private = 0
extension2_ph$ext_visits_private[extension2_ph$ext_source_private == 1] <- extension2_ph$ext_visits[extension2_ph$ext_source_private == 1]

extension2_ph <- dplyr::select(extension2_ph,-ext_source, -ext_source_public,-ext_source_private, -ext_source_coop)


#check for NA values
findNA=row(extension2_ph)[which(is.na(extension2_ph))]

#sum over extension visits
extension2sum_ph <- group_by(extension2_ph, hhid) %>%
  summarise(ext_visits_total=sum(ext_visits), ext_visits_public=sum(ext_visits_public), ext_visits_private=sum(ext_visits_private))

#merge both extension files
#Note that there is very low number of extension observations
extension1sum_ph$hhid <- as.numeric(extension1sum_ph$hhid)
extension2sum_ph$hhid <- as.numeric(extension2sum_ph$hhid)
ext_ph <- full_join(extension1sum_ph, extension2sum_ph)%>% 
  setNames(paste0(names(.), "_ph"))
names(ext_ph)[names(ext_ph)=="hhid_ph"]<-"hhid"

#write.csv(ext_ph, "ext_ph.csv")

rm(list=c("extension1", "extension1sum", "extension1_ph", "extension1sum_ph", "extension2", "extension2sum", "extension2_ph", "extension2sum_ph"))


#########HOUSEHOLD QUESTIONNAIRE##############################
# ------------------------------------
# access to mobile phone
# ------------------------------------

#upload extension files from POST-HARVEST
ICT_postharv <- read_dta(file.path(dataPath, "Post Harvest Wave 1/Household/sect5_harvestw1.dta")) %>%
  dplyr::select(hhid,  mobile_access= s5q8, phones_owned= s5q10, internet_access=s5q14, 
                internet_athome=s5q17, internet_freq= s5q25, internet_4marketinfo=s5q26,
                internet_4banking=s5q33)

#convert 1-2 variables to 0-1 (except for frequency - 3 values)
ICT_postharv$mobile_access <- ifelse(ICT_postharv$mobile_access %in% 1, 1, 0)
ICT_postharv$phones_owned <- ifelse(ICT_postharv$phones_owned %in% 1, 1, 0)
ICT_postharv$internet_access <- ifelse(ICT_postharv$internet_access %in% 1, 1, 0)
ICT_postharv$internet_athome <- ifelse(ICT_postharv$internet_athome %in% 1, 1, 0)
ICT_postharv$internet_4marketinfo <- ifelse(ICT_postharv$internet_4marketinfo %in% 1, 1, 0)
ICT_postharv$internet_4banking <- ifelse(ICT_postharv$internet_4banking %in% 1, 1, 0)

#replace NA observations with zero
ICT_postharv$internet_freq[is.na(ICT_postharv$internet_freq)] <- 0

#create dummies for internet frequency use
ICT_postharv <- dplyr::mutate(ICT_postharv,
                              internet_onceaweek=ifelse((internet_freq %in% 2), 1, 0),
                              internet_onceaday=ifelse(internet_freq %in% 1, 1, 0))

ICT_postharv <- dplyr::select(ICT_postharv, -internet_freq)

#check for NA values
findNA=row(ICT_postharv)[which(is.na(ICT_postharv))]

#summarize over individuals - mobile and internet access defined = 1 when at least one individual
#in the household has an access to it
ICTsum_ph <- group_by(ICT_postharv, hhid) %>%
  summarise(mobile_access=max(mobile_access), phones_owned=max(phones_owned),    
            internet_access=max(internet_access),internet_athome=max(internet_athome), 
            internet_4marketinfo=max(internet_4marketinfo), internet_4banking=max(internet_4banking), 
            internet_onceaweek= max(internet_onceaweek), internet_onceaday=max(internet_onceaday) )

#upload information on mobile phone costs from section 8
ICT2_postharv <- read_dta(file.path(dataPath, "Post Harvest Wave 1/Household/sect8_harvestw1.dta")) %>%
  dplyr::select(hhid, mobile_monthlycosts=s8q32)

#merge the two files
ICT_ph <- full_join(ICT2_postharv, ICTsum_ph)%>% 
  setNames(paste0(names(.), "_ph"))
names(ICT_ph)[names(ICT_ph)=="hhid_ph"]<-"hhid"


rm(list=c("ICT_postharv", "ICT2_postharv", "ICTsum_ph" ))

# ------------------------------------
# banking account and credit 
# ------------------------------------

#upload from the post-planting household questionnaire section 4
credit <- read_dta(file.path(dataPath, "Post Planting Wave 1/Household/sect4_plantingw1.dta")) %>%
  dplyr::select(hhid,  bank_account_own=s4q1, bank_account_others4use = s4q3, borrow_dummy=s4q7 )

#convert to 0-1
credit$bank_account_own <- ifelse(credit$bank_account_own %in% 1, 1, 0)
credit$bank_account_others4use <- ifelse(credit$bank_account_others4use %in% 1, 1, 0)
credit$borrow_dummy <- ifelse(credit$borrow_dummy %in% 1, 1, 0)


#check for NA values
findNA=row(credit)[which(is.na(credit))]

#summarize over individuals - at least one individual with bank own bank account, bank account of
#others that can be used and at least one individual that has borrowed money from any institution
creditsum <- group_by(credit, hhid) %>%
  summarise(bank_account_own=max(bank_account_own), bank_account_others4use=max(bank_account_others4use),    
            borrow_dummy=max(borrow_dummy) )

#upload from the post-planting household questionnaire section 6
#note: credit here refers to the operation of the farm, before it refers to general money borrowing
credit2 <- read_dta(file.path(dataPath, "Post Planting Wave 1/Household/sect6_plantingw1.dta")) %>%
  dplyr::select(hhid,  credit_used=s6q15, credit_source = s6q16a, amount_borrowed=s6q17 )

#convert to 0-1
credit2$credit_used <- ifelse(credit2$credit_used  %in% 1, 1, 0)

#replace NA observations in credit source and amount borrowed
credit2$credit_source[is.na(credit2$credit_source)] <- 0
credit2$amount_borrowed[is.na(credit2$amount_borrowed)] <- 0

#select only credit from bank or microfinancing institution
credit2 <- dplyr::mutate(credit2,
                         credit_bank=ifelse((credit_source %in% 1), 1, 0))

credit2 <- dplyr::select(credit2, -credit_source)

#check for NA values
findNA=row(credit2)[which(is.na(credit2))]

#summarize over company entities
creditsum2 <- group_by(credit2, hhid) %>%
  summarise(credit_used=max(credit_used), amount_borrowed=sum(amount_borrowed),    
            credit_bank=max(credit_bank))

#merge credit files
credit_pp <- full_join(creditsum, creditsum2) %>% 
  setNames(paste0(names(.), "_pp"))
names(credit_pp)[names(credit_pp)=="hhid_pp"]<-"hhid"

#replace NA observations with zero
credit_pp[is.na(credit_pp)] <- 0

rm(list=c("credit", "creditsum", "credit2", "creditsum2"))

# ------------------------------------
# credit - postharvest (limited info)
# ------------------------------------

#upload from the post-harvest household questionnaire section 9
#note: credit here refers to the operation of the farm, before it refers to general money borrowing
credit2_ph <- read_dta(file.path(dataPath, "Post Harvest Wave 1/Household/sect9_harvestw1.dta")) %>%
  dplyr::select(hhid,  credit_used=s9q18, credit_source = s9q19a, amount_borrowed=s9q20 )

#convert to 0-1
credit2_ph$credit_used <- ifelse(credit2_ph$credit_used  %in% 1, 1, 0)

#replace NA observations in credit source and amount borrowed
credit2_ph$credit_source[is.na(credit2_ph$credit_source)] <- 0
credit2_ph$amount_borrowed[is.na(credit2_ph$amount_borrowed)] <- 0

#select only credit from bank or microfinancing institution
credit2_ph <- dplyr::mutate(credit2_ph,
                            credit_bank=ifelse((credit_source %in% 1), 1, 0))

credit2_ph <- dplyr::select(credit2_ph, -credit_source)

#check for NA values
findNA=row(credit2_ph)[which(is.na(credit2_ph))]

#summarize over company entities
creditsum2_ph <- group_by(credit2_ph, hhid) %>%
  summarise(credit_used=max(credit_used), amount_borrowed=sum(amount_borrowed),    
            credit_bank=max(credit_bank))

#final step
credit_ph = creditsum2_ph %>% 
  setNames(paste0(names(.), "_ph"))
names(credit_ph)[names(credit_ph)=="hhid_ph"]<-"hhid"


rm(list=c("credit2_ph", "creditsum2_ph"))

#########COMMUNITY QUESTIONNAIRE##############################

# ------------------------------------
# infrastructure - post harvest
# ------------------------------------

#Upload info from post-harvest community questionnaire (note that post-planting is much reduced_)
infrastructure_ph <- read_dta(file.path(dataPath, "Post Harvest Wave 1/Community/sectc2_harvestw1.dta")) %>%
  dplyr::select(EAID=ea, lga, infra_type=is_cd, infra_dummy=sc2q1,infra_distance=sc2q3, infra_cost=sc2q5  )

# lga has duplicate lables, i.e. one label has multiple levels, which is not allowed. This is corrected.
# Create label-number table
lga = attr(infrastructure_ph$lga, 'labels')
name_lga <- names(lga)
link <- data.frame(lga, DISNAME = name_lga)
count <- as.data.frame(table(link$DISNAME))
# Six labels names with multiple labels in lga
infrastructure_ph <- left_join(infrastructure_ph, link) %>%
  mutate(DISNAME = toupper(factor(DISNAME))) %>%  
  dplyr::select(-lga)

rm(link, count, lga, name_lga)

infrastructure_ph$infra_dummy <- ifelse(infrastructure_ph$infra_dummy %in% 1, 1, 0)

infrastructure_ph <- dplyr::mutate(infrastructure_ph,
                                   infra_type_finance=ifelse((infra_type %in% (216:217)), 1, 0),
                                   infra_type_market=ifelse((infra_type %in% 219), 1, 0) )


#CORRECT in 2010: Correct all bellow!!!!!!!! 
infrastructure_ph <- dplyr::mutate(infrastructure_ph,
                                   infra_dummy_finance=ifelse(((infra_type %in% (216:217))&(infra_dummy %in% 1)), 1, 0),
                                   infra_dummy_market=ifelse(((infra_type %in% 219)&(infra_dummy %in% 1 )), 1, 0) )


#create distance variables: not that even if infra dummy = 0, we are interested in the distance as market might be not in the village but outside
infrastructure_ph$dist2market = NA
infrastructure_ph$dist2market[infrastructure_ph$infra_type_market == 1] <- infrastructure_ph$infra_distance[infrastructure_ph$infra_type_market == 1]

infrastructure_ph$cost2market = NA
infrastructure_ph$cost2market[infrastructure_ph$infra_type_market == 1] <- infrastructure_ph$infra_cost[infrastructure_ph$infra_type_market == 1]

#Note: finance means either bank or microfinance institution
infrastructure_ph$dist2finance = NA
infrastructure_ph$dist2finance[infrastructure_ph$infra_type_finance == 1] <- infrastructure_ph$infra_distance[infrastructure_ph$infra_type_finance == 1]

infrastructure_ph <- dplyr::select(infrastructure_ph, -infra_type, -infra_type_market, -infra_type_finance, -infra_dummy,-infra_distance, -infra_cost)

#summarize over infra types, note for distance from finance there are both bank and microfinance
#therefore we take min of the two
infrastructuresum_ph <- group_by(infrastructure_ph, EAID, DISNAME) %>%
  summarise(infra_dummy_finance=max(infra_dummy_finance, na.rm=TRUE),
            infra_dummy_market=max(infra_dummy_market, na.rm=TRUE),
            dist2market=max(dist2market, na.rm=TRUE), 
            cost2market=max(cost2market, na.rm=TRUE), 
            dist2finance=min(dist2finance, na.rm=TRUE))

infra_ph = infrastructuresum_ph %>% 
  setNames(paste0(names(.), "_ph"))

names(infra_ph)[names(infra_ph)=="EAID_ph"]<-"EAID"
names(infra_ph)[names(infra_ph)=="DISNAME_ph"]<-"DISNAME"

rm(list=c("infrastructure_ph", "infrastructuresum_ph"))


# ------------------------------------
# community organizations
# ------------------------------------

#Upload info from post-harvest community questionnaire 
community_ph <- read_dta(file.path(dataPath, "Post Harvest Wave 1/Community/sectc3_harvestw1.dta")) %>%
  dplyr::select(EAID=ea,lga, community_type=group_cd, community_dummy=sc3q1 )

# lga has duplicate lables, i.e. one label has multiple levels, which is not allowed. This is corrected.
# Create label-number table
lga = attr(community_ph$lga, 'labels')
name_lga <- names(lga)
link <- data.frame(lga, DISNAME = name_lga)
count <- as.data.frame(table(link$DISNAME))
# Six labels names with multiple labels in lga
community_ph <- left_join(community_ph, link) %>%
  mutate(DISNAME = toupper(factor(DISNAME))) %>%  
  dplyr::select(-lga)

rm(link, count, lga, name_lga)


community_ph$community_dummy[is.na(community_ph$community_dummy)] <- 0
community_ph$community_dummy <- ifelse(community_ph$community_dummy %in% 1, 1, 0)

community_ph <- dplyr::mutate(community_ph,
                              cummunity_type_agricoop=ifelse((community_type %in% 302 & community_dummy %in% 1) , 1, 0),
                              cummunity_type_creditcoop=ifelse((community_type %in% 303 & community_dummy %in% 1) , 1, 0))

community_ph <- dplyr::select(community_ph, -community_type, -community_dummy)

#summarize over community types
communitysum_ph <- group_by(community_ph, EAID, DISNAME) %>%
  summarise(cummunity_type_agricoop=max(cummunity_type_agricoop), cummunity_type_creditcoop=max(cummunity_type_creditcoop))

#final steps

comm_ph = communitysum_ph %>% 
  setNames(paste0(names(.), "_ph"))

names(comm_ph)[names(comm_ph)=="EAID_ph"]<-"EAID"
names(comm_ph)[names(comm_ph)=="DISNAME_ph"]<-"DISNAME"

rm(list=c("community_ph", "communitysum_ph"))

#########################files to merge#############################
#comm_ph
#infra_ph
#ICT_ph
#credit_ph
#credit_pp
#ext_ph
#ext_pp
#plotrights
#seed



#######################################
########### CROSS SECTION #############
#######################################

# -------------------------------------
# plot level joins - mind that ouput_maize has 1323 and NGA 1326 observations???

NGA2010 <- left_join(oput_maize, chem)
NGA2010 <- left_join(NGA2010, areas)
NGA2010 <- left_join(NGA2010, lab)
NGA2010 <- left_join(NGA2010, cropping)
NGA2010 <- left_join(NGA2010, irrig)
NGA2010 <- left_join(NGA2010, loc)
NGA2010 <- left_join(NGA2010, geo)

# ZUZANA: plot level joins
NGA2010 <- left_join(NGA2010, plotrights)
NGA2010 <- left_join(NGA2010, seed)

# add in placeholder for planting labour in wave2
NGA2010$plant_lab <- NA

# -------------------------------------
# household level joins

NGA2010 <- left_join(NGA2010, areaTotal)
NGA2010 <- left_join(NGA2010, implmt)
NGA2010 <- left_join(NGA2010, lvstk)
NGA2010 <- left_join(NGA2010, lvstk2)
NGA2010 <- left_join(NGA2010, se)

#Zuzana household left joins

NGA2010 <- left_join(NGA2010, ext_pp)
NGA2010 <- left_join(NGA2010, ext_ph)

ICT_ph$hhid = as.numeric(ICT_ph$hhid)
NGA2010 <- left_join(NGA2010, ICT_ph)

credit_ph$hhid = as.numeric(credit_ph$hhid)
NGA2010 <- left_join(NGA2010, credit_ph)

credit_pp$hhid = as.numeric(credit_pp$hhid)
NGA2010 <- left_join(NGA2010, credit_pp)

# -------------------------------------
# Zuzana - community level joins

NGA2010 <- left_join(NGA2010, infra_ph)
NGA2010 <- left_join(NGA2010, comm_ph)

# -------------------------------------
# Make some new variables
# -------------------------------------

# if there is an NA value for any type of
# asset set to zero to calculate total assets per plot

NGA2010$implmt_value <- ifelse(is.na(NGA2010$implmt_value), 0, NGA2010$implmt_value)
NGA2010$lvstk_valu <- ifelse(is.na(NGA2010$lvstk_valu), 0, NGA2010$lvstk_valu)
NGA2010$lvstk2_valu <- ifelse(is.na(NGA2010$lvstk2_valu), 0, NGA2010$lvstk2_valu)

# per hectacre
NGA2010 <- NGA2010 %>%  
            mutate(
              asset= (implmt_value + lvstk2_valu),
              lab = (plant_lab + harv_lab),
              yld=qty/area_gps,
              N=N/area_gps,
              P=P/area_gps,
              lab=lab/area_gps, # Note that plant lab is missing in 2010
              plant_lab = plant_lab/area_gps,
              harv_lab = harv_lab/area_gps,
              asset=asset/area_tot) %>%
            select(-qty)

# -------------------------------------
# Inflate 2010 prices to 2012 prices
# using inflation rate for 2010 and 2011
# from world bank:
# http://data.worldbank.org/indicator/FP.CPI.TOTL.ZG/countries/NG?display=graph
# -------------------------------------

#inflation <- read.csv(file.path(dataPath, "Other/Inflation/inflation.csv"))
inflation <- read.csv(file.path(dataPath, "inflation.csv"))
rate2011 <- inflation$inflation[inflation$code=="NG" & inflation$year==2011]
rate2013 <- inflation$inflation[inflation$code=="NG" & inflation$year==2013]

NGA2010 <- mutate(NGA2010,
                  asset = asset*(1 + rate2011)*(1 + rate2013),
                  # maize_prc = maize_prc*(1 + rate2011)*(1 + rate2013),
                  WPn = WPn*(1 + rate2011)*(1 + rate2013))

# add final variables and rename
hhid <- paste("hhid", surveyyear, sep ="")
eaid <- paste("eaid", surveyyear, sep ="")
plotid <- paste("plotid", surveyyear, sep ="")

NGA2010 <- mutate(NGA2010, surveyyear = surveyyear) %>%
  rename_(.dots = setNames(c("eaid", "hhid", "plotid"), c(eaid, hhid, plotid))) 

write.csv (NGA2010, "NGA2010.csv")

#rm(list=ls()[!ls() %in% "NGA2010"])

# save to file
#saveRDS(NGA2010, file=paste(".\\Data\\", iso3c, "_data_", surveyyear, ".rds", sep=""))
