#'========================================================================================================================================
#' Project:  CIMMYT
#' Subject:  Script to create figures
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
BasePackages<- c("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
#lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("WDI", "countrycode", "cowplot", "scales")
lapply(AdditionalPackages, library, character.only = TRUE)

### DETERMINE ROOT DIRECTORY
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
db9_harv <- readRDS(file.path(root, "Cache/db9_harv.rds")) %>%
  mutate(source = "harv")
db9_gps <- readRDS(file.path(root, "Cache/db9_gps.rds")) %>%
  mutate(source = "gps")
  
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


### PRICES
# Plot for maize

# Get colours used in ggplot
#show_col(hue_pal()(5))

Prices_maize <- db1 %>%
  ungroup() %>%
  filter(CLIMATEZONE %in% CZ_target) %>%
  dplyr::select(hhid, plotid, CLIMATEZONE, Pc)

Fig_price_maize = ggplot(data = as.data.frame(Prices_maize), aes(x = CLIMATEZONE, y = Pc)) +
  geom_boxplot(outlier.colour = NA, fill = "#F8766D") +
  stat_boxplot(geom ='errorbar') +
  labs(x = "", 
       y = "Price (Naira/kg)",
       title = "Maize price") +
  guides(fill = F) +
  coord_cartesian(ylim=c(0, 85)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # To centre title

#Fig_price_maize

# Plot for nitrogen
Prices_nit <- db1 %>%
  ungroup() %>%
  filter(CLIMATEZONE %in% CZ_target) %>%
  dplyr::select(hhid, plotid, CLIMATEZONE, Pn)

Fig_price_nit = ggplot(data = as.data.frame(Prices_nit), aes(x = CLIMATEZONE, y = Pn)) +
  geom_boxplot(outlier.colour = NA, fill = "#00B0F6") +
  stat_boxplot(geom ='errorbar') +
  labs(x = "", 
       y = "",
       title = "Nitrogen price") +
  guides(fill = F) +
  coord_cartesian(ylim=c(0, 600)) +
  scale_y_continuous(breaks=seq(0, 600, 100)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) # To centre title

#Fig_price_nit

# Combine plots
Fig_price = plot_grid(Fig_price_maize, Fig_price_nit)
#Fig_price


### YIELD LEVEL COMPARISON db9_harv
yield_comp_harv <- db9_harv %>%
  ungroup() %>%
  filter(CLIMATEZONE %in% CZ_target) %>%
  dplyr::select(hhid, plotid, Y, Ycor, TEY, EY, PFY, PY, HFY90, HFY95, HFY100, CLIMATEZONE, surveyyear, area_harv) %>%
  gather(variable, value, -hhid, -plotid, -CLIMATEZONE, -surveyyear, -area_harv) %>%
  mutate(value = value/1000,
          variable = factor(variable, 
                      levels = c("Y", "Ycor", "TEY", "EY", "HFY90", "HFY95", "HFY100", "PFY", "PY"))) %>%
  group_by(CLIMATEZONE, variable) %>%
  summarize(n = n(),
            max = quantile(value, probs = 0.95, na.rm = TRUE),
            min = quantile(value, probs = 0.05, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            se = sd/sqrt(n),  # Calculate standard error of the mean
            mean_UW = mean(value, na.rm = T),
            mean_W = sum(value*area_harv, na.rm = TRUE)/sum(area_harv, na.rm = TRUE)) %>%
  filter(!(variable %in% c("HFY90", "Ycor", "HFY100")))
  
# Figure with yield levels
Fig_YL_harv = ggplot(data = yield_comp_harv, aes(x = variable, y=mean_W)) + 
  geom_bar(stat="identity", colour = "black", aes(fill = variable)) +
  geom_errorbar(aes(ymax = max, ymin = min), width = 0.25) +
  guides(fill = F) +
  facet_wrap(~CLIMATEZONE, scale = "free", ncol = 1) +
  theme_classic() +
  labs(x = "",
       y = "tons/ha",
       title = "Harvested area") +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5))

#Fig_YL_harv

### YIELD LEVEL COMPARISON db9_harv
yield_comp_gps <- db9_gps %>%
  ungroup() %>%
  filter(CLIMATEZONE %in% CZ_target) %>%
  dplyr::select(hhid, plotid, Y, Ycor, TEY, EY, PFY, PY, HFY90, HFY95, HFY100, CLIMATEZONE, surveyyear, area_gps) %>%
  gather(variable, value, -hhid, -plotid, -CLIMATEZONE, -surveyyear, -area_gps) %>%
  mutate(value = value/1000,
         variable = factor(variable, 
                           levels = c("Y", "Ycor", "TEY", "EY", "HFY90", "HFY95", "HFY100", "PFY", "PY"))) %>%
  group_by(CLIMATEZONE, variable) %>%
  summarize(n = n(),
            max = quantile(value, probs = 0.90, na.rm = TRUE),
            min = quantile(value, probs = 0.10, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            se = sd/sqrt(n),  # Calculate standard error of the mean
            mean_UW = mean(value, na.rm = T),
            mean_W = sum(value*area_gps, na.rm = TRUE)/sum(area_gps, na.rm = TRUE)) %>%
  filter(!(variable %in% c("HFY90", "Ycor", "HFY100")))

# Figure with yield levels
Fig_YL_gps = ggplot(data = yield_comp_gps, aes(x = variable, y=mean_W)) + 
  geom_bar(stat="identity", colour = "black", aes(fill = variable)) +
  geom_errorbar(aes(ymax = max, ymin = min), width = 0.25) +
  guides(fill = F) +
  facet_wrap(~CLIMATEZONE, scale = "free", ncol = 1) +
  theme_classic() +
  labs(x = "",
       y = "",
       title = "Plot area") +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5))

#Fig_YL_gps

# Combine plots
Fig_YL = plot_grid(Fig_YL_harv, Fig_YL_gps)

### COMPARE GPS AND HARV ESTIMATES
dbtot <- bind_rows(db9_gps, db9_harv) %>%
  dplyr::select(hhid, plotid, Y, Ycor, TEY, EY, PFY, PY, HFY90, HFY95, HFY100, CLIMATEZONE, surveyyear, source) %>%
  gather(variable, value, -hhid, -plotid, -CLIMATEZONE, -surveyyear, -source) %>%
  spread(source, value) %>%
  filter(variable %in% c("Y", "EY", "TEY", "PFY"),  CLIMATEZONE %in% CZ_target)

Fig_gps_harv_comp <- ggplot() + 
  geom_point(data = dbtot, aes(x = gps, y = harv, colour = CLIMATEZONE)) +
  coord_fixed(ratio = 1) +
  facet_wrap(~variable, scale = "free") +
  geom_abline(intercept = 0, slope =1) +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(face = "bold")) +
  theme(aspect.ratio=1) +
  labs(x = "Plot area based yield levels (kg/ha)", 
       y = "Harvested area based yield levels (kg/ha)",
       colour = "Climate zone") +
  coord_cartesian(ylim=c(0, 15000),xlim=c(0, 15000)) +
  scale_y_continuous(breaks=seq(0, 15000, 2500)) +
  scale_x_continuous(breaks=seq(0, 15000, 2500)) +
  theme(legend.position="bottom",
        legend.box="horizontal") +
  guides(colour = guide_legend(title.position="top", title.hjust = 0.5))

Fig_gps_harv_comp  

### SHARE OF YIELD GAPS 
# GPS
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
    TEYG = TEYG_l/YG_l_Ycor,
    EYG = EYG_l/YG_l_Ycor,
    EUYG = EUYG_l/YG_l_Ycor,
    TYG = TYG_l/YG_l_Ycor,
    YG = (TEYG_l + EYG_l + EUYG_l + TYG_l)/YG_l_Ycor) %>%
  dplyr::select(-TEYG_l:-YG_l_Ycor) %>%
  setNames(c("CL", "TEYg", "EYg", "FYg", "TYg", "Yg")) %>%
  gather(variable, value, -CL) %>%
  filter(variable != "Yg") %>%
  mutate(CL = factor(CL, levels = c("9301", "9401", "9501", "9701", "9801", "9901", "10401", "Total")),
         variable = factor(variable, levels = c("TYg", "FYg", "EYg", "TEYg"))) %>%
  group_by(CL) %>%
  mutate(pos = (cumsum(value) - 0.5*value),
         label = paste0(sprintf("%.0f", value*100), "%"),
         source = "gps")


Fig_YG_gps <- ggplot(Tbl_YG_sh_gps, aes(x = CL, y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  geom_text(aes(y = pos, label = label), size = 3) +
  labs(x = "", y = "", colour = "Yield gap", title = "Harvested area") +
  theme(plot.title = element_text(hjust = 0.5)) # To centre title


# Harv
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
  ungroup() %>%
  mutate(
    TEYG = TEYG_l/YG_l_Ycor,
    EYG = EYG_l/YG_l_Ycor,
    EUYG = EUYG_l/YG_l_Ycor,
    TYG = TYG_l/YG_l_Ycor,
    YG = (TEYG_l + EYG_l + EUYG_l + TYG_l)/YG_l_Ycor) %>%
  dplyr::select(-TEYG_l:-YG_l_Ycor) %>%
  setNames(c("CL", "TEYg", "EYg", "FYg", "TYg", "Yg")) %>%
  gather(variable, value, -CL) %>%
  filter(variable != "Yg") %>%
  mutate(CL = factor(CL, levels = c("9301", "9401", "9501", "9701", "9801", "9901", "10401", "Total")),
         variable = factor(variable, levels = c("TYg", "FYg", "EYg", "TEYg"))) %>%
  group_by(CL) %>%
  mutate(pos = (cumsum(value) - 0.5*value),
         label = paste0(sprintf("%.0f", value*100), "%"),
         source = "harv")


Fig_YG_harv <- ggplot(Tbl_YG_sh_harv, aes(x = CL, y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  geom_text(aes(y = pos, label = label), size = 3) +
  labs(x = "", y = "", title = "Plot area") +
  theme(plot.title = element_text(hjust = 0.5)) # To centre title
  

# Combine data
Tbl_YG_sh <- bind_rows(Tbl_YG_sh_gps, Tbl_YG_sh_harv) %>%
  mutate(source = ifelse(source == "gps", "Plot area", "Harvested area"))

Fig_YG <- ggplot(Tbl_YG_sh, aes(x = CL, y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  geom_text(aes(y = pos, label = label), size = 3) +
  facet_wrap(~source) +
  theme_classic() +
  labs(x = "", y = "", fill = "Yield gap") +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5))


  #Fig_YL_gps
# 
# ggplot(data = yield_comp2, aes(x = ZONE, y = value, colour = variable, shape = variable)) + 
#   geom_line(aes(group = ZONE), colour = "black", linetype = "solid") +
#   geom_point(size = 2.5) +
#   scale_shape_manual(values=seq(0,15)) +
#   theme_bw()

# yield_comp <- db9_harv %>%
#   filter(CLIMATEZONE %in% CZ_target) %>%
#   ungroup() %>%
#   dplyr::select(hhid, plotid, Y, TEY, EY, PFY, CLIMATEZONE, surveyyear) %>%
#   gather(variable, value, -hhid, -plotid, -CLIMATEZONE, -surveyyear) %>%
#   group_by(CLIMATEZONE, variable) %>%
#   summarize(value = mean(value, na.rm = T)) 
# 
# ggplot(yield_comp, aes(x = CLIMATEZONE, y = value, fill = variable)) + 
#   geom_bar(position = "fill",stat = "identity") + 
#   scale_y_continuous(labels = percent_format())

# ### SCATTERPLOT OF YIELD PER CLIMATE ZONE
# df <- db9_harv %>%
#   filter(CLIMATEZONE %in% CZ_target) %>%
#   dplyr::select(hhid, plotid, Y, TEY, EY, PFY, N_harv, CLIMATEZONE, surveyyear) %>%
#   gather(variable, value, - N_harv, -hhid, -plotid, -CLIMATEZONE, -surveyyear) %>%
#   filter(N_harv>0, variable %in% c("Y"))
# 
# ggplot() +
#   geom_point(data = df, aes(x = N_harv, y = value, colour = variable, shape = variable)) +
#   facet_wrap(~ CLIMATEZONE, scales = "free")
# 
# ggplot(data = df, aes(x = N_harv, y = value, colour = variable, shape = variable)) +
#   filter(Y >0) +
#   geom_point() +
#   stat_smooth()
#   

# # SPAM Data 
# SPAMData <- read.csv("Cache/SPAMData_NGA.csv") %>%
#   rename(Y_SPAM = yield, PROD = TargetProduction, ZONE = zone) %>%
#   select(value = Y_SPAM, ZONE) %>%
#   mutate(variable = "SPAM")
# 
# # GYGA Data
# GYGA <- read_excel(file.path(dataPath, "Other/GYGA/GygaRainfedMaizeSubSaharanAfrica.xlsx"), sheet = "Climate zone") %>%
#   filter(COUNTRY == "Nigeria")
# 
# yield_comp2 <- bind_rows(yield_comp2, SPAMData)
# 
# 
# ggplot(data = yield_comp2, aes(x = ZONE, y = value, colour = variable, shape = variable)) + 
#   geom_line(aes(group = ZONE), colour = "black", linetype = "solid") +
#   geom_point(size = 2.5) +
#   scale_shape_manual(values=seq(0,15)) +
#   theme_bw() 
# 
# 
# # CLOSING YIELD GAPS
# 
# # Calculation of potential increase in production when gap is closed on the basis of sample
# GapClose1 <- mutate(db9_harv, PROD = Y * area_harv,
#                     ERROR_close = ERROR_l*area_harv,
#                     TEYG_close = TEYG_l*area_harv,
#                     EYG_close = EYG_l*area_harv,
#                     EUYG_close = EUYG_l*area_harv,
#                     TYG_close = TYG_l*area_harv,
#                     POTPROD = PROD + ERROR_close + TEYG_close + EYG_close + TYG_close + EUYG_close)
# 
# # Total increase in yield per year
# GapClose1a <- GapClose1 %>%
#   #group_by(ZONE) %>%
#   summarize(PROD = sum(PROD, na.rm=T)/1000000,
#             ERROR_close = sum(ERROR_close, na.rm=T)/1000000,
#             TEYG_close = sum(TEYG_close, na.rm=T)/1000000,
#             EYG_close = sum(EYG_close, na.rm=T)/1000000,
#             EUYG_close = sum(EUYG_close, na.rm=T)/1000000,
#             TYG_close = sum(TYG_close, na.rm=T)/1000000,
#             POTPROD = sum(POTPROD, na.rm=T)/1000000)
# 
# # Calculation of potential increase in production when gap is closed on the basis of SPAM and FAO data weighted over two surveyyears.
# # As the yield in the LSMS is much lower than the FAO/SPAM yield and also that of the LSMS we apply the different yield shares as found above to the base yield of SPAM.
# # We use Ycor as base. This means we assume there was an error (e) and this is corrected for.
# 
# SPAMData <- read.csv("Cache/SPAMData_NGA.csv") %>%
#   rename(Y_SPAM = yield, PROD = TargetProduction, ZONE = zone)
# 
# # Closing of yield gaps per zone
# # Note that for some regions, notably those with very low potential yield (Central and Western), closing TEY and EY already results in
# # Closing the gap. To avoid negative closing the EYG, EUYG and TYG are capped.
# # The reason for overshooting can be caused by a variety of factors, including mismeasurement. Most likely is that Nyp is too high for regions
# # With a very low potential. The all over impact is low as the involved regions have very limited maize production.
# 
# 
# GapClose2 <- db9_harv %>% 
#   group_by(ZONE) %>%
#   summarize(
#     TEYG_s = sum(TEYG_s*area_harv)/sum(area_harv),
#     EYG_s = sum(EYG_s*area_harv)/sum(area_harv),
#     #TYG_s = (sum((TYG_s)*area)/sum(area)), # TYG_s based on LSMS yield, not used
#     #YG_s = (sum((YG_s)*area)/sum(area)), # YG_s based on LSMS yield, not used
#     EUYG_s = sum(EUYG_s*area_harv)/sum(area_harv),
#     PY = mean(PY, na.rm=T)) %>% # Average of potential yield from GYGA
#   left_join(SPAMData, .) %>%
#   mutate(
#     TEY_SPAM = Y_SPAM/TEYG_s, # TEY using SPAM yield as reference
#     EY_SPAM = TEY_SPAM/EYG_s, # EY using SPAM yield as reference
#     EY_SPAM = ifelse(PY-EY_SPAM<0, PY, EY_SPAM), # Correct EY if impact of TEYG and EYG results in yield larger than PY.
#     EYG_s_SPAM =  TEY_SPAM/EY_SPAM, # Recalculate EYG_s
#     UY_SPAM = EY_SPAM/EUYG_s, # UY using SPAM yield as reference
#     UY_SPAM = ifelse(PY-UY_SPAM<0, PY, UY_SPAM), # Correct UY if impact of TEYG and EYG results in yield larger than PY.
#     EUYG_s_SPAM =  EY_SPAM/UY_SPAM, # Recalculate UEYG_s
#     TYG_s_SPAM = UY_SPAM/PY, # Recalculate TYG_s 
#     check = TEYG_s*EYG_s_SPAM*EUYG_s_SPAM*TYG_s_SPAM, #check if multiplication of different parts is the same as total
#     YG_s = Y_SPAM/PY, # YG_s using SPAM yield as reference
#     PTEYG = PROD/TEYG_s, # Total production when TEYG is closed
#     PEYG = PTEYG/EYG_s_SPAM, # Total production when EYG is closed
#     PEUYG = PEYG/EUYG_s_SPAM, # Total production when EUYG is closed
#     PTYG = PEUYG/TYG_s_SPAM, # Total production when TYG is closed
#     POTPROD = PROD/YG_s, # Total production when YG is closed
#     TEYG_close = PTEYG - PROD, # Additional production when closing TEYG
#     EYG_close = PEYG - PTEYG, # Additional production when closing EYG
#     EUYG_close = PEUYG - PEYG, # Additional production when closing EUYG
#     TYG_close = POTPROD - PEUYG) %>%
#   mutate(check2 = TEYG_close + EYG_close + EUYG_close + TYG_close+PROD)
# 
# GapClose2a <- GapClose2 %>% 
#   summarize(PROD = sum(PROD/1000000), # in million tons
#             TEYG_close = sum(TEYG_close/1000000),
#             EYG_close = sum(EYG_close/1000000),
#             TYG_close = sum(TYG_close/1000000),
#             EUYG_close = sum(EUYG_close/1000000), 
#             POTPROD = sum(POTPROD/1000000)) %>%
#   mutate(check2 = TEYG_close + EYG_close + EUYG_close + TYG_close+PROD)
# 
# # Table with absolute yield gap information per zone
# # Note that by definition, YG_s computed by weighting individual YG_s values is not the same as multiplication of weighted TEYG_s etc.
# # We therefore calculate YG_s as the product of the weighted components.
# ZonalYieldGap_l <- bind_rows(
#   db9_harv %>% 
#     dplyr::select(Zone = ZONE, ERROR_l, TEYG_l, EYG_l, EUYG_l, TYG_l, YG_l_Ycor, YG_l, area_harv) %>%
#     group_by(Zone) %>%
#     summarize(ERROR_l =(sum((ERROR_l)*area_harv)/sum(area_harv)),
#               TEYG_l = (sum((TEYG_l)*area_harv)/sum(area_harv)),
#               EYG_l = (sum((EYG_l)*area_harv)/sum(area_harv)),
#               EUYG_l = (sum((EUYG_l)*area_harv)/sum(area_harv)),
#               TYG_l = (sum((TYG_l)*area_harv)/sum(area_harv)),
#               YG_l = (sum((YG_l)*area_harv)/sum(area_harv)),
#               YG_l_Ycor = (sum((YG_l_Ycor)*area_harv)/sum(area_harv)),
#               YG_lcheck = (ERROR_l+TEYG_l+EYG_l+EUYG_l+TYG_l)),
#   db9_harv %>% 
#     dplyr::select(Zone = ZONE, ERROR_l, TEYG_l, EYG_l, EUYG_l, TYG_l, YG_l_Ycor, YG_l, area_harv) %>%
#     summarize(Zone = "Total", 
#               ERROR_l =(sum((ERROR_l)*area_harv)/sum(area_harv)),
#               TEYG_l = (sum((TEYG_l)*area_harv)/sum(area_harv)),
#               EYG_l = (sum((EYG_l)*area_harv)/sum(area_harv)),
#               EUYG_l = (sum((EUYG_l)*area_harv)/sum(area_harv)),
#               TYG_l = (sum((TYG_l)*area_harv)/sum(area_harv)),
#               YG_l = (sum((YG_l)*area_harv)/sum(area_harv)),
#               YG_l_Ycor = (sum((YG_l_Ycor)*area_harv)/sum(area_harv)),
#               YG_lcheck = (ERROR_l+TEYG_l+EYG_l+EUYG_l+TYG_l))) %>%
#   dplyr::select(-ERROR_l,-YG_l, -YG_lcheck)
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
# # Table with yield levels
# YieldLevels <- bind_rows(
#   db9_harv %>% 
#     dplyr::select(Zone = ZONE, Y, Ycor, TEY, EY, PFY, PY, area_harv) %>%
#     group_by(Zone) %>%
#     summarize(Y =(sum((Y)*area_harv)/sum(area_harv)),
#               Ycor = (sum((Ycor)*area_harv)/sum(area_harv)),
#               TEY = (sum((TEY)*area_harv)/sum(area_harv)),
#               EY = (sum((EY)*area_harv)/sum(area_harv)),
#               PFY = (sum((PFY)*area_harv)/sum(area_harv)),
#               PY = (sum((PY)*area_harv)/sum(area_harv))
#     ),
#   db9_harv %>% 
#     dplyr::select(Zone = ZONE, Y, Ycor, TEY, EY, PFY, PY, area_harv) %>%
#     summarize(Zone = "Total", 
#               Y =(sum((Y)*area_harv)/sum(area_harv)),
#               Ycor = (sum((Ycor)*area_harv)/sum(area_harv)),
#               TEY = (sum((TEY)*area_harv)/sum(area_harv)),
#               EY = (sum((EY)*area_harv)/sum(area_harv)),
#               PFY = (sum((PFY)*area_harv)/sum(area_harv)),
#               PY = (sum((PY)*area_harv)/sum(area_harv)))) %>%
#   dplyr::select(Zone, Y, Ycor, TEY, EY, PFY, PY)
# 
# # Closing of yield gap assuming decomposition of levels.
# GapClose3 <- SPAMData %>%
#   rename(Zone = ZONE) %>%
#   left_join(ZonalYieldGap_l_sh, .) %>%
#   left_join(.,YieldLevels) %>%
#   filter(Zone != "Total") %>%
#   mutate(POTPROD = PY/Y_SPAM*PROD, # Total production when YG is closed
#          TEYG_close = (POTPROD-PROD)*TEYG/100, # Additional production when closing TEYG
#          EYG_close = (POTPROD-PROD)*EYG/100, # Additional production when closing EYG
#          EUYG_close = (POTPROD-PROD)*EUYG/100, # Additional production when closing EUYG
#          TYG_close = (POTPROD-PROD)*TYG/100, # Additional production when closing TYG
#          check2 = TEYG_close + EYG_close + EUYG_close + TYG_close+PROD)
# 
# GapClose3a <- GapClose3 %>% 
#   summarize(PROD = sum(PROD/1000000), # in million tons
#             TEYG_close = sum(TEYG_close/1000000),
#             EYG_close = sum(EYG_close/1000000),
#             TYG_close = sum(TYG_close/1000000),
#             EUYG_close = sum(EUYG_close/1000000), 
#             POTPROD = sum(POTPROD/1000000)) 
# 
# # http://www.r-bloggers.com/waterfall-plots-in-r/
# # Create database for waterfall plot
# # Add error, which is very small to target production
# wf.df <- GapClose3a %>% 
#   dplyr::select(PROD,  TEYG_close, EYG_close, EUYG_close, TYG_close, POTPROD)
# 
# wf.df <- as.data.frame(t(wf.df)) %>%
#   mutate(category =c("Actual \n production", " Closing \n technical efficiency \n yield gap", " Closing \n economic \n yield gap",
#                      " Closing \n feasible \n yield gap", " Closing \n technical \n yield gap", "Water-limited \n potential production"),
#          sector = category) %>%
#   rename(value = V1)
# 
# # Create waterfall plot
# cbPalette <- c("#009E73", "#CC79A7", "#0072B2", "#D55E00", "black", "#999999")
# waterfall_f(wf.df)
# 
# ## Determines the spacing between columns in the waterfall chart
# offset <- 0.3
# 
# waterfall <- waterfall_f(wf.df, offset=offset) +
#   scale_fill_manual(guide="none", values=cbPalette)+
#   labs(x="", y="Maize production (million tons)") +
#   scale_y_continuous(breaks=seq(0, 70, 5), labels = comma) +
#   theme_classic() 
# 
# 
# print(waterfall)
# library(Cairo)
# ggsave(plot = waterfall, ".\\Graphs\\Waterfall.png", height = 150, width = 200, type = "cairo-png", units="mm")
# 
# # Distribution of relative yield gaps
# db11 <- db9%>%
#   dplyr::select(ZONE, surveyyear, ERROR_s, TEYG_s, EYG_s, EUYG_s, TYG_s, YG_s_Ycor) %>%
#   gather(yieldgap, value, ERROR_s:YG_s_Ycor) %>%
#   filter(yieldgap!="ERROR_s") %>% 
#   droplevels() %>%
#   mutate(value = (1-value)*100,
#          yieldgap = factor(yieldgap, levels = c("TEYG_s", "EYG_s", "EUYG_s", "TYG_s",  "YG_s_Ycor")))
# 
# 
# # Note that average error=0 and therefore not interesting to show.
# # Show plot with minimum hinges and no outliers.
# boxplot <-ggplot(data=db11, aes(x=yieldgap, y=value)) +
#   #geom_boxplot(fill=c("#D55E00", "#009E73", "#0072B2", "#CC79A7", "#999999"), outlier.colour = NA) +
#   geom_boxplot(fill=c("#D55E00", "#009E73", "#0072B2", "#CC79A7", "#999999"), outlier.colour = "black") +
#   stat_boxplot(geom ='errorbar') +
#   guides(fill=FALSE) +
#   stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
#   theme_classic() +
#   #geom_jitter(position=position_jitter(width=.1, height=0)) +
#   labs(x="", y="Yield gap (%)")+ 
#   scale_x_discrete(breaks=c("TEYG_s", "EYG_s", "EUYG_s", "TYG_s", "YG_s_Ycor"), 
#                    labels=c("Techical efficiency \n yield gap", "Economic \n yield gap", 
#                             "Feasible \n yield gap", "Technical \n yield gap",  "Total \n yield gap")) 
# #facet_wrap(~zone) +
# #scale_y_continuous(breaks=seq(0, 100, 25)) 
# 
# # Rescale axes.
# #sts <- boxplot.stats(db11$value)$stats  # Compute lower and upper whisker limits
# #boxplot = boxplot + coord_cartesian(ylim = c(-5,max(sts)*1.05))
# boxplot
# ggsave(plot = boxplot, ".\\FigTab\\Distribution.png", height = 150, width = 200, type = "cairo-png", units="mm")
# 
# # Distribution of absolute yield gaps
# db12 <- db9%>%
#   dplyr::select(ZONE, surveyyear, ERROR_l, TEYG_l, EYG_l, EUYG_l, TYG_l, YG_l_Ycor) %>%
#   gather(yieldgap, value, ERROR_l:YG_l_Ycor) %>%
#   filter(yieldgap!="ERROR_l") %>% 
#   droplevels() %>%
#   mutate(yieldgap = factor(yieldgap, levels = c("TEYG_l", "EYG_l", "TYG_l", "EUYG_l", "YG_l_Ycor")))
# 
# # Note that average error=0 and therefore not interesting to show.
# # Show plot with minimum hinges and no outliers.
# boxplot2 <-ggplot(data=db12, aes(x=yieldgap, y=value)) +
#   geom_boxplot(fill=c("#D55E00", "#009E73", "#0072B2", "#CC79A7", "#999999"), outlier.colour = NA) +
#   #geom_boxplot(fill=c("#D55E00", "#009E73", "#0072B2", "#CC79A7", "#999999"), outlier.colour = "black") +
#   stat_boxplot(geom ='errorbar') +
#   guides(fill=FALSE) +
#   stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
#   theme_classic() +
#   labs(x="", y="Yield gap (%)")+ 
#   scale_x_discrete(breaks=c("TEYG_l", "EYG_l", "TYG_l", "EUYG_l", "YG_l_Ycor"), 
#                    labels=c("Techical efficiency \n yield gap", "Economic \n yield gap", 
#                             "Technical \n yield gap", " Economically \n unexploitable \n yield gap", "Yield gap"))
# #+ scale_y_continuous(breaks=seq(0, 100, 25)) 
# 
# # Rescale axes.
# boxplot2
# ggsave(plot = boxplot2, ".\\FigTab\\Distribution2.png", height = 150, width = 200, type = "cairo-png", units="mm")
# 
# 
# 
# 
# 
# 
