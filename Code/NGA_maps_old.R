
# Map with target climate zones and average yield
GYGA_LSMS <- ggplot() +
  coord_equal() +
  labs(
    #title = "Yield gaps and average yield in Nigeria (%)",
    #subtitle = "check",
    #caption = "Source: LSMS-ISA and Global Yield Gap Atlas (www.yieldgap.org)",
    x="", y="") +
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank()) +
  geom_path(data = GYGA_df_f, aes(x = long, y = lat, group = group), colour="black") +
  geom_polygon(data = filter(GYGA_df_f, !is.na(YW2), !(CLIMATEZONE %in% CZ_target)), aes(x = long, y = lat, group = group), fill = "grey", colour="black") +
  geom_polygon(data = filter(GYGA_df_f, !is.na(YW2), CLIMATEZONE %in% CZ_target), aes(x = long, y = lat, group = group, fill = CZ_YW), colour="black") +
  scale_fill_discrete(name = "Climate zone\n (pot. water lim. yield)") +
  geom_point(data = yld, aes(x = lon, y = lat, size = av_yld2), colour="black") +
  scale_size_manual(name = "Average yield (ton/ha)", values = c(1, 2, 3, 4)) 

GYGA_LSMS


### ADMINISTRATIVE MAPS
# Zonal map with community yield levels
countryMap <- readRDS(file.path(dataPath, "Other/Spatial/NGA/GADM_2.8_NGA_adm1.rds"))

# Rename zones using LSMS names
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Benue", "Federal Capital Territory", "Kogi", "Niger", "Kwara", "Nassarawa", "Plateau")] <- "North Central"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Bauchi", "Taraba", "Adamawa", "Borno", "Gombe", "Yobe")] <- "North East"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Kaduna", "Katsina", "Zamfara", "Jigawa", "Kano", "Kebbi", "Sokoto")] <- "North West"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Abia", "Ebonyi", "Imo", "Anambra", "Enugu")] <- "South East"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Akwa Ibom", "Cross River", "Rivers", "Bayelsa", "Delta", "Edo")] <- "South South"
countryMap@data$ZONE[countryMap@data$NAME_1 %in% c("Ekiti", "Ondo", "Oyo", "Ogun", "Osun", "Lagos")] <- "South West"
countryMap@data$ZONE <- factor(countryMap@data$ZONE)
countryMapData <- countryMap@data

# Fortify spatial data to use with ggplot and join using join functions from dplyr
# The join is on id, make sure all ids are character vectors
countryMap@data <- rename(countryMap@data, id = ID_1)
countryMap@data$id <- as.character(countryMap@data$id)
tf <- fortify(countryMap)
tf2 <- left_join(tf, countryMap@data)

# Use ggplot to plot map of Nigeria, specifying the labels and choosing nice colours
# from the RColorBrewer package
ADM1 <- ggplot() +
  geom_polygon(data = tf2, aes(x = long, y = lat, group = group, fill = ZONE), colour = "black")+
  geom_point(data = yld, aes(x = lon, y = lat, size = (av_yld2)), colour = "black")+
  scale_fill_brewer(name = "Zones", palette = "Set1") +
  scale_size_manual(name="Average yield (tons)", values=c(1.5, 2.5, 3.5, 4,5)) +
  coord_equal()+
  labs(
    title = "Zones and average yield in Nigeria (%)",
    #subtitle = "check",
    caption = "Source: LSMS-ISA",
    x="", y="") +
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank())
#ADM1 


### GYGA YIELD GAP 
# Draw map
GYGA_YG <- ggplot() +
  geom_polygon(data = filter(GYGA_df_f, !is.na(YG)), aes(x = long, y = lat, group = group, fill = YG), colour = "black")+
  geom_polygon(data = filter(GYGA_df_f, is.na(YG)), aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  #scale_fill_gradientn(colours = terrain.colors(10)) +
  #scale_fill_gradient(low = "light green", high = "dark green") +
  scale_fill_distiller(palette = "Spectral", name = "%") +
  coord_equal() +
  labs(
    title = "Water-limited yield gap in Nigeria (%)",
    #subtitle = "check",
    caption = "Source: Global Yield Gap Atlas (www.yieldgap.org)",
    x="", y="") +
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank())


