#Phase 2 of seamap data wrangle
#bring in clean data and set it up for analyses, perform analyses
library(tidyverse)

#raw catch info and trawl info before merge
catch <- read_csv('~/Documents/KrackN/Seamap_man/Historic_cleaned/seamap_catchdata.csv')
trawl.info <- read_csv('~/Documents/KrackN/Seamap_man/Historic_cleaned/seamap_trawlinfo.csv')

#all the data together
seamap <- read_csv('~/Documents/KrackN/Seamap_man/Historic_cleaned/seamap_sapidus_consolidation_2018.csv')


####################  rename things  ####################
names(seamap)
seamap <- seamap %>% rename(CTD_Depth = DEPTH_EMAX,
                  Temp_S = TEMPSURF,
                  Temp_B = TEMPMAX,
                  Salinity_S = SALSURF,
                  Salinity_B = SALMAX,
                  DO_S = OXYSURF,
                  DO_B = OXYMAX,
                  Chlor_S = CHLORSURF,
                  Chlor_B = CHLORMAX,
                  Turb_S = TURBSURF,
                  Turb_B = TURBMAX,
                  Start_Lat = DECSLAT,
                  Start_Long = DECSLON,
                  End_Lat = DECELAT,
                  End_Long = DECELON,
                  Start_Depth = DEPTH_SSTA,
                  End_Depth = DEPTH_ESTA,
                  Survey_Year = YR,
                  Date = MO_DAY_YR)


###################  Restrict to just 40ft trawls  #############
seamap <- seamap %>% filter(GEAR_TYPE %in% c("ST","FT"),
                            GEAR_SIZE %in% c(20,40)) #20 and 40ft.

seamap$Start_Lat <- as.numeric(as.character(seamap$Start_Lat)); seamap$Start_Long <- as.numeric(as.character(seamap$Start_Long))
seamap <- seamap[complete.cases(seamap$Start_Long),]
seamap <- seamap[complete.cases(seamap$Start_Lat),]

#coordinate references
degree.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84") #Degree coordinate reference syster WGS 1984
meters.crs <- crs("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs")

###################  ggplot map polygons  #############
map.world_polygon <- map_data("world")

#positive catches in 40ft trawls
seamap %>% mutate(Crab_Caught = ifelse(Sapidus_Catch > 0, T, F),
                  Crab_Caught = factor(Crab_Caught, levels = c(T,F))) %>%
  filter(Crab_Caught == T) %>% 
  ggplot(aes(x = Start_Long, y = Start_Lat)) +
  coord_quickmap() +  # Define aspect ratio of the map, so it doesn't get stretched when resizing
  coord_cartesian(xlim = c(-98.08, -79.95),ylim = c(24.8, 31.2)) +
  theme_classic() +  # Remove ugly grey background
  theme(legend.position = "bottom") +  # Position the legend at the top of the plot
  geom_polygon(data = map.world_polygon, aes(x = long, y = lat, group = group)) +
  geom_point(aes(color = Crab_Caught), alpha = .35) +
  xlab("Longitude") +
  ylab("Latitude") 



#################  Get Catch per hectare, 1kt = 1.852km/h  ############
seamap <- seamap %>%  filter(MIN_FISH < 60) %>%     #remove any super long trawls
  mutate(Spd_kmh = VESSEL_SPD * 1.852,              #convert knots to km/h
         Area_km = ifelse(GEAR_SIZE == 40, ((MIN_FISH/60) * Spd_kmh) * .012192, ((MIN_FISH/60) * Spd_kmh) * 0.006096), #12.192m = 40ft
         Area_Hectares = Area_km * 100,             #1km sq = 100 hectares
         CPUE_towspd = Sapidus_Catch/Area_Hectares) #Crabs/hectare based on two speed and minutes fished

#cpue points per year
plot(seamap$Survey_Year, seamap$CPUE_towspd)

#subset so no NA's
seamap <- seamap[which(!is.na(seamap$CPUE_towspd) == TRUE),]



#CPUE_towspd yearly average
library(data.table) # You'll also need to install this package
DT <- data.table(as.data.frame(seamap))# Convert data.frame to data.table
catch_timeline <- DT[,mean(CPUE_towspd, na.rm = T), by = Survey_Year]
catch_sd <- DT[,sd(CPUE_towspd, na.rm = T), by = Survey_Year]

# sort by year
catch_timeline <- catch_timeline[order(Survey_Year),] 
catch_timeline <- merge(catch_timeline, catch_sd, by = 'Survey_Year')

colnames(catch_timeline)[2] <- "CPUE"
colnames(catch_timeline)[3] <- "sd"

plot(CPUE ~ Survey_Year, data = catch_timeline, type = 'l',ylim=range(c(0, 10)),
     ylab = 'Mean catch / Hectare', main = 'Seamap C. sapidus sampling')

points(catch_timeline$Survey_Year, catch_timeline$CPUE,
       pch=19, xlab="Year", ylab="Mean CPUE +/- SD",
       main="CPUE and its Overdispersion")
# hack: we draw arrows but with very special "arrowheads"
arrows(catch_timeline$Survey_Year, 0, catch_timeline$Survey_Year, catch_timeline$CPUE+catch_timeline$sd, length=0.05, angle=90, code=3)


#remove point over texas, and invalid point
seamap <- seamap[which(seamap$Start_Long > -97.63193),]
seamap <- seamap[which(seamap$Start_Long < 0),] #remove coordinate error


#######################  calculate distance from shore  ##########################
# #Full us shapefile if you want to doublecheck the distances from shore
# #us <-  readOGR("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/GIS/nos80k_coastline/nos80k.shp")
# coordinates(seamap)<- ~ Start_Long + Start_Lat # converts the file to a spatialPoints object
# crs(seamap) <-crs(degree.crs)
# seamap <-spTransform(seamap, crs(meters.crs)) # transforms to meters
# crs(project.shapefile);crs(seamap)
# 
# seamap$distance_offshore <- rep(NA, nrow(seamap))
# for(i in 1:nrow(seamap)){
#   seamap$distance_offshore[i] <- gDistance(seamap[i,], project.shapefile)
# }
# 
# #make it km not meters
# seamap$distance_offshore <- seamap$distance_offshore/1000
# hist(seamap$distance_offshore)
# 
# #change it back to degrees
# seamap <-spTransform(seamap, crs(degree.crs)) # transforms to meters
# plot(gulf); points(seamap, col = 'lightblue')
# plot(gulf); points(seamap[which(seamap$Year == 2016),], col = 'lightblue')



# Make Hex_Bin Plot
seamap %>%
  filter(Sapidus_Catch > 0) %>%
  ggplot(aes(x = Start_Long, y = Start_Lat)) +
  #coord_quickmap() +  # Define aspect ratio of the map, so it doesn't get stretched when resizing
  coord_cartesian(xlim = c(-98.08, -79.95),ylim = c(24.8, 31.2)) +
  geom_polygon(data = map.world_polygon, aes(x = long, y = lat, group = group)) +
  geom_hex(bins = 600, alpha = 0.75) +
  scale_fill_distiller(palette = "Spectral") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(legend.position = "bottom") + 
  ggtitle("Frequency that Blue Crabs were Caught at a station From 1982-2017")


#make 2d heatmap instead
seamap %>%
  filter(Sapidus_Catch > 0) %>%
  ggplot(aes(x = Start_Long, y = Start_Lat)) +
  coord_cartesian(xlim = c(-98.08, -79.95),ylim = c(24.8, 31.2)) +
  geom_polygon(data = map.world_polygon, aes(x = long, y = lat, group = group)) +
  geom_bin2d(bins = 600, alpha = 0.75) +
  scale_fill_distiller(palette = "Spectral") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_classic() +
  theme(legend.position = "bottom") + 
  ggtitle("Frequency that Blue Crabs were Caught at a station From 1982-2017")

#timeline
filter(seamap, Survey_Year > 1983) %>% group_by(Survey_Year) %>% 
  summarise(CPUE = mean(CPUE_towspd, na.rm = T),
            sd = sd(CPUE_towspd, na.rm = T)) %>% 
  ggplot(aes(Survey_Year, CPUE)) +
  geom_point() + 
  geom_errorbar(aes(x = Survey_Year, ymin = 0, ymax = CPUE + sd))

seamap <- as.data.frame(seamap)
write_csv(seamap,"~/Documents/KrackN/Seamap_man/Historic_cleaned/seamap_cpue_2018.csv")
