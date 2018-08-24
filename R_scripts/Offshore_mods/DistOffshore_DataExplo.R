#Phase 2 of seamap data wrangle
#bring in clean data and set it up for analyses, perform analyses

#raw catch info and trawl info before merge
catch <- read.csv('~/Documents/KrackN/Seamap_man/GSMFC_data/Historic_cleaned/seamap_catchdata.csv')
trawl.info <- read.csv('~/Documents/KrackN/Seamap_man/GSMFC_data/Historic_cleaned/seamap_trawlinfo.csv')

#all the data together
seamap <- read.csv('~/Documents/KrackN/Seamap_man/GSMFC_data/Historic_cleaned/seamap_sapidus_consolidation_2018.csv')


# test

#Lets rename things now that we are going to be doing more work besides merging
names(seamap)
seamap <- seamap[,c("STATIONID","Sapidus_Catch","DEPTH_EMAX","TEMPSURF","TEMPMAX","SALSURF","SALMAX","OXYSURF","OXYMAX",
                    "CHLORSURF","CHLORMAX","TURBSURF","TURBMAX","WECOLOR","CRUISEID","MO_DAY_YR","DECSLAT",
                    "DECSLON","DECELAT","DECELON","VESSEL_SPD","DEPTH_SSTA","DEPTH_ESTA","YR","TITLE","SOURCE","GEAR_SIZE",
                    "GEAR_TYPE","MESH_SIZE","MIN_FISH")]
colnames(seamap)[which(names(seamap) == "DEPTH_EMAX")] <- 'CTD_Depth'
colnames(seamap)[which(names(seamap) == "TEMPSURF")] <- 'Temp_S'
colnames(seamap)[which(names(seamap) == "TEMPMAX")] <- 'Temp_B'
colnames(seamap)[which(names(seamap) == "SALSURF")] <- 'Salinity_S'
colnames(seamap)[which(names(seamap) == "SALMAX")] <- 'Salinity_B'
colnames(seamap)[which(names(seamap) == "OXYSURF")] <- 'DO_S'
colnames(seamap)[which(names(seamap) == "OXYMAX")] <- 'DO_B'
colnames(seamap)[which(names(seamap) == "CHLORSURF")] <- 'CHLOR_S'
colnames(seamap)[which(names(seamap) == "CHLORMAX")] <- 'CHLOR_B'
colnames(seamap)[which(names(seamap) == "TURBSURF")] <- 'TURB_S'
colnames(seamap)[which(names(seamap) == "TURBMAX")] <- 'TURB_B'
colnames(seamap)[which(names(seamap) == "DECSLAT")] <- 'Start_Lat'
colnames(seamap)[which(names(seamap) == "DECSLON")] <- 'Start_Long'
colnames(seamap)[which(names(seamap) == "DECELAT")] <- 'End_Lat'
colnames(seamap)[which(names(seamap) == "DECELON")] <- 'End_Long'
colnames(seamap)[which(names(seamap) == "DEPTH_SSTA")] <- 'Start_Depth'
colnames(seamap)[which(names(seamap) == "DEPTH_ESTA")] <- 'End_Depth'
colnames(seamap)[which(names(seamap) == "YR")] <- 'Year'
colnames(seamap)[which(names(seamap) == "MO_DAY_YR")] <- 'Date'
names(seamap)
seamap$Start_Lat <- as.numeric(as.character(seamap$Start_Lat));seamap$Start_Long <- as.numeric(as.character(seamap$Start_Long))
seamap <- seamap[complete.cases(seamap$Start_Long),]
seamap <- seamap[complete.cases(seamap$Start_Lat),]
coordinates(seamap) <- ~Start_Long+Start_Lat


gulf <- readOGR("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/Tagging_calculations/Nos80_gulfcrop.shp") #study area shapefile
crs(gulf)
crs(seamap) <- crs(gulf)

#coordinate references
degree.crs <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84") #Degree coordinate reference syster WGS 1984
meters.crs <- crs("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs")

#spatial distribution
plot(gulf, main = 'C. Sapidus Catch Locations')
points(seamap[which(seamap$Sapidus_Catch > 0),], col = 'blue')
bubble(seamap['Sapidus_Catch'], main = 'C. Sapidus Catch Locations' )

#distributions of environmental variables
seamap <- as.data.frame(seamap)
hist(seamap$Sapidus_Catch)
hist(log(seamap$Sapidus_Catch)) #use poisson distribution


hist(seamap$DO)
plot(seamap$DO,seamap$Sapidus_Catch)
hist(seamap$Salinity)
plot(seamap$Salinity,seamap$Sapidus_Catch)
hist(seamap$Temp)
plot(seamap$Temp,seamap$Sapidus_Catch)
hist(seamap$Start_Depth)
plot(seamap$Start_Depth,seamap$Sapidus_Catch)

summary(seamap$GEAR_TYPE)

#restrict to standard trawl only and get cpue, ST is shrimp trawl maybe use all trawling
#trawl codes
trawl.codes <- c('ST','FT')
#shrimp traw;, fish trawl

seamap <- seamap[which(seamap$GEAR_TYPE %in% trawl.codes),]
#standardize to 30min trawls

seamap$CPUE <- seamap$Sapidus_Catch / (seamap$MIN_FISH/30)

#cpue points per year
plot(seamap$Year, seamap$CPUE)
tapply(seamap$CPUE, seamap$Year, summary)

#subset so no missing NA's
seamap <- seamap[which(!is.na(seamap$CPUE) == TRUE),]

#cpue yearly average
library(data.table) # You'll also need to install this package
DT = data.table(as.data.frame(seamap))# Convert data.frame to data.table
catch_timeline <- DT[,mean(CPUE), by = Year]
catch_sd <- DT[,sd(CPUE), by = Year]

# sort by year
catch_timeline <- catch_timeline[order(Year),] 
catch_timeline <- merge(catch_timeline, catch_sd, by = 'Year')

colnames(catch_timeline)[2] <- "CPUE"
colnames(catch_timeline)[3] <- "sd"

plot(CPUE ~ Year, data = catch_timeline, type = 'l',ylim=range(c(0, catch_timeline$CPUE+catch_timeline$sd)),
     ylab = 'Mean catch / 30min trawl', main = 'Seamap C. sapidus sampling')

points(catch_timeline$Year, catch_timeline$CPUE,ylim=range(c(0, catch_timeline$CPUE+catch_timeline$sd)),
       pch=19, xlab="Year", ylab="Mean CPUE +/- SD",
       main="CPUE and its Overdispersion")
# hack: we draw arrows but with very special "arrowheads"
arrows(catch_timeline$Year, 0, catch_timeline$Year, catch_timeline$CPUE+catch_timeline$sd, length=0.05, angle=90, code=3)



#create relief column
seamap$relief <- abs(seamap$Start_Depth - seamap$End_Depth)


#### Get Distance Offshore column ############
# #Full us shapefile if you want to doublecheck the distances from shore
# us <-  readOGR("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/GIS/nos80k_coastline/nos80k.shp")
# plot(us)
# locator()
# e1 <- extent(-98.14283, -80.25406, 24.60669, 31.01361)
# plot(crop(us,e1))
# project.shapefile <- crop(us,e1)

#either use the gulf crop, or the code above to make sure crabs near the keys have accurate distances from land
project.shapefile <- gulf

crs(project.shapefile)<-crs(degree.crs) # Set CRS 
project.shapefile<-spTransform(project.shapefile, crs(meters.crs)) # Convert CRS to meters based

#remove point over texas, and invalid point
#seamap <-spTransform(seamap, crs(degree.crs)) # transform back to degrees
#seamap <-  as.data.frame(seamap)
seamap <- seamap[which(seamap$Start_Long > -97.63193),]
seamap <- seamap[which(seamap$Start_Long < 0),] #remove coordinate error

coordinates(seamap)<- ~ Start_Long + Start_Lat # converts the file to a spatialPoints object
crs(seamap) <-crs(degree.crs)
seamap <-spTransform(seamap, crs(meters.crs)) # transforms to meters
crs(project.shapefile);crs(seamap)

plot(project.shapefile);points(seamap, col = 'lightblue')




#calculate distance from shore
for(i in 1:nrow(seamap)){
  seamap$distance_offshore[i] <- gDistance(seamap[i,], project.shapefile)
}

#make it km not meters
seamap$distance_offshore <- seamap$distance_offshore/1000
hist(seamap$distance_offshore)

#change it back to degrees
seamap <-spTransform(seamap, crs(degree.crs)) # transforms to meters
plot(gulf); points(seamap, col = 'lightblue')
plot(gulf); points(seamap[which(seamap$Year == 2016),], col = 'lightblue')


seamap <- as.data.frame(seamap)
write.csv(seamap,"L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/historic_polished/seamap_distances_2018.csv")
