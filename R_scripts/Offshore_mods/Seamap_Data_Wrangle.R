#SEAMAP DATA WRANGLING
#SEAMAP offshore crab populations
setwd("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017")
library(maps)
library(sp)
library(dplyr)
library(raster)
#library(mapdata)
#library(maptools)
library(mapproj)
library(ggplot2)
#library(gpclib)
library(gstat)
library(gdistance)
library(rgdal)
library(rgeos)

BGSREC <- read.csv("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/GSMFC_data/public_seamap_csvs/BGSREC.csv")
ENVREC <- read.csv("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/GSMFC_data/public_seamap_csvs/ENVREC.csv")
CRUISES <- read.csv("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/GSMFC_data/public_seamap_csvs/CRUISES.csv") 
STAREC <- read.csv("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/GSMFC_data/public_seamap_csvs/STAREC.csv",
                   quote = "")
INVREC <- read.csv("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/GSMFC_data/public_seamap_csvs/INVREC.csv")
GLFREC <- read.csv("L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/GSMFC_data/public_seamap_csvs/GLFREC.csv")

#GLFREC has SEX of catch
names(GLFREC)
GLFREC <- GLFREC[GLFREC$SPEC_GLF == "SAPIDU",]
nrow(GLFREC[GLFREC$SEX_GLF == "M",])
nrow(GLFREC[GLFREC$SEX_GLF == "F",])
table(GLFREC$SEX_GLF)

#Cruise/Survey Info
names(CRUISES) #cruise ID, Year, Survey Title, Data collector
CRUISES <- CRUISES[,c("CRUISEID","YR","TITLE","SOURCE")]
tail(CRUISES)

#Trawl info
names(STAREC) #Station ID, Cruise ID, MM/DD/YY, Start Lat, Start Lon, End Lat, End Lon, Vessel Speed, start depth, end depth
STAREC <- STAREC[,c("X.STATIONID.","X.CRUISEID.","X.MO_DAY_YR.","X.DECSLAT.","X.DECSLON.","X.DECELAT.",
                    "X.DECELON.","X.VESSEL_SPD.","X.DEPTH_SSTA.","X.DEPTH_ESTA.")]
colnames(STAREC)[1] <- "STATIONID" 
colnames(STAREC)[2] <- "CRUISEID" 
colnames(STAREC)[3] <- "MO_DAY_YR" 
colnames(STAREC)[4] <- "DECSLAT" 
colnames(STAREC)[5] <- "DECSLON" 
colnames(STAREC)[6] <- "DECELAT" 
colnames(STAREC)[7] <- "DECELON" 
colnames(STAREC)[8] <- "VESSEL_SPD" 
colnames(STAREC)[9] <- "DEPTH_SSTA" 
colnames(STAREC)[10] <- "DEPTH_ESTA" 
tail(STAREC)

#Tow Time
names(INVREC)
INVREC <- INVREC[,c("STATIONID","GEAR_SIZE","GEAR_TYPE","MESH_SIZE","MIN_FISH")]
tail(INVREC)


#JOIN
CRUISES$CRUISEID <- as.factor(CRUISES$CRUISEID)
trawl.info <- left_join(STAREC, CRUISES, by = "CRUISEID")
#test merge
tail(trawl.info)
summary(trawl.info$YR)
STAREC[which(STAREC$STATIONID == 179909),]
CRUISES[which(CRUISES$CRUISEID == 993),]



INVREC$STATIONID <- as.factor(INVREC$STATIONID)
trawl.info <- left_join(trawl.info, INVREC, by = "STATIONID")
tail(trawl.info)
length(unique(trawl.info$STATIONID))
#View(trawl.info)


#catch data
names(BGSREC) #Station ID, Cruise ID, Genus, Species, Count
BGSREC <- BGSREC[,c("STATIONID","GENUS_BGS","SPEC_BGS","CNT")]
table(BGSREC$GENUS_BGS)

#subset callinectes to check counts
callin <- BGSREC[BGSREC$GENUS_BGS == "CALLINE",]
table(factor(callin$SPEC_BGS))

#checking all similar genus names to callinectes, just replace CALLINE to check other genuses
table(factor(BGSREC[BGSREC$GENUS_BGS == "CALLINE",]$SPEC_BGS))

 


#create a count column that maintains zero counts for sapidus
BGSREC$SAP_CNT <- rep(0,nrow(BGSREC))
for (i in 1:nrow(BGSREC)) {
  if (BGSREC$SPEC_BGS[i] == "SAPIDU"){
    BGSREC$SAP_CNT[i] <- BGSREC$CNT[i]
  }
  
}
tail(BGSREC)

names(ENVREC) #Cruise ID, Station ID, Lat, Long, BotDepth, BotTemp, BotSal, BotOx, Botchlor 
ENVREC <- ENVREC[,c("STATIONID","DEPTH_EMAX","TEMPSURF","TEMPMAX","SALSURF","SALMAX","OXYSURF","OXYMAX",
                    "CHLORSURF","CHLORMAX","TURBSURF","TURBMAX","WECOLOR")]
tail(ENVREC)


seamap_merge <-  left_join(ENVREC,BGSREC, by = "STATIONID")


#View(seamap_merge)
tail(seamap_merge)




#getting one row for each station, selecting the sapidus rows for stations that have a catch
catch.fix <- unique(data.frame("STATIONID" = seamap_merge$STATIONID, "COUNT" = seamap_merge$SAP_CNT))
catch.fix$ROW <- c(1:nrow(catch.fix)) 
catch.fix$SPEC_BGS <- c(1:nrow(catch.fix))
catch.fix$SPEC_BGS <- ifelse(catch.fix$COUNT > 0, catch.fix$SPEC_BGS <- 'SAPIDU', catch.fix$SPEC_BGS <- 'no_sap')
'%not in%' <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) #function for not in
cal <- catch.fix[which(catch.fix$SPEC_BGS == 'SAPIDU'),] #tag sapidus rows


#these two are the rows to keep
other.stations <- catch.fix[which(catch.fix$STATIONID %not in% cal$STATIONID == TRUE),] #stations with no sapidus
row.fix <- catch.fix[which(catch.fix$ROW %in% cal$ROW == TRUE),] #rows that have sapidus catches
#View(row.fix)
#View(other.stations)

keep <- nrow(other.stations) + nrow(row.fix)


#the rows being droppped
discard <- nrow(catch.fix[which(catch.fix$STATIONID %in% cal$STATIONID == TRUE & catch.fix$SPEC_BGS != 'SAPIDU'),])#rows that share station id but are other species
#check that the amount of rows added and discarded equal original amount
keep+discard
nrow(catch.fix) 



#combine rows of cal and other stations, remove rows that dont match stationid
station_id <- c(other.stations$STATIONID,row.fix$STATIONID)
catch <- c(other.stations$COUNT,row.fix$COUNT)
station.catch <- data.frame("STATIONID" = station_id, "Sapidus_Catch" = catch)
summary(station.catch$Sapidus_Catch)

names(seamap_merge)
seamap_merge <- seamap_merge[,c(1:13)] #dropping "GENUS_BGS","SPEC_BGS","CNT","SAP_CNT" and joining with station specific blue crab catch
seamap_merge <- merge(station.catch, unique(seamap_merge), by = 'STATIONID')
tail(seamap_merge)

seamap_merge$STATIONID <- as.factor(seamap_merge$STATIONID); trawl.info$STATIONID <- as.factor(trawl.info$STATIONID)
all.info <- left_join(seamap_merge, trawl.info, by = "STATIONID")
tail(all.info)



#save to computer to not need flash drive
write.csv(trawl.info,'L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/historic_polished/seamap_trawlinfo.csv' )
write.csv(seamap_merge, 'L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/historic_polished/seamap_catchdata.csv')
write.csv(all.info, 'L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/historic_polished/seamap_sapidus_consolidation_2018.csv')

#---------------------------------------SEAMAP data upload and management----------------------------------
rm(list=ls())

#raw catch info and trawl info before merge
catch <- read.csv('L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/historic_polished/seamap_catchdata.csv')
trawl.info <- read.csv('L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/historic_polished/seamap_trawlinfo.csv')

seamap <- read.csv('L:/Dropbox (The Craboratory)/The Craboratory/Kemberling/SEAMAP_2017/historic_polished/seamap_sapidus_consolidation_2018.csv')


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


################ playing around with it ############



################################################## adding mean CW for blue crabs, unfinished, possibly unnecesary #######################################

# #LEN_GLF is length
# #SPEc_GLF is species
# 
# names(GLFREC)
# GLFREC <- GLFREC[,c(2,4,10,11,14,15,16)]
# names(GLFREC)
# 
# GLFREC <- GLFREC[which(GLFREC$SPEC_GLF == 'SAPIDU'),]
# 
# #create a count column that maintains zero counts for sapidus
# GLFREC$SAP_CW <- rep(0,nrow(GLFREC))
# for (i in 1:nrow(GLFREC)) {
#   if (GLFREC$SPEC_GLF[i] == "SAPIDU"){
#     GLFREC$SAP_CW[i] <- GLFREC$LEN_GLF[i]
#   }
#   
# }
# names(GLFREC)
# plot(GLFREC$LEN_GLF[which(GLFREC$SPEC_GLF == 'SAPIDU')])
# 
# #carapace width yearly average
# library(data.table) # You'll also need to install this package
# DT = data.table(GLFREC)# Convert data.frame to data.table
# cw_timeline <- DT[,mean(SAP_CW), by = CRUISEID]
# colnames(cw_timeline)[2] <- "mean_cw"
# cw_sd <- DT[,sd(SAP_CW), by = CRUISEID]
# colnames(cw_sd)[2] <- "sd_cw"
# 
# cw_cruises <- merge(cw_timeline,cw_sd, by = 'CRUISEID')
# 
# 
# #add to 
# cw_test <-  merge(seamap, cw_cruises, by = 'CRUISEID')
# DT = data.table(cw_test)# Convert data.frame to data.table
# cw_timeline <- DT[,mean(mean_cw), by = Year]
# colnames(cw_timeline)[2] <- "mean_cw"
# 
# sd_timeline <- DT[,mean(sd_cw), by = Year]
# colnames(sd_timeline)[2] <- "sd_cw"
# 
# cw_timeline <- merge(cw_timeline, sd_timeline, by = 'Year')
# 
# plot(mean_cw ~ Year, data = cw_timeline, type = 'l')
# 
# 
# 
# plot(mean_cw ~ Year, data = cw_timeline, type = 'l', ylim=range(c(0, cw_timeline$mean_cw+cw_timeline$sd_cw)),
#      ylab = 'Mean catch / 30min trawl', main = 'Seamap C. sapidus sampling')
# 
# points(cw_timeline$Year, cw_timeline$mean_cw,ylim=range(c(0, cw_timeline$mean_cw +cw_timeline$sd_cw)),
#        pch=19, xlab="Year", ylab="Mean mean_cw +/- SD",
#        main="mean_cw and its Overdispersion")
# # hack: we draw arrows but with very special "arrowheads"
# arrows(cw_timeline$Year, 0, cw_timeline$Year, cw_timeline$mean_cw+cw_timeline$sd_cw, length=0.05, angle=90, code=3)