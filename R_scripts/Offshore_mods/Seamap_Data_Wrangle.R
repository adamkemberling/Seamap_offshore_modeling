#SEAMAP DATA WRANGLING, August 2018
#Adam A. Kemberling

#Packages
library(maps)
library(sp)
library(mapproj)
library(raster)
library(tidyverse)
library(gstat)
library(gdistance)
library(rgdal)
library(rgeos)

#Data import, data uploaded from GSMFC February 2018
BGSREC <- read_csv("~/Documents/KrackN/Seamap_man/GSMFC_data/BGSREC.csv")
ENVREC <- read_csv("~/Documents/KrackN/Seamap_man/GSMFC_data/ENVREC.csv")
CRUISES <- read_csv("~/Documents/KrackN/Seamap_man/GSMFC_data/CRUISES.csv") 
STAREC <- read_csv("~/Documents/KrackN/Seamap_man/GSMFC_data/STAREC.csv")
INVREC <- read_csv("~/Documents/KrackN/Seamap_man/GSMFC_data/INVREC.csv")
GLFREC <- read_csv("~/Documents/KrackN/Seamap_man/GSMFC_data/GLFREC.csv")  #GLFREC has SEX of catch

#Data Cleanup
names(GLFREC)
GLFREC <- GLFREC %>% filter(SPEC_GLF == "SAPIDU")


#Cruise/Survey Info
names(CRUISES) #cruise ID, Year, Survey Title, Data collector
CRUISES <- CRUISES %>% select(CRUISEID, YR, TITLE, SOURCE)


#Trawl info
names(STAREC) #Station ID, Cruise ID, MM/DD/YY, Start Lat, Start Lon, End Lat, End Lon, Vessel Speed, start depth, end depth
STAREC <- STAREC %>% dplyr::select(STATIONID, CRUISEID, MO_DAY_YR, DECSLAT, DECSLON, VESSEL_SPD, DEPTH_SSTA, DEPTH_ESTA)


#Tow Time
names(INVREC)
INVREC <- INVREC[,c("STATIONID","GEAR_SIZE","GEAR_TYPE","MESH_SIZE","MIN_FISH")]
tail(INVREC)


#Join cruise information with station vessel speed and depth
CRUISES$CRUISEID <- as.factor(CRUISES$CRUISEID)
STAREC$CRUISEID <- as.factor(STAREC$CRUISEID)
trawl.info <- left_join(STAREC, CRUISES, by = "CRUISEID")

summary(trawl.info$YR) #Survey Years


#Merge that with total catch info, and gear info
INVREC$STATIONID <- as.factor(INVREC$STATIONID)
trawl.info$STATIONID <- as.factor(trawl.info$STATIONID)
trawl.info <- left_join(trawl.info, INVREC, by = "STATIONID")


#catch data
names(BGSREC) #Station ID, Cruise ID, Genus, Species, Count
BGSREC <- BGSREC %>% dplyr::select(STATIONID, GENUS_BGS, SPEC_BGS, CNT)
table(BGSREC$GENUS_BGS)

#subset callinectes to check counts
callin <- BGSREC[BGSREC$GENUS_BGS == "CALLINE",]
table(factor(callin$SPEC_BGS))

#checking all similar genus names to callinectes, just replace CALLINE to check other genuses
table(factor(BGSREC[BGSREC$GENUS_BGS == "CALLINE",]$SPEC_BGS))

 
#create a count column that maintains zero counts for sapidus
BGSREC$SAP_CNT <- NA
BGSREC$SAP_CNT <- rep(0,nrow(BGSREC))
for (i in 1:nrow(BGSREC)) {
  if (BGSREC$SPEC_BGS[i] == "SAPIDU"){BGSREC$SAP_CNT[i] <- BGSREC$CNT[i]}
}


#Environmental dataset
names(ENVREC) #Cruise ID, Station ID, Lat, Long, BotDepth, BotTemp, BotSal, BotOx, Botchlor 
ENVREC <- ENVREC %>% dplyr::select(STATIONID,DEPTH_EMAX,TEMPSURF,TEMPMAX,SALSURF,SALMAX,OXYSURF,OXYMAX,
                    CHLORSURF,CHLORMAX,TURBSURF,TURBMAX,WECOLOR)


seamap_merge <-  left_join(ENVREC,BGSREC, by = "STATIONID")





#  getting one row for each station
#  &
#  selecting the sapidus rows for stations that have a catch


# 1. make a dataframe of just unique station numbers and whatever the blue crab catch was
catch.fix <- unique(data.frame("STATIONID" = seamap_merge$STATIONID, "COUNT" = seamap_merge$SAP_CNT))

# 2. create label for the row number
catch.fix$ROW <- c(1:nrow(catch.fix)) 

# 3. make a new column that matches a column in the larger dataset
catch.fix$SPEC_BGS <- c(1:nrow(catch.fix)) 
catch.fix$SPEC_BGS <- ifelse(catch.fix$COUNT > 0, catch.fix$SPEC_BGS <- 'SAPIDU', catch.fix$SPEC_BGS <- 'no_sap')

# 4. tag all the sapidus catches with "cal" dataframe
cal <- catch.fix[which(catch.fix$SPEC_BGS == 'SAPIDU'),] #tag sapidus rows

# 5. use the %not in% function to subset out rows
'%not in%' <- function (x, table) is.na(match(x, table, nomatch=NA_integer_)) #function for not in

#these two are the rows to keep
other.stations <- catch.fix[which(catch.fix$STATIONID %not in% cal$STATIONID == TRUE),] #stations with no sapidus
row.fix <- catch.fix[which(catch.fix$ROW %in% cal$ROW == TRUE),] #rows that have sapidus catches

keep <- nrow(other.stations) + nrow(row.fix) #double check the numbers to see we aren't leaving data behind


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
seamap_merge <- seamap_merge %>% dplyr::select(-GENUS_BGS, -SPEC_BGS, -CNT, -SAP_CNT) #dropping "GENUS_BGS","SPEC_BGS","CNT","SAP_CNT" and joining with station specific blue crab catch
seamap_merge <- merge(station.catch, unique(seamap_merge), by = 'STATIONID')
tail(seamap_merge)

seamap_merge$STATIONID <- as.factor(seamap_merge$STATIONID); trawl.info$STATIONID <- as.factor(trawl.info$STATIONID)
all.info <- left_join(seamap_merge, trawl.info, by = "STATIONID")
tail(all.info)



#save to computer to not need flash drive
write_csv(trawl.info,'~/Documents/KrackN/Seamap_man/GSMFC_data/Historic_cleaned/seamap_trawlinfo.csv' )
write_csv(seamap_merge, '~/Documents/KrackN/Seamap_man/GSMFC_data/Historic_cleaned/seamap_catchdata.csv')
write_csv(all.info, '~/Documents/KrackN/Seamap_man/GSMFC_data/Historic_cleaned/seamap_sapidus_consolidation_2018.csv')

#push test