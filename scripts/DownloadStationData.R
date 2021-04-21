############################################
## This script imports weather data 
############################################
##Requires data file in csv with locations and year. 
##############################
## Load functions
##############################
# Key paramaters:
# datasetid - see: http://www.ncdc.noaa.gov/cdo-web/datasets
dset<-"GHCND" # Daily climate observations

##############################
## Load functions
##############################
source("functions/geodist.R")
source("functions/clstation.R")

##############################
## Load population data
##############################

PopData<-read.csv("data/PopData_2018_02_08.csv", stringsAsFactors = F)
print("ReadinData")
##############################
## 1. Find Weather Stations
##############################
# Make Map Directory if needed
dir.create("WeatherRawData", showWarnings = FALSE)
# Download station locations unless they already exist:
if(!file.exists("WeatherRawData/ghcnd-inventory.txt")){  
  download.file(url=paste0("http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"),destfile="WeatherRawData/ghcnd-inventory.txt")
}
# Data format note:
# No headers V1. Station ID; V2. Latitude; V3. Longitude; V4. Observation type; V5. Earliest Year of Record; V6. Latest Year of Record
# See http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
# A few key measurements in V4:
# PRCP - precipitation (1/10mm); SNWD - snow depth (mm); TMAX - max temp (1/10 deg C); TMIN - min temp (1/10 deg C); WESD - water equivalent of snow on ground (1/10mm)
# Measurements of interest
MsrInt<- c("PRCP","SNWD","TMAX","TMIN","WESD")
# Create object with weather station info
InData<-read.table("WeatherRawData/ghcnd-inventory.txt") 

# Skip import data step if file already exists
if(file.exists("WeatherRawData/NOAAStationData.csv")){  
  NOAAData<-read.csv("WeatherRawData/NOAAStationData.csv")
} else {
  NOAAData<-{}
  Ndg<-0.5 # Range (+/- degrees lat/log) to search for weather stations
  SMax<-100 # Max number of stations to retain (NOTE: Final number will be smaller due to missing weather data, etc.)
  for(i in 1:nrow(PopData)){
    # Find all stations +/- Ndg degrees and have records in year of collection
    CloseStations<-InData[InData$V2 > PopData$Latitude[i]-Ndg & InData$V2 < PopData$Latitude[i]+Ndg &
                            InData$V3 > PopData$Longitude[i]-Ndg & InData$V3 < PopData$Longitude[i]+Ndg & InData$V5 <= PopData$Year[i] & InData$V6 >= PopData$Year[i],1:4]
    names(CloseStations)<-c("StationID","Latitude","Longitude","Measure")
    # Use wider area if < SMax stations
    if(length(unique(CloseStations$StationID[CloseStations$Measure=="TMAX"]))<SMax){ 
      Ndg<-1
      CloseStations<-InData[InData$V2 > PopData$Latitude[i]-Ndg & InData$V2 < PopData$Latitude[i]+Ndg &
                              InData$V3 > PopData$Longitude[i]-Ndg & InData$V3 < PopData$Longitude[i]+Ndg & InData$V5 <= PopData$Year[i] & InData$V6 >= PopData$Year[i],1:4]
      names(CloseStations)<-c("StationID","Latitude","Longitude","Measure")
    }
    
    # Add pop info and calculate distances
    if(nrow(CloseStations)>0){ # if at least one result is returned..
      # Calculate distance from population to each station
      CloseStations<-cbind(CloseStations,Pop_Code=PopData$Pop_Code[i])
      CloseStations$Dist<-NA
      for(j in 1:nrow(CloseStations)){ # Cycle through each station
        CloseStations$Dist[j]<-geodist(c(CloseStations$Latitude[j],CloseStations$Longitude[j],PopData$Latitude[i],PopData$Longitude[i]))   
      }
    }
    # Find up to SMax closest stations for EACH measure of interest
    for (Msr in MsrInt) { # Note - need to use same stations for tmax & tmin so don't do separately
      KeepSt<-subset(CloseStations,Measure==Msr)
      # If >SMax results, keep closest SMax 
      if(nrow(KeepSt)>SMax){
        KeepSt<-KeepSt[order(KeepSt$Dist),][1:SMax,]
      }
      # Add to main Stations list
      NOAAData<-rbind(NOAAData,KeepSt)
	  print(i)
    }      
  }
  # Eliminate unused factor levels
  ## Rename some columns for later merging
  NOAAData$Measure<-factor(NOAAData$Measure)
  NOAAData$Type<-"NOAA_GHCN"
  write.csv(NOAAData,"WeatherRawData/NOAAStationData.csv", row.names=FALSE) 
}
print("Station Search Complete")
##############################
## Download data and calculate Growing Degrees per day
##############################
# Download weather data from NOAA (ftp server has all locations for each year)
# Use FTP site to download data since we have >1,000 stations * years
for (year in 1866:2016){
  WthrData<-{} # Reset 'stations' data in each iteration
  FilePath<-paste0("WeatherRawData/NOAA_GHCN",year,".csv.gz")
  if(!file.exists(FilePath)){  # Skip if file already downloaded
    download.file(url=paste0("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/",year,".csv.gz"),destfile=FilePath)
    # Data format note:
    # No headers V1. Station ID; V2. Date, V3. Observation type; V4. Observation Value; V5. Observation time
    # See http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
    # A few key measurements:
    # PRCP - precipitation (1/10mm); SNWD - snow depth (mm); TMAX - max temp (1/10 deg C); TMIN - min temp (1/10 deg C); WESD - water equivalent of snow on ground (1/10mm)    
  }
  GDFilePath<-paste0("WeatherRawData/NOAAStnsClose",year,".csv") 
  if(!file.exists(GDFilePath)){  # Skip if data already calculated
    # Open Weather data
    WthrData<-read.csv(FilePath,header=F)[,1:4]
    ## NOTE: Opening file takes ~ 5sec/MB; or about 15min per file ()
    # Keep only stations in NOAAData and only weather data of interest
    WthrData<-WthrData[WthrData$V1 %in% NOAAData$StationID & WthrData$V3 %in% c("TMAX","TMIN","SNWD","PRCP","WESD"),]
    # Add null data to avoid error message during reshape		
    nodat<-data.frame(V1=NA,V2=NA,V3=c("TMAX","TMIN","SNWD","PRCP","WESD"),V4=NA)		
    WthrData<-rbind(WthrData,nodat)
    # Reorganize data into 'wide' format
    WthrData<-reshape(WthrData,v.names="V4",idvar=c("V1","V2"),timevar="V3",direction="wide")
    WthrData<-WthrData[,c("V1","V2","V4.TMAX","V4.TMIN","V4.SNWD","V4.PRCP","V4.WESD")]
    names(WthrData)<-c("StationID","Date","TMAX","TMIN","SNWD","PRCP","WESD")
    WthrData$Date<-as.Date(gsub("([0-9]{4})([0-9]{2})([0-9]{2})","\\1-\\2-\\3",WthrData$Date))
    WthrData$Day<-strptime(WthrData$Date,format="%Y-%m-%d")$yday
    ##GDD calculation, (Tmax + Tmin)/2 - Tbase, divided by ten due to the temperature being counted in tenths of a degree
    WthrData$GDeg <- (WthrData$TMAX/10 + WthrData$TMIN/10)/2 - 8
    # Save data frame
    write.csv(WthrData,GDFilePath,row.names=FALSE)
    print(GDFilePath)
  }
}

print("COMPLETED")
