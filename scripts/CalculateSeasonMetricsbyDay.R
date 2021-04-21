############################################
## This script calculates growing degree days (GDD)
## using weather data from DownloadStationData.R
## then interpolates GDD for each population
## and growing season metrics.
############################################


##############################
## Load functions
##############################
library(fields) # Used for spatial interpolation of GDD for each population
library(zoo) # Used to impute missing data points for some weather stations in some days
library(dplyr)
library(tibble)
library(FSA)
library(reshape2)
source("functions/idw.R")

############

##############################
## Load data
##############################
#setwd("~/2016 Queens/WeatherScraper/WeatherScraper/")
StnData<-read.csv("WeatherRawData/NOAAStationData.csv")
PopData<-read.csv("data/PopData_2018_02_08.csv", header=T)

##############################
## Calculate GDD for 20 closest stations for each Pop
##############################

# For each population in the dataset
# Find nearby stations in NOAAData 
# load station growing degrees for each day (GD)
# NOTE: GD = (TMAX+TMIN)/20-8 and set GD=0 if GD<0


##GDeg : growing degree day per day
PopData$GD<-NA # Number of growing days above 5oC (Season Length)
PopData$GDs<-NA # Number of growing days from start of season to collection date
PopData$GDD<-NA # Standard Growing-Degree Days, as above
PopData$GDDs<-NA # GDD from start of season to date of collection
PopData$meanGDeg<-NA ##mean growing-degress per day over season
PopData$varGDeg<-NA ##variance of growing degrees per day over season
PopData$skewGDeg<-NA ## skew of """"
PopData$kurtGDeg<-NA ## kurtosis of """"
PopData$numStns <- NA ##number of stations used for the analysis

GeoStns <- NULL


Cntr<-0

# For each year: 


for(year in (1866:2016)){ 
  
  # Open file with GD data
  GDFilePath<-paste0("WeatherRawData/NOAAStnsClose",year,".csv") 
  GDData<-read.csv(GDFilePath)
  for(Pop in PopData$Pop_Code[PopData$Year==year]){ # Cycle through pop_codes sampled in same year as GD data
    Cntr<-Cntr+1
    # Find names of nearby stations
    LocStns<-paste(unique(StnData$StationID[StnData$Pop_Code==Pop & StnData$Measure=="TMAX"]))
    if (length(LocStns) == 0) { 
      next }
    # Subset GDData for stations of interest from Jan 1 to day of sampling
    # find all stations in station list, and cross with all stations in the year data
    PopGDData<-GDData[GDData$StationID %in% LocStns,]
    # reshape data frame so that each row is a day, and each column a station
    test<- dcast(PopGDData, Date ~ StationID, value.var = "GDeg")
    ## make days the row names
    test %>% remove_rownames %>% column_to_rownames(var="Date") -> test
    test<- test[colSums(!is.na(test)) > 0]
    if (nrow(test) < 300) { next 
      }
    StnId<-names(test)
    # find all stations from station list based on population code, and only the stations that exist in yearly data set
    # Make data frame get geographic distance, lat, long for each station, subset to 20 stations if necessary
    
    GeoDat<-unique(StnData[StnData$Pop_Code==Pop & StnData$StationID %in% StnId,c("StationID", "Latitude", "Longitude", "Dist")])
    GeoDat$Dist<-ifelse(GeoDat$Dist==0, 0.0001, GeoDat$Dist)
    
    if (ncol(test) >20 ) {
      # reduce number of stations to closest 20
      GeoDat<- head(GeoDat[order(GeoDat$Dist),],20)
      PopGDData<-GDData[GDData$StationID %in% GeoDat$StationID,]
      ## resubset again to reduce yearly data set to only the data on the twenty or less stations
      test<- dcast(PopGDData, Date ~ StationID, value.var = "GDeg")
      ## make days the row names
      test %>% remove_rownames %>% column_to_rownames(var="Date") -> test
      ## double check here for stations in data set
      test<- test[colSums(!is.na(test)) > 0]
      StnId<-names(test)
      GeoDat<-unique(StnData[StnData$Pop_Code==Pop & StnData$StationID %in% StnId,c("StationID", "Latitude", "Longitude", "Dist")])
      GeoDat$Dist<-ifelse(GeoDat$Dist==0, 0.0001, GeoDat$Dist)
      }


    # GeoDat$GD<-NA ##Tota Growing Season Length (in Days)
    # GeoDat$GDs<-NA ##Growing Seaon Length to collection (in Days)
    # GeoDat$GDD<-NA ##Total Growing Degree Days over season (in Growing Degrees)
    # GeoDat$GDDs<-NA ##Growing Degree Days cumulative to time of collection (in Growing Degrees)
    # GeoDat$meanGDeg<-NA ##mean of growing degrees per day / total season length (GDD/GD)
    # GeoDat$varGDeg<-NA ##variance of growing degrees per day over the season 
    # GeoDat$skewGDeg<-NA ##skew of growing degrees per day over the season 
    # GeoDat$kurtGDeg<-NA ##variance of growing degrees per day over the season 
    # 
    
  # Growing degree day interpolation 
    test$GDeg<- apply(test,1, FUN=idw, distance=GeoDat$Dist)
    


   
    ##Calculate GD, GDs, GDD, GDDs values for each station
      ###
    yday<-as.numeric(PopData$yday[PopData$Pop_Code==Pop & PopData$Year==year])
    test$Indicator <-FALSE
      ####Set days with positive GDD to true
    test$Indicator <- ifelse(test$GDeg > 0, TRUE, test$Indicator)
    ## Give sequences of growing degree days and non-growing days
    rletest<-rle(test$Indicator)
    ##put rle results into table
    length<-rletest$lengths
    value<-rletest$values
    df<-data.frame(length, value)
    df<- df %>% rownames_to_column()
    ##find values, but only start growing season 
    intervals<-df[which(df$length >= 10 & df$value ==TRUE),]
    ## row numbers for the beginning(min) and end(max) of the growing season
    rowdates<-c(min(as.numeric(intervals$rowname)), max(as.numeric(intervals$rowname)))  ## first value is beginning of season, second value is end of season
    df$cumDayminus<-pcumsum(df$length) ### these values are for beginning of season
    df$cumDay<-cumsum(df$length) ### these values are used for end of season
    begin <- as.numeric(df$cumDayminus[as.integer(rowdates[1])]) +1 ##first day of season, need one day added because cumDayminus means it starts at day before the actual start of season
    end <- as.numeric(df$cumDay[as.integer(rowdates[2])]) ##last day of season
      
      
      #Calculations for season length, and season to collection, moments of distribution
    GD <- (end - begin) + 1  ##season length, need +1 so start of season is included
    GDs <- yday-begin+1 ##length of season to collection, need +1 so start of season is included
    test[mapply(is.infinite, test)] <- NaN
    test$GDeg<- ifelse(test$GDeg<0, 0, test$GDeg)
    GDD <- sum(test$GDeg[begin:end], na.rm=T) ## GDD for the entire season
    GDDs <- sum(test$GDeg[begin:yday], na.rm=T) ##GDD from start of season to collection
      
    test <- test[c(begin:end),] ##subset data to only growing season
    meanGDeg <- mean(test$GDeg, na.rm=T) ##mean of growing degrees per day
    varGDeg <- sum((test$GDeg - meanGDeg)^2, na.rm=T)/GD ## var of growing degrees per day for growing season, no adjustion for sample size
    skewGDeg <- ((sum((test$GDeg - meanGDeg)^3, na.rm=T))/GD) /(varGDeg)^(3/2) ##skewness, Fisher-Pearson (not adjusted for sample size)
    kurtGDeg <- ((sum((test$GDeg - meanGDeg)^4, na.rm=T))/GD) /(varGDeg)^(4/2) -3 ##excess kurtosis for univariate data, 
      
      
      
      ##Data Values to Put into PopDat (all stations for each pop)
    PopData$GD[PopData$Pop_Code==Pop & PopData$Year==year] <- GD
    PopData$GDs[PopData$Pop_Code==Pop & PopData$Year==year] <- GDs
    PopData$GDD[PopData$Pop_Code==Pop & PopData$Year==year] <- GDD
    PopData$GDDs[PopData$Pop_Code==Pop & PopData$Year==year] <- GDDs
    PopData$meanGDeg[PopData$Pop_Code==Pop & PopData$Year==year] <- meanGDeg
    PopData$varGDeg[PopData$Pop_Code==Pop & PopData$Year==year] <- varGDeg
    PopData$skewGDeg[PopData$Pop_Code==Pop & PopData$Year==year] <- skewGDeg
    PopData$kurtGDeg[PopData$Pop_Code==Pop & PopData$Year==year] <- kurtGDeg
    PopData$numStns[PopData$Pop_Code==Pop & PopData$Year==year] <- ncol(test) - 2 # minus GDeg column and Indicator column
    GeoStns <- rbind(GeoStns, cbind(GeoDat, Pop_Code = Pop))
    write.csv(GeoStns, "GeoStns.csv", row.names = F)
    cat("***************\nIteration ",Cntr," of",length(PopData$Pop_Code),"\nYear: ",year,"\nPop: ",Pop,"\n",Sys.time(),"seconds","\nGD: ",PopData$GDD[PopData$Pop_Code==Pop],"\nGDDs: ",PopData$GDDs[PopData$Pop_Code==Pop],"\n***************")
    yday<-LocStns<-PopGDData<-GeoDat<-tps<-NA # clean up for next iteration of pop
    # SAVE output
    write.csv(PopData,"HerbariumPopData_GDD_byDay.csv",row.names=F)
  }
  GDData<-GDFilePath<-NA # Clean-up for next iteration of year
}

## manually fixed problems:
# Pop_Code 366284 : two stations with error data, removed from interpolation

