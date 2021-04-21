
## Montague Field Data
FieldData<-read.csv("../data/FieldPhenology.csv", header=T, stringsAsFactors = FALSE)
names<-names(FieldData)
names[1]<-"Pop_Code"
names[10]<-"yday"
names[8]<-"calculatedGDD"
names(FieldData)<-names
FieldData$Year<-2004
FieldData$yday<-round(FieldData$yday) ##rounded in order to make GD and GDD calcuations


write.csv(FieldData, "../outputs/FieldPopulation.csv", row.names=FALSE)


## Montague Greenhouse Data

GHData<-read.csv("../data/MontagueData_Cleaned.csv", header=T, stringsAsFactors = F)
GHData<-GHData[-1,] # Remove dummy variable from 1st row
GHData$Longitude<-GHData$Longitude*-1
names(GHData)[1]<-"Pop_Code"
GreenhouseData<- aggregate(GHData, by=list(GHData$Pop_Code), FUN=mean)
GreenhouseData$yday<-round(GreenhouseData$Days) ##rounded in order to make GD and GDD calcuations
names(GreenhouseData)[1]<-"Pop_Code"
names(GreenhouseData)
str(GreenhouseData)
GreenhouseData<-GreenhouseData[GreenhouseData$Pop_Code!="ONEC",]
GreenhouseData$fti<-(GreenhouseData$yday-mean(GreenhouseData$yday, na.rm=TRUE))/sd(GreenhouseData$yday, na.rm=TRUE)
GreenhouseData<-GreenhouseData[,c("Pop_Code", "Latitude", "fti")]
GreenhouseData$Source<-"MGreenhouse"
write.csv(GreenhouseData, "../outputs/MontagueGreenhouse.csv", row.names=F)

## Colautti Barrett Common Garden Data

library(tidyr)
library(dplyr)
CommonGarden<-read.csv("../data/CBCommonGarden_GDD_byDay.csv", stringsAsFactors = F)
names(CommonGarden)[1]<-"Individualnumber"
CommonGarden %>% separate(Plant_ID, c("Sitenumber", "Site1", "Mat", "Row", "Position"), sep="_" ) -> CommonGarden
CommonGarden$Pop<-CommonGarden$Mat # Extract population code from maternal family code
CommonGarden$Pop<-gsub("[0-9]","",CommonGarden$Pop)
AllData<-read.table("../data/AllDataFixed.txt", header=T, sep=",")
AllData<-AllData[,c("Pop_Code", "Pop", "Lat")]
names(AllData)<-c("Pop", "Pop_Code", "Lat")
AllData<-unique(AllData)
AllData %>% mutate_if(is.factor, as.character) -> AllData
CommonGarden<-left_join(CommonGarden, AllData)

CommonGarden <- CommonGarden %>% group_by(Site1, Pop_Code, Lat) %>% summarize(fti = mean(GDs)) %>% ungroup() %>% as.data.frame()



CommonGarden$fti <- (CommonGarden$fti - mean(CommonGarden$fti, nam.rm = TRUE))/sd(CommonGarden$fti, na.rm = TRUE)

CommonGarden <- CommonGarden[, c("Pop_Code", "fti", "Lat", "Site1")]

names(CommonGarden)[3]<-"Latitude"
names(CommonGarden)[4]<-"Source"
write.csv(CommonGarden, "../outputs/CBCommonGarden.csv", row.names=F)

## Colautti Barrett Greenhouse Data

RCGreenhouseData<-read.table("../data/AllDataFixed.txt", header=T, sep=",")
RCGreenhouseData<-RCGreenhouseData[,c("Fam", "Pop_Code", "Pop", "Ind", "Lat", "FdaysAdj")]
RCGreenhouseData<-RCGreenhouseData[RCGreenhouseData$FdaysAdj!=".",]
RCGreenhouseData$FdaysAdj<-as.numeric(RCGreenhouseData$FdaysAdj)
RCGreenhouseData$fti<-(RCGreenhouseData$FdaysAdj-mean(RCGreenhouseData$FdaysAdj))/sd(RCGreenhouseData$FdaysAdj)
## first aggregate by family in populations
avgRCGreenhouseData<-aggregate(RCGreenhouseData, by=list(RCGreenhouseData$Pop, RCGreenhouseData$Fam), FUN="mean")
## then aggregate by population
avgRCGreenhouseData<-aggregate(RCGreenhouseData, by=list(RCGreenhouseData$Pop), FUN="mean")
## subset data frame and rename columns
avgRCGreenhouseData<-avgRCGreenhouseData[,c("Group.1", "Lat", "fti")]
names(avgRCGreenhouseData)<-c("Pop_Code", "Latitude", "fti")
avgRCGreenhouseData$Source<-"RCGreenhouse"
write.csv(avgRCGreenhouseData, "../outputs/CBGreenhouseData.csv", row.names=F)

## merge all data together 

PhenolAllData <- read.csv("../outputs/PhenolAllData.csv", stringsAsFactors = FALSE)
PhenolAllData<-subset(PhenolAllData, Year >=1960)
ValidHerb<-PhenolAllData[(PhenolAllData$Latitude < 49) & (PhenolAllData$Longitude > -85), ]
ValidHerb<-ValidHerb[(ValidHerb$Latitude > 38) & (ValidHerb$Longitude < -74), ]

binx<-0.5 ## long bin size in degrees
biny<-0.5 ## lat bin size in degrees
longs<-seq(min(ValidHerb$Longitude,na.rm=T)+binx,max(ValidHerb$Longitude,na.rm=T)-binx,by=binx*2)
lats<-seq(min(ValidHerb$Latitude,na.rm=T)+biny,max(ValidHerb$Latitude,na.rm=T)-biny,by=biny*2)
## Make into matrix
bindat<-data.frame(Longitude=rep(longs,length(lats)),Latitude=sort(rep(lats,length(longs))),fti=NA,GD=NA)

lat_groups <- c(seq(min(ValidHerb$Latitude), max(ValidHerb$Latitude)), max(ValidHerb$Latitude)) 

labs <- levels(cut(round(ValidHerb$Latitude, 2), lat_groups, dig.lab = 4))

intervals <- cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ))

mean_lats <- apply(as.data.frame(intervals), 1, mean)

mean_lats[length(mean_lats)] <- mean_lats[length(mean_lats) - 1] + 1

ValidHerb$pLatitude <- cut(ValidHerb$Latitude, lat_groups, include.lowest = T,  labels = mean_lats)

mean_herbarium <- ValidHerb %>% 
  select(Pop_Code, pLatitude, fti) %>% 
  group_by(pLatitude) %>% summarise(fti = mean(fti)) %>% 
  mutate(Source = "Herbarium", Pop_Code = pLatitude) %>% 
  select(Pop_Code, Latitude = pLatitude, fti, Source)

mean_herbarium$fti<-(mean_herbarium$fti - mean(mean_herbarium$fti, na.rm = T))/sd(mean_herbarium$fti, na.rm=T)

Validationdf<-rbind(GreenhouseData, CommonGarden, avgRCGreenhouseData)
AllData<-rbind(Validationdf, mean_herbarium)
write.csv(AllData, "../outputs/Validationdataset.csv", row.names=F)

bootstrap_df <- ValidHerb %>% 
  select(Pop_Code, pLatitude, fti) %>% 
  mutate(Source = "Herbarium", Pop_Code = pLatitude) %>% 
  select(Pop_Code, Latitude = pLatitude, fti, Source) %>% 
  mutate_if(is.factor, as.character) %>%
  mutate(Latitude = as.numeric(Latitude)) 

CommonGarden<-read.csv("../data/CBCommonGarden_GDD_byDay.csv", stringsAsFactors = F)
names(CommonGarden)[1]<-"Individualnumber"
CommonGarden %>% separate(Plant_ID, c("Sitenumber", "Site1", "Mat", "Row", "Position"), sep="_" ) -> CommonGarden
CommonGarden$Pop<-CommonGarden$Mat # Extract population code from maternal family code
CommonGarden$Pop<-gsub("[0-9]","",CommonGarden$Pop)
AllData<-read.table("../data/AllDataFixed.txt", header=T, sep=",")
AllData<-AllData[,c("Pop_Code", "Pop", "Lat")]
names(AllData)<-c("Pop", "Pop_Code", "Lat")
AllData<-unique(AllData)
AllData %>% mutate_if(is.factor, as.character) -> AllData
CommonGarden<-left_join(CommonGarden, AllData)

CommonGarden$find<-(CommonGarden$GDs-mean(CommonGarden$GDs, na.rm=TRUE))/sd(CommonGarden$GDs, na.rm=TRUE)
CommonGarden$fti<-CommonGarden$find
CommonGarden<- CommonGarden[,c("Pop_Code","fti", "Lat", "Site1")]
names(CommonGarden)[3]<-"Latitude"
names(CommonGarden)[4]<-"Source"

bootstrap_df <- rbind(bootstrap_df, CommonGarden)

write.csv(bootstrap_df, "../outputs/bootstrap_validation.csv", row.names = F)




