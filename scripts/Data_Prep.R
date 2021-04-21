## run this from top directory (for correct paths)

### library setup

library(dplyr)
library(geosphere)


### Inflorescence calculations
data<-read.csv("outputs/HerbariumPopData_GDD_byDay.csv", header=T, stringsAsFactors = F)
names(data)
# exclude 'populations' (i.e. sample locations) that are missing climate data
data<-subset(data,!(is.na(data["GD"])))
# calculate actual lengths in cm
# length of flower portion of inflorescence (converting pixels to cm)
data$flower.cm<-data$actual/data$standard*data$flower.inf.length
# length of bud portion of inflorescence
data$bud.cm<-data$actual/data$standard*data$bud.inf.Length
# length of fruit portion of inflorescence
data$fruit.cm<-data$actual/data$standard*data$fruit.inf.Length
# length of aborted flower portion of inflorescence 
data$a.flower.cm<-data$actual/data$standard*data$aborted.inf.length
# length of inflorescence
data$infl.cm<-data$flower.cm+data$bud.cm+data$fruit.cm+data$a.flower.cm
# phenology index
data$phind<-(0* data$bud.cm + 0.5 * data$flower.cm + 1 * data$fruit.cm + 1* data$a.flower.cm)/data$infl.cm

write.csv(data, "outputs/PhenolAllData.csv", row.names=F)
print("Inf calculations Done")

### Region and Era Categorization

PhenolAllData<-read.csv("outputs/PhenolAllData.csv", header=T, stringsAsFactors = FALSE)
# Separate data into regions based on longitude
PhenolAllData$Region <-NULL
PhenolAllData$Region[PhenolAllData$Longitude > -76]<-"EastCoast"
PhenolAllData$Region[PhenolAllData$Longitude <= -76 & PhenolAllData$Longitude > -101]<-"Midwest"
PhenolAllData$Region[PhenolAllData$Longitude <= -101]<-"West"
PhenolAllData$Region<-as.factor(PhenolAllData$Region)

# Separate by date
PhenolAllData$Era <-NULL
PhenolAllData$Era[PhenolAllData$Year < 1900]<-"A: <1900"
PhenolAllData$Era[PhenolAllData$Year >= 1900 & PhenolAllData$Year < 1940]<-"B: 1900-1939"
PhenolAllData$Era[PhenolAllData$Year >= 1940 & PhenolAllData$Year < 1980]<-"C: 1940-1980"
PhenolAllData$Era[PhenolAllData$Year >= 1980]<-"D: >1980"

write.csv(PhenolAllData, "outputs/PhenolAllData.csv", row.names=F)
print("Region Era category done")

### Binning for earliest occurrence 

if (!file.exists("outputs/Herbarium_MinYear.csv")){
	PhenolAllData$source<-"herbarium"
	bison<-read.csv("data/bison-lythrum_salicaria.csv", stringsAsFactors = FALSE)
	names(bison)[1]<-"ID"
	bison$source<-"bison"

	# GBIF data setup
	GBIF<-read.csv("data/GBIF-Lythrum.csv", stringsAsFactors = FALSE)
	names(GBIF)[1]<-"ID"
	GBIF$source<-"GBIF"
	# Vascan data setup
	vascan<-read.csv("data/VASCAN-Lythrum.csv", stringsAsFactors = FALSE)
	names(vascan)[1]<-"ID"
	vascan$source<-"vascan"
	vascan$decimalLatitude<-as.numeric(as.character(vascan$decimalLatitude))
	vascan$decimalLongitude<-as.numeric(as.character(vascan$decimalLongitude))

	# merge three occurrence data sets
	alldat<-rbind(bison[,c("ID", "year", "decimalLatitude", "decimalLongitude", "source")], GBIF[,c("ID", "year", "decimalLatitude", "decimalLongitude", "source")], vascan[,c("ID", "year", "decimalLatitude", "decimalLongitude", "source")])
	alldata<-na.omit(alldat)
	names(alldata)<- c("Pop_Code", "Year", "Latitude", "Longitude", "source")

	# merge occurrence data set to herbarium data set
	alldata<-rbind(alldata, PhenolAllData[,c("Pop_Code", "Year", "Latitude", "Longitude", "source")])

	# compute distance matrix
	distancematrix<-as.data.frame(distm(alldata[,c("Longitude", "Latitude")], fun=distCosine))

	alldata$minYear<-NA

	for (i in 1:nrow(alldata)) {
		alldata$minYear[i]<-min(alldata$Year[which(distancematrix[,i]<250*1000)])
	}

	alldata$time<-alldata$Year-alldata$minYear
	alldata<-alldata[alldata$Latitude>0,]

	write.csv(alldata, "outputs/Occurrences_MinYear.csv", row.names=F)

	minYeardata<-alldata[alldata$source=="herbarium",c("Pop_Code", "minYear")]

	write.csv(minYeardata, "outputs/Herbarium_MinYear.csv", row.names=F)	
} else {
	alldata<-read.csv("outputs/Occurrences_MinYear.csv", stringsAsFactors = FALSE)
}

# binning
binx<-0.5 # long bin size in degrees
biny<-0.5 # lat bin size in degrees

longs<-seq(min(alldata$Longitude,na.rm=T)+binx,max(alldata$Longitude,na.rm=T)-binx,by=binx*2)
lats<-seq(min(alldata$Latitude,na.rm=T)+biny,max(alldata$Latitude,na.rm=T)-biny,by=biny*2)

# Make into matrix
bindat<-data.frame(Longitude=rep(longs,length(lats)),Latitude=sort(rep(lats,length(longs))),minYear=NA)

for (i in 1:nrow(bindat)){
  bindat$minYear[i]<-min(alldata$minYear[alldata$Latitude >= bindat$Latitude[i]-biny & alldata$Latitude < bindat$Latitude[i]+biny & alldata$Longitude >= bindat$Longitude[i]-binx & alldata$Longitude < bindat$Longitude[i]+binx])
}

bindat<-bindat[bindat$minYear!=Inf,]

firstyear<-read.csv("outputs/Herbarium_MinYear.csv", header=T, stringsAsFactors = FALSE)

PhenolAllData$Pop_Code<-as.character(PhenolAllData$Pop_Code)
firstyear$Pop_Code<-as.character(firstyear$Pop_Code)

PhenolAllData$time<-NULL
PhenolAllData<-left_join(PhenolAllData, firstyear)
## rudimentary first year since invasion to year of collection, will be replaced by krieging estimate

PhenolAllData$time<-PhenolAllData$Year-PhenolAllData$minYear

write.csv(PhenolAllData, "outputs/PhenolAllData.csv", row.names=F)
print("Appended min Year")
print("Data prep done. Go do krieging and get krieg data for history.")




