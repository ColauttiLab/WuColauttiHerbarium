library(ggplot2)
library(mapdata)
library(maptools)
library(raster)
library(rgeos)
library(rworldmap)
library(gstat)
library(ggpolypath)
library(rgdal)
alldata<-read.csv("../outputs/Occurrences_MinYear.csv")
alldata<-alldata[alldata$Longitude< -50,]

## binning
binx<-0.5 ## long bin size in degrees
biny<-0.5 ## lat bin size in degrees

longs<-seq(min(alldata$Longitude,na.rm=T)+binx,max(alldata$Longitude,na.rm=T)-binx,by=binx*2)
lats<-seq(min(alldata$Latitude,na.rm=T)+biny,max(alldata$Latitude,na.rm=T)-biny,by=biny*2)

## Make into matrix
bindat<-data.frame(Longitude=rep(longs,length(lats)),Latitude=sort(rep(lats,length(longs))),minYear=NA)


for (i in 1:nrow(bindat)){
  bindat$minYear[i]<-min(alldata$minYear[alldata$Latitude >= bindat$Latitude[i]-biny & alldata$Latitude < bindat$Latitude[i]+biny & alldata$Longitude >= bindat$Longitude[i]-binx & alldata$Longitude < bindat$Longitude[i]+binx])
}

bindat<-bindat[bindat$minYear!=Inf,]

coordinates(bindat) <- ~ Longitude + Latitude
# bbox(bindat)
# coordinates(bindat)

test.vgm <-variogram(minYear~1, locations=coordinates(bindat), bindat)
test.fit<-fit.variogram(test.vgm, vgm("Sph"))

sPDF<-readOGR("small", "North America")
# mapCountryData(sPDF[sPDF$continent=="North America",])

#northamerica<-subset(sPDF, continent=="North America")

points<-spsample(sPDF, 500000, "regular")
pointlocations<-as.data.frame(points@coords)
names(pointlocations)<- c('x','y')
pointlocations<-pointlocations[pointlocations$y< 60,]
proj4string(bindat)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

coordinates(pointlocations) <- ~ x+y
proj4string(pointlocations)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

test.kriged <- krige(minYear ~ 1, bindat, pointlocations, model=test.fit, nmin=2, maxdist=150)

fitmap<-as.data.frame(test.kriged)
write.csv(fitmap, "../outputs/fitmap.csv", row.names=F)
# continental<-map_data("worldHires", c("USA", "Canada"))
continental <- fortify(sPDF)
krigemap<- ggplot() +
  geom_polypath(data=continental, aes(x=long, y=lat, group=group), fill = "white", colour = "black") +
  geom_raster(data=fitmap[fitmap$var1.pred>1799,], aes(x=x, y=y, fill=var1.pred)) +
  coord_equal(ylim=c(31,59), xlim=c(-129,-51)) +
  labs(x="Longitude", y="Latitude") +
  scale_fill_gradientn(name="Year",colours = rainbow(10)) + theme_classic() +
  theme(panel.background = element_rect(fill = "#BFEEF0", color = "black"))


ggsave("../outputs/figures/fig_a1_krige.pdf", krigemap, width = 6, height = 6)
ggsave("../outputs/figures/fig_a1_krige.jpg", krigemap, width = 6, height = 6)

