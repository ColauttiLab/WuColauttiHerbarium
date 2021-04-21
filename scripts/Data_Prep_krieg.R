library(purrr)
PhenolAllData <- read.csv("outputs/PhenolAllData.csv")
fitmap <- read.csv("outputs/fitmap.csv", stringsAsFactors = F)

min_year_finder<- function(longitude, latitude, fitmap){
  nearest_lat <- which(abs(fitmap$y-latitude)==min(abs(fitmap$y-latitude)))
  nearest_long <- which(abs(fitmap$x-longitude)==min(abs(fitmap$x-longitude)))
  row <- intersect(nearest_lat, nearest_long)
  if(length(row) == 1) {
    return(fitmap[row,"var1.pred"])
  } else {
    return(0)
  }
  
}

min_years <-purrr::map2_dbl(PhenolAllData$Longitude, PhenolAllData$Latitude, min_year_finder, fitmap = fitmap)


PhenolAllData$minYear <- min_years
PhenolAllData <- PhenolAllData[!is.na(PhenolAllData$minYear),]
# PhenolAllData <- PhenolAllData[PhenolAllData$minYear != 0,]
PhenolAllData$time <- PhenolAllData$Year - PhenolAllData$minYear
PhenolAllData <- PhenolAllData[PhenolAllData$time >= 0,]

write.csv(PhenolAllData, "outputs/PhenolAllData.csv")
