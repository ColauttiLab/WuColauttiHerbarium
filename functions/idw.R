idw <- function(distance, value) {
  weighting<-1/distance^2
  interpolation<-(sum(weighting*value, na.rm=T)/sum(weighting[value>=0], na.rm=TRUE))
  return(interpolation)
}
