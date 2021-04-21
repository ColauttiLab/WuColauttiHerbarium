# Calculate Geodetic distance between two lat/long points
geodist <- function(locs){ #INPUT AS c(lat1,long1,lat2,long2)
  R<-6371
  locs<-locs*pi/180
  dist<-sin(locs[1])*sin(locs[3])+cos(locs[1])*cos(locs[3])*cos(abs(locs[2]-locs[4]))
  dist <- acos(round(dist,10)) # round avoids error with acos for some values (I think it is something to do with calculating acos on a float with a small rounding error)
  return(R*dist)
}
