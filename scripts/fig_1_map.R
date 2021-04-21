library(ggplot2)
library(ggmap)
library(mapdata)
library(rgdal)
# read in stations used in interpolation
StnData <- read.csv("../outputs/GeoStns.csv")
# read in herbarium data
PhenolAllData <- read.csv("../outputs/PhenolAllData.csv")
# find unique stations by their id
UniqueStns<-aggregate(StnData[,c(2,3)], by=list(StnData$StationID), FUN=mean)

# get map of North America
continental <- readOGR("small", "North America")

library(cowplot)
library(ggpolypath)
# with annotation, and no region coloring

Regionmap <- ggplot() +
  geom_polypath(data=continental, aes(x=long, y=lat, group=group), fill = "white", color = "black") +
  coord_cartesian(ylim=c(32,54), xlim=c(-129,-52)) +
  geom_point(data=UniqueStns, aes(x=Longitude, y=Latitude), color="gray50", alpha=0.3, size = 1 ) +
  geom_point(data= PhenolAllData, aes(x=Longitude, y=Latitude), color="#CC33FF", alpha=0.6, size=0.5) + 
  theme_classic() + 
  theme(panel.background = element_rect(fill = "#BFEEF0", color = "black")) +  
  scale_y_continuous(name = "Latitude") + 
  scale_x_continuous(name = "Longitude",breaks = c(-76, -101), sec.axis = dup_axis(name = NULL))


annotation_plot <- ggdraw(Regionmap) + draw_text(c("West", "Midwest", "East Coast"), c(0.2, 0.52, 0.85), 0.98)
ggsave("../outputs/figures/fig_1_map.pdf", annotation_plot, width = 8, height = 5)
ggsave("../outputs/figures/fig_1_map.jpg", annotation_plot, width = 8, height = 5)
