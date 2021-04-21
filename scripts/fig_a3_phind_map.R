library(ggplot2)
library(mapdata)
library(rgdal)
library(ggpolypath)
library(cowplot)
PhenolAllData <- read.csv("../outputs/PhenolAllData.csv")

#continental<-map_data("worldHires", c("USA", "Canada"))
continental <- readOGR("small", "North America")
continental <- fortify(continental)
phindmap<- ggplot()+ 
  geom_polypath(data=continental, aes(x=long, y=lat, group=group),  fill = "white", color = "black") +
  coord_cartesian(ylim=c(32,54), xlim=c(-129,-52)) +
  geom_point(data=PhenolAllData, aes(x=Longitude, y=Latitude, color=phind), alpha=0.5) +
  #geom_rect(data=past, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="red", fill=NA)+
  scale_color_gradientn(colours=rainbow(7), name=expression(varphi)) +
  theme_classic() + theme(panel.background = element_rect(fill = "#BFEEF0", color = "black")) + 
  scale_y_continuous(name = "Latitude") + 
  scale_x_continuous(name = "Longitude",breaks = c(-76, -101), sec.axis = dup_axis(name = NULL))

ggsave("../outputs/figures/fig_a3_phind_map.pdf", phindmap, width = 8, height = 5)
ggsave("../outputs/figures/fig_a3_phind_map.jpg", phindmap, width = 8, height = 5)
