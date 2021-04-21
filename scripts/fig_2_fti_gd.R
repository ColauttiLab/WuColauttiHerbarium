library(ggplot2)


PhenolAllData <- read.csv("../outputs/PhenolAllData.csv")

PhenolAllData$Age_Group <-NULL
PhenolAllData$Age_Group[PhenolAllData$time < 25]<-"A: < 25"
PhenolAllData$Age_Group[PhenolAllData$time >= 25 & PhenolAllData$time <= 75]<-"B: 25 - 75"
PhenolAllData$Age_Group[PhenolAllData$time >= 75]<-"C: > 75"

PhenolAllData$bin_latitude <- round(PhenolAllData$Latitude)


# needed since facet removes the axis lines from the facets

cleantheme <- theme_bw() + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), strip.background =element_rect(fill="white")) +
  theme(axis.line = element_line(color = 'black'))


PhenolAllData$bin_GD<- round(PhenolAllData$GD, digits = -1)

fti_gd <- 
  ggplot(PhenolAllData, aes(x=bin_GD, y=fti, color=Region)) +
  # geom_point()+
  facet_grid(Region~Era) +
  stat_smooth(method = "lm", se = F, color = "black") +
  stat_summary(fun.y = "mean", aes(x = bin_GD, y = fti), geom = "point", alpha = 0.8) +
  cleantheme +
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  theme(axis.line = element_line(color = 'black'), legend.position = "none") +
  # geom_smooth(method="lm", size=1) + 
  labs(x = "Season Length", y=expression(paste(bar(psi))))

ggsave("../outputs/figures/fig_gd_lat.pdf", fti_gd)
ggsave("../outputs/figures/fig_gd_lat.jpg", fti_gd)


