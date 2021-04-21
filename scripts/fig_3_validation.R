Source<-c("BEF", "Herbarium", "KSR","Timmins")
cols <- c("BEF" = "#F8766D","Herbarium" = "#00BF7D", "KSR" = "#E76BF3","Timmins" = "#00B0F6")
shape<-c("BEF"= 24, "Herbarium"=21, "KSR"=24, "Timmins"=24)
alphas<-c("BEF"= 0.5, "Herbarium"=0.5, "KSR"=0.5, "Timmins"=0.5)
lines <- c("BEF"= "dotdash", "Herbarium"= "solid", "KSR"="dotdash", "Timmins"="dotdash")
legend <- data.frame(Source, shape, cols, alphas, lines, stringsAsFactors = F)


library(ggplot2)
library(dplyr)

## fti standardizing is done in validation_data_prep_2

AllData<-read.csv("../outputs/Validationdataset.csv", stringsAsFactors = F)
AllData <- AllData[AllData$Source %in% c("BEF", "KSR", "Timmins", "Herbarium"),]
AllData<-left_join(AllData, legend)
AllData$Source<-as.factor(AllData$Source)

Validation_plot <- ggplot(data = AllData, aes(x = as.numeric(Latitude), y = fti)) +
  geom_point(aes( color = Source, shape = Source, fill = Source), size=5, stroke = 1) +
  # stat_summary(data=AllData, fun.y="mean", aes(x=Latitude, y=fti, color=Source, shape = Source, fill = Source), size=5, geom="point", stroke = 1) +
  # stat_smooth(data = AllData, aes(x=Latitude, y=fti, color=Source, linetype = Source), size=1.5, method="lm", se = FALSE) +
  geom_smooth(method = "lm",aes(color=Source, linetype = Source), size = 1.5, se = FALSE) +
  scale_color_manual(values=cols) +
  scale_alpha_manual(values = alphas) +
  scale_shape_manual(values = shape) +
  scale_linetype_manual(values = lines) +
  scale_fill_manual(values = alpha(cols, alphas)) +
  theme_classic() +
  labs(x="Latitude", y=expression(paste("Standardized mean flowering time ( ",bar(psi), " )")))

Validation_plot


ggsave("../outputs/figures/fig_3_validation.pdf", Validation_plot)
ggsave("../outputs/figures/fig_3_validation.jpg", Validation_plot)

