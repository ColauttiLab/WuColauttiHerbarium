library(ggplot2)

PhenolAllData <- read.csv("../outputs/PhenolAllData.csv")

GDs_hist <- ggplot(PhenolAllData) + 
  geom_histogram(aes(x=GDs), binwidth=5) + 
  theme_classic() + 
  labs(x=expression(italic("Days since start of season")), y = "Number of Specimens")

ggsave("../outputs/figures/fig_a4_GDs_hist.pdf", GDs_hist)
ggsave("../outputs/figures/fig_a4_GDs_hist.jpg", GDs_hist)
