library(ggplot2)

PhenolAllData <- read.csv("../outputs/PhenolAllData.csv")

phind_hist <- ggplot(PhenolAllData) + 
  geom_histogram(aes(x=phind), binwidth=0.1) + 
  theme_classic() + 
  labs(x=expression("Phenological Stage"~varphi), y = "Number of Specimens")

ggsave("../outputs/figures/fig_a2_phind_hist.pdf", phind_hist)
ggsave("../outputs/figures/fig_a2_phind_hist.jpg", phind_hist)
