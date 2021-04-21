library(ggplot2)

PhenolAllData <- read.csv("../outputs/PhenolAllData.csv")

collection_hist <- ggplot(PhenolAllData) + 
  geom_histogram(aes(x=Year), binwidth=5) + 
  theme_classic() + 
  labs(x=expression(italic("Year")), y = "Number of Specimens")

ggsave("../outputs/figures/fig_a7_collection_hist.pdf", collection_hist)
ggsave("../outputs/figures/fig_a7_collection_hist.jpg", collection_hist)
