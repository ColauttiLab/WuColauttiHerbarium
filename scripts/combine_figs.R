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
#continental<-map_data("worldHires", c("USA", "Canada"))
continental <- readOGR("small", "North America")

library(cowplot)
library(ggpolypath)
# with annotation, and no region coloring

Regionmap <- ggplot() +
  geom_polypath(data=continental, aes(x=long, y=lat, group=group), fill = "white", color = "black") +
  coord_cartesian(ylim=c(32,54), xlim=c(-129,-52)) +
  # geom_rect(aes(xmin=-76, xmax=Inf, ymin=-Inf, ymax=Inf), fill="#F8766D", alpha=0.4) +
  # geom_rect(aes(xmin=-76, xmax=-101, ymin=-Inf, ymax=Inf), fill="#7CAE00", alpha=0.4) +
  # geom_rect(aes(xmin=-101, xmax=-Inf, ymin=-Inf, ymax=Inf), fill="#00BFC4", alpha=0.4) +
  # geom_vline(xintercept = -122, size = 1) +
  # geom_vline(xintercept = -101, size = 1) +
  # geom_vline(xintercept = -76, size = 1) +
  geom_point(data=UniqueStns, aes(x=Longitude, y=Latitude), color="gray50", alpha=0.3, size = 1 ) +
  geom_point(data= PhenolAllData, aes(x=Longitude, y=Latitude), color="#CC33FF", alpha=0.6, size=0.5) + 
  theme_classic() + 
  theme(panel.background = element_rect(fill = "#BFEEF0", color = "black")) + 
  # theme( axis.title.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank()) + 
  # scale_y_continuous(breaks = NULL, labels = NULL) + 
  scale_y_continuous(name = "Latitude") + 
  scale_x_continuous(name = "Longitude",breaks = c(-76, -101), sec.axis = dup_axis(name = NULL)) 


annotation_plot <- ggdraw(Regionmap) + draw_text(c("West", "Midwest", "East Coast"), c(0.23, 0.53, 0.84), 0.96, size = 12)

cleantheme <- theme_bw() + 
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), strip.background =element_rect(fill="white")) +
  theme(axis.line = element_line(color = 'black'))


library(dplyr)
library(purrr)
library(broom)
library(tidyr)
library(ggplot2)
## binning
binx<-0.5 ## long bin size in degrees
biny<-0.5 ## lat bin size in degrees

PhenolAllData <- read.csv("../outputs/PhenolAllData.csv")
longs<-seq(min(PhenolAllData$Longitude,na.rm=T)+binx,max(PhenolAllData$Longitude,na.rm=T)-binx,by=binx*2)
lats<-seq(min(PhenolAllData$Latitude,na.rm=T)+biny,max(PhenolAllData$Latitude,na.rm=T)-biny,by=biny*2)

# make bins for grids
lat_groups <- cut(PhenolAllData$Latitude, breaks = seq(floor(min(PhenolAllData$Latitude)), 
                                                       ceiling(max(PhenolAllData$Latitude)),
                                                       by = 0.1))

long_groups <- cut(PhenolAllData$Longitude, breaks = seq(floor(min(PhenolAllData$Longitude)), ceiling(max(PhenolAllData$Longitude)), 0.1))

PhenolAllData$age_range <- cut(PhenolAllData$time, c(0, 30, 60, 90, 120, 150, 250), include.lowest = T)

# tidy grid has points grouped into bins based on lat long
tidy_grid <- PhenolAllData %>% 
  group_by(long_gr = long_groups, lat_gr = lat_groups) 


sample_grid <- function(x, df){
  df %>% sample_n(1) %>% ungroup() %>% as.data.frame()
}

set.seed(1001)

list_df <- lapply(1:1000, sample_grid, df = tidy_grid)

tidy_nls <- function(x){
  nls(phind~a^exp(-exp(1)*(b + d * GD ) * GDDs), data=x, start=list(a = 0.3, b=0.00001,  d = 0.00000001))
}

map_nls <- function(x){
  as_tibble(x) %>% 
    group_by(age_range) %>%
    nest() %>% 
    mutate(
      nls_model = map(data, tidy_nls),
      tidy_model = map(nls_model, tidy),
      glance_model = map(nls_model, glance),
      augment_model = map(nls_model, augment)
    )
}

coef_list <- map(list_df, map_nls)

extract_coefficient <- function(x, coefficient){
  x %>% select(age_range, tidy_model) %>% unnest(tidy_model) %>% select(age_range, term, estimate, std.error) %>% filter(term == coefficient)
}

b_coef <- map_df(coef_list, extract_coefficient, "b")

bootstrap_estimates <- b_coef %>% group_by(age_range) %>%
  summarize(mean_b = mean(estimate),
            sd_b = sd(estimate),
            lower_ci = quantile(estimate, 0.025),
            higher_ci = quantile(estimate, 0.975))

b_bootstrap_plot <- ggplot(bootstrap_estimates, aes(x = age_range, y = mean_b)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = higher_ci)) +
  theme_classic() +
  labs(x = "Time Since Invasion", y = "Estimated coefficient of b")

# bootstrap_plot

d_coef <- map_df(coef_list, extract_coefficient, "d")

bootstrap_estimates <- d_coef %>% group_by(age_range) %>%
  summarize(mean_b = mean(estimate),
            sd_b = sd(estimate),
            lower_ci = quantile(estimate, 0.025),
            higher_ci = quantile(estimate, 0.975))

d_bootstrap_plot <- ggplot(bootstrap_estimates, aes(x = age_range, y = mean_b)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = higher_ci)) +
  theme_classic() +
  labs(x = "Time Since Invasion", y = "Estimated coefficient of d")

maxGD<-PhenolAllData$GD[order(PhenolAllData$GD)][floor(nrow(PhenolAllData)*0.95)]
minGD<-PhenolAllData$GD[order(PhenolAllData$GD)][floor(nrow(PhenolAllData)*0.05)]
midGD<-PhenolAllData$GD[order(PhenolAllData$GD)][floor(nrow(PhenolAllData)*0.5)]


maxtime<-PhenolAllData$time[order(PhenolAllData$time)][floor(nrow(PhenolAllData)*0.95)]
mintime<-PhenolAllData$time[order(PhenolAllData$time)][floor(nrow(PhenolAllData)*0.05)]
midtime<-median(PhenolAllData$time)

GD<-rep(c(minGD, midGD, maxGD),3)
time<-rep(c(maxtime, midtime, mintime), c(3,3,3))
## make data frame with all combinations of the values
variabledf<-data.frame(GD, time)
variabledf$cat_GD<-rep(c("L", "M", "H"), 3)
variabledf$cat_time<-rep(c("H", "M", "L"), each=3)

extract_all_coefs <- function(x){
  x %>% select(age_range, tidy_model) %>% unnest(tidy_model) %>% select(age_range, term, estimate, std.error)
}

coef_df <-  map_df(coef_list, extract_all_coefs)

median_coefs <-  coef_df %>% 
  group_by(age_range, term) %>% 
  summarize(
    median_coef = median(estimate)
  )

median_coefs <- median_coefs %>% pivot_wider(names_from = term, values_from = median_coef)

draw_nlscurve<- function(a, b, d, GD, x){
  a^exp(-exp(1)*(b + d * GD ) * x)
}

variableGD <- data.frame(GD = quantile(PhenolAllData$GD, c(0.1, 0.3, 0.5, 0.7, 0.9)), 
                         a = 0.001,
                         b = mean(median_coefs$b),
                         d = mean(median_coefs$d))

color_time <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=5))

gd_graph <- variableGD %>% mutate(season_length = GD) %>%
  crossing(x = seq(0, 3000, 1)) %>% 
  mutate(y = draw_nlscurve(a, b, d, GD, x)) %>%
  ggplot(aes(x, y, color = as.factor(season_length))) +
  geom_line() + theme_classic() + 
  labs(x = "GDD", y = expression(varphi)) +
  # scale_color_discrete(name = "Season Length (Days)") +
  coord_cartesian(ylim = c(0, 1)) + 
  # labs(caption = "Varying Season Length") +
  scale_color_manual(values = color_time) + 
  theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5))


variable_d <- data.frame(GD = quantile(PhenolAllData$GD, 0.5),
                         a = 0.001,
                         b = mean(median_coefs$b),
                         d = seq(-0.000001, -0.000002, length.out = 5))


variable_d %>% 
  crossing(x = seq(0, 3000, 1)) %>% 
  mutate(y = draw_nlscurve(a, b, d, GD, x)) %>%
  ggplot(aes(x, y, color = as.factor(d))) +
  geom_line() + theme_classic() + labs(x = "GDD", y = expression(varphi)) +
  # scale_color_discrete(name = "Season Length (Days)") +
  coord_cartesian(ylim = c(0, 1)) + labs(caption = "Variable coefficient d") +
  scale_color_viridis_d(name = "Variable coefficient of growing season")

## fit linear vs quadratic model to coefs

b_coef$age <- recode_factor(b_coef$age_range, "[0,30]" = 15, "(30,60]" = 45, "(60,90]" = 75, "(90,120]" = 105, "(120,150]" = 135, "(150,250]" = 200)

b_coef$age <- as.numeric(as.character(b_coef$age))
b_coef$age2 <- b_coef$age^2

linear_b <- lm(estimate ~ age, data = b_coef)

test <- lm(estimate ~ age + I(age^2), data = b_coef)

quadratic_b <- lm(estimate ~age + age2, data = b_coef)

quad_draw <- function(int, age, age2,  x){
  int + age * x + age2 * x^2
}

b_coef_graph <- ggplot(data.frame(x=c(0, 200)), aes(x=x)) + 
  geom_point(aes(x = age, y = estimate), data = b_coef, alpha = 0.05, fill = NA) +
  # geom_abline(intercept = linear_b$coefficients[1], slope = linear_b$coefficients[2]) +
  stat_function(fun = quad_draw, 
                args = list(int = quadratic_b$coefficients[1], 
                            age = quadratic_b$coefficients[2], 
                            age2 = quadratic_b$coefficients[3])) +
  theme_classic() +
  scale_y_continuous(name = expression("Temperature Accumulation Coefficient (" *10^-4* ")" ), 
                     breaks = c(0, 0.0005, 0.0010, 0.0015), 
                     labels = 10000 * c(0, 0.0005, 0.0010, 0.0015), 
                     limits = c(0, 0.0015)) +
  labs(x = "Time Since Invasion") 



d_coef$age <- recode_factor(d_coef$age_range, "[0,30]" = 15, "(30,60]" = 45, "(60,90]" = 75, "(90,120]" = 105, "(120,150]" = 135, "(150,250]" = 200)

d_coef$age <- as.numeric(as.character(d_coef$age))
d_coef$age2 <- d_coef$age^2

linear_d <- lm(estimate ~ age, data = d_coef)
quadratic_d <- lm(estimate ~age + age2, data = d_coef)

d_coef_graph <- ggplot(data.frame(x=c(0, 200)), aes(x=x)) + 
  geom_point(aes(x = age, y = estimate), data = d_coef, alpha = 0.05) +
  # geom_abline(intercept = linear_d$coefficients[1], slope = linear_d$coefficients[2]) +
  stat_function(fun = quad_draw, args = list(int = quadratic_d$coefficients[1], age = quadratic_d$coefficients[2], age2 = quadratic_d$coefficients[3])) +
  geom_hline(yintercept = 0) +
  theme_classic() +
  scale_y_continuous(name = expression("Season Length Coefficient (" *10^-6* ")"),
                     breaks = -0.000001 * c(0:4),
                     labels = 0:-4,
                     limits = c(-0.0000042, 0)) +
  labs(x = "Time Since Invasion")

bottom_row <- plot_grid(gd_graph, b_coef_graph, d_coef_graph, ncol = 3,
                        labels = c("A", "B", "C"), label_size = 12)

# combined_fig <- plot_grid(sliding_window, bottom_row, nrow = 2, ncol = 1)

# bottom_row <- plot_grid(combined_plot_caption, bootstrap_plot, legend, NULL, ncol = 2, nrow = 2, rel_widths = c(2, 1), rel_heights = c(1, 0.1), labels = c("C", "D", "", ""))

# combined_fig <- plot_grid(sliding_window, bottom_row, nrow = 2, ncol = 1, labels = c("A", ""), label_size = 12)

combined_fig <- plot_grid(gd_graph, b_coef_graph, d_coef_graph, ncol = 3,
                          labels = c("A", "B", "C"), label_size = 12)



save_plot("../outputs/figures/combined_figure.pdf", combined_fig, ncol = 1, nrow = 1, base_height = 3.71, base_asp = 2.1)
save_plot("../outputs/figures/combined_figure.jpg", combined_fig, ncol = 1, nrow = 1, base_height = 3.71, base_asp = 2.1)

library(tidyverse)
library(gridExtra)
# Some custom graphing stuff
source("./theme_pub.R")
theme_set(theme_pub() + theme(axis.text.x = element_blank(),
                               axis.text.y = element_blank(),
                               axis.ticks = element_blank(),
                               axis.title.y = element_text(angle = 0)))


X<-c(-250:600)/50
Y<-dnorm(X)
pDat<-data.frame(X=X,Y=Y)
#ggplot(aes(x=X,y=Y),data=pDat) + geom_line(width=4)
#ggplot(aes(x=X,y=Y),data=pDat) + geom_line(linetype="dashed",width=4)
#ggplot(aes(x=X,y=Y),data=pDat) + geom_line(linetype="dotted",width=4)

# Continuous evolution
CEMpop<-ggplot(aes(x=X,y=Y),data=pDat) +
  geom_line(size=1.2,alpha=0.5) +
  geom_line(aes(y=dnorm(X,sd=3)*10),linetype="dotted",size=1.2) +
  geom_line(aes(y=dnorm(X-2)),size=1.2,colour="blue",alpha=0.5) +
  geom_line(aes(y=dnorm(X-4)),size=1.2,colour="purple",alpha=0.5) +
  geom_line(aes(y=dnorm(X-6)),size=1.2,colour="violet",alpha=0.5) +
  geom_line(aes(y=dnorm(X-8)),size=1.2,colour="red") +
  geom_line(aes(y=dnorm(X-10,sd=3)*10),linetype="dashed",size=1.2,colour="red") +
  xlab("Trait Value (z)") + ylab("w")

CEMcline<-ggplot(aes(x=X,y=X),data=pDat) +
  geom_line(aes(y=0),size=1.2) +
  geom_line(aes(y=0.2*X),size=1.2,colour="blue",alpha=0.5) +
  geom_line(aes(y=0.4*X),size=1.2,colour="purple",alpha=0.5) +
  geom_line(aes(y=0.6*X),size=1.2,colour="violet",alpha=0.5) +
  geom_line(aes(y=0.8*X),size=1.2,colour="red") +
  xlab("Environmental Gradient") + ylab("z")
 


# Punctuated evolution
PEMpop<-ggplot(aes(x=X,y=Y),data=pDat) +
  geom_line(size=1.2,alpha=0.5) +
  geom_line(aes(y=dnorm(X,sd=3)*10),linetype="dotted",size=1.2) +
  geom_line(aes(y=dnorm(X-4,sd=0.9)),size=1.2,colour="blue",alpha=0.5) +
  geom_line(aes(y=dnorm(X-6,sd=0.8)),size=1.2,colour="purple",alpha=0.5) +
  geom_line(aes(y=dnorm(X-7,sd=0.7)),size=1.2,colour="violet",alpha=0.5) +
  geom_line(aes(y=dnorm(X-8,sd=0.6)),size=1.2,colour="red") +
  geom_line(aes(y=dnorm(X-10,sd=3)*10),linetype="dashed",size=1.2,colour="red") +
  xlab("Trait Value (z)") + ylab("w")

PEMcline<-ggplot(aes(x=X,y=X),data=pDat) +
  geom_line(aes(y=0),size=1.2) +
  geom_line(aes(y=0.4*X),size=1.2,colour="blue",alpha=0.5) +
  geom_line(aes(y=0.6*X),size=1.2,colour="purple",alpha=0.5) +
  geom_line(aes(y=0.7*X),size=1.2,colour="violet",alpha=0.5) +
  geom_line(aes(y=0.8*X),size=1.2,colour="red") +
  xlab("Environmental Gradient") + ylab("z")

grid.arrange(CEMpop, CEMcline, PEMpop, PEMcline, nrow = 2)

pdf("./Fig1_CEM_GEM.pdf",height=6,width=12)
  grid.arrange(CEMpop, CEMcline, PEMpop, PEMcline, nrow = 2)
dev.off()


