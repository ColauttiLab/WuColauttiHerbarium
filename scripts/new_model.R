# create new flowering time index

library(dplyr)
library(purrr)
library(broom)
library(tidyr)
library(ggplot2)
## binning
binx<-0.5 ## long bin size in degrees
biny<-0.5 ## lat bin size in degrees

PhenolAllData <- read.csv("outputs/PhenolAllData.csv")
longs<-seq(min(PhenolAllData$Longitude,na.rm=T)+binx,max(PhenolAllData$Longitude,na.rm=T)-binx,by=binx*2)
lats<-seq(min(PhenolAllData$Latitude,na.rm=T)+biny,max(PhenolAllData$Latitude,na.rm=T)-biny,by=biny*2)

# make bins for grids
lat_groups <- cut(PhenolAllData$Latitude, breaks = seq(floor(min(PhenolAllData$Latitude)), 
                                                       ceiling(max(PhenolAllData$Latitude)),
                                                       by = 0.1))

long_groups <- cut(PhenolAllData$Longitude, breaks = seq(floor(min(PhenolAllData$Longitude)), ceiling(max(PhenolAllData$Longitude)), 0.1))

# tidy grid has points grouped into bins based on lat long
tidy_grid <- PhenolAllData %>% 
  group_by(long_gr = long_groups, lat_gr = lat_groups) %>%
  select(time, GD, GDDs, phind, Pop_Code, Year, minYear)


sample_grid <- function(x, df){
  df %>% sample_n(1) %>% ungroup() %>% as.data.frame()
}

set.seed(1001)

list_df <- lapply(1:1000, sample_grid, df = tidy_grid)

tib_list_df <- tibble(group = 1:1000, data = list_df)


library(progress)

pb <- progress_bar$new(total = 1000)

tidy_full_nls <- function(x){

  model <- nls.multstart::nls_multstart(
    phind~a^exp(-exp(1)*(b + c * time + d * GD + e * GD * time) * GDDs),
    data=x,
    iter = 100,
    start_upper = c(a = 0.3, b=0.0001, c = 0.00001, d = 0.000001, e = 0.0000001),
    start_lower = c(a = 0.001, b=0.000001, c = 0.0000001, d = 0.0000001, e = 0.00000001))
  pb$tick()
  tidy(model)
}

coef_list <- tib_list_df %>% mutate(tidy_model = map(data, tidy_full_nls))

a_coef <- coef_list %>% unnest(tidy_model) %>% select(term, estimate, std.error) %>% filter(term == "a")

b_coef <- coef_list %>% unnest(tidy_model) %>% select(term, estimate, std.error) %>% filter(term == "b")

c_coef <- coef_list %>% unnest(tidy_model) %>% select(term, estimate, std.error) %>% filter(term == "c")

d_coef <- coef_list %>% unnest(tidy_model) %>% select(term, estimate, std.error) %>% filter(term == "d")

e_coef <- coef_list %>% unnest(tidy_model) %>% select(term, estimate, std.error) %>% filter(term == "e")

full_coef_df <- rbind(a_coef, b_coef, c_coef, d_coef, e_coef)

save(full_coef_df, file = "outputs/full_pheno_mod_coefs.RData")

load("outputs/full_pheno_mod_coefs.RData")

library(ggplot2)

ggplot(full_coef_df) +
  geom_histogram(aes(x = estimate)) +
  facet_grid(.~term, scales = "free") + theme_classic() +
  theme(axis.text.x = element_text(angle = 90))

mean_a <- mean(a_coef$estimate)
mean_b <- mean(b_coef$estimate)
mean_c <- mean(c_coef$estimate)
mean_d <- mean(d_coef$estimate)
mean_e <- mean(e_coef$estimate)

PhenolAllData <- read.csv("outputs/PhenolAllData.csv")

PhenolAllData$phi_calc <- mean_a^exp(-exp(1)*(mean_b + mean_c * mean(PhenolAllData$time) + mean_d * mean(PhenolAllData$GD) + mean_e * mean(PhenolAllData$time) * mean(PhenolAllData$GD))* PhenolAllData$GDDs)

PhenolAllData$fti <- PhenolAllData$phi_calc - PhenolAllData$phind 

write.csv(PhenolAllData, "outputs/PhenolAllData.csv", row.names=F)
