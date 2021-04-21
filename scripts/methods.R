## ----lythrum-read-in-data, echo=FALSE-----------------
PhenolAllData<-read.csv("outputs/PhenolAllData.csv")


## ----results-data, echo=F-----------------------------
options(knitr.graphics.auto_pdf = TRUE)
PhenolAllData<-read.csv("outputs/PhenolAllData.csv")
GeoStns<-read.csv("outputs/GeoStns.csv")

GeoStns <- dplyr::left_join(GeoStns, PhenolAllData[,c("Pop_Code", "Year")], by = c("Pop_Code" = "Pop_Code"))
UniqueStns<-unique(GeoStns[, c("StationID", "Year")])


## ---- echo = F, warning = F, message = F--------------
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))
suppressWarnings(library(broom))
library(pander)
library(tibble)
library(rsample)


## ----bootstrap-lm, echo=F, warning = F, message=F-----

## bootstrap fti is not standardized yet
AllData <- read.csv("outputs/bootstrap_validation.csv", stringsAsFactors = F)

# AllData <- AllData %>% group_by(Source, Latitude) %>% mutate(fti = (fti - mean(fti, na.rm = TRUE)) / sd(fti, na.rm = TRUE)) %>% as.data.frame()

fit_lm <- function(splits){
  df <- analysis(splits)
  mean_df <- df %>% group_by(Latitude) %>% summarize(mean_fti = mean(fti)) %>% mutate(mean_fti = (mean_fti - mean(mean_fti, na.rm = TRUE)) / sd(mean_fti, na.rm = TRUE)) %>% as.data.frame() 
  lm(mean_fti ~ Latitude, data = mean_df)
}

set.seed(2020)

bef_bootstrap_reps <- bootstraps(AllData[AllData$Source == "BEF",], times = 1000)

ksr_bootstrap_reps <- bootstraps(AllData[AllData$Source == "KSR",], times = 1000)

tim_bootstrap_reps <- bootstraps(AllData[AllData$Source == "Timmins",], times = 1000)

herb_bootstrap_reps <- bootstraps(AllData[AllData$Source == "Herbarium",], times = 1000)


## ----fit-lm, echo = F---------------------------------

bef_lm <- bef_bootstrap_reps %>% 
  mutate(model = purrr::map(splits, fit_lm),
         coef_info = purrr::map(model, tidy)) %>%
  unnest(coef_info) %>% mutate(Source = "BEF")

ksr_lm <- ksr_bootstrap_reps %>% 
  mutate(model = purrr::map(splits, fit_lm),
         coef_info = purrr::map(model, tidy)) %>%
  unnest(coef_info) %>% mutate(Source = "KSR")

tim_lm <- tim_bootstrap_reps %>% 
  mutate(model = purrr::map(splits, fit_lm),
         coef_info = purrr::map(model, tidy)) %>%
  unnest(coef_info) %>% mutate(Source = "Timmins")


herb_lm <- herb_bootstrap_reps %>% 
  mutate(model = purrr::map(splits, fit_lm),
         coef_info = purrr::map(model, tidy)) %>%
  unnest(coef_info) %>% mutate(Source = "Herbarium")

coefs_df <- dplyr::bind_rows(herb_lm, tim_lm, ksr_lm, bef_lm)


source_slopes <- coefs_df %>% filter(term == "Latitude") %>%
  select(estimate, std.error, Source) %>%
  group_by(Source) %>% 
  summarize(mean_source = mean(estimate),
            sd_source = sd(estimate),
            lower_ci = quantile(estimate, 0.025),
            higher_ci = quantile(estimate, 0.975))
  


## ----fit-cor------------------------------------------
fit_cor <- function(splits){
  df <- analysis(splits)
  mean_df <- df %>% group_by(Latitude) %>% summarize(mean_fti = mean(fti)) %>% 
    mutate(mean_fti = (mean_fti - mean(mean_fti, na.rm = TRUE)) /sd(mean_fti, na.rm = TRUE)) %>% as.data.frame()
  cor.test(mean_df$mean_fti, mean_df$Latitude)
}

bef_cor <- bef_bootstrap_reps %>% 
  mutate(model = purrr::map(splits, fit_cor),
         coef_info = purrr::map(model, tidy)) %>%
  unnest(coef_info) %>% mutate(Source = "BEF")

ksr_cor <- ksr_bootstrap_reps %>% 
  mutate(model = purrr::map(splits, fit_cor),
         coef_info = purrr::map(model, tidy)) %>%
  unnest(coef_info) %>% mutate(Source = "KSR")

tim_cor <- tim_bootstrap_reps %>% 
  mutate(model = purrr::map(splits, fit_cor),
         coef_info = purrr::map(model, tidy)) %>%
  unnest(coef_info) %>% mutate(Source = "Timmins")

herb_cor <- herb_bootstrap_reps %>% 
  mutate(model = purrr::map(splits, fit_cor),
         coef_info = purrr::map(model, tidy)) %>%
  unnest(coef_info) %>% mutate(Source = "Herbarium")

coefs_df <- dplyr::bind_rows(herb_cor, tim_cor, ksr_cor, bef_cor)


cor_source_slopes <- coefs_df %>%
  select(estimate, Source) %>%
  group_by(Source) %>% 
  summarize(mean_source = mean(estimate),
            sd_source = sd(estimate),
            lower_ci = quantile(estimate, 0.025),
            higher_ci = quantile(estimate, 0.975))


## ---- echo = F----------------------------------------

PhenolAllData<-read.csv("outputs/PhenolAllData.csv", stringsAsFactors = F)

PhenolAllData <- PhenolAllData[, c("Pop_Code", "fti", "GD", "Region")]

PhenolAllData$bin_GD<- round(PhenolAllData$GD, digits = -1)

fit_cor_gd <- function(splits){
  df <- analysis(splits)
  mean_df <- df %>% group_by(bin_GD) %>% summarize(mean_fti = mean(fti)) %>% 
    mutate(mean_fti = (mean_fti - mean(mean_fti, na.rm = TRUE)) /sd(mean_fti, na.rm = TRUE)) %>% as.data.frame()
  cor.test(mean_df$mean_fti, mean_df$bin_GD)
}

east_coast <- bootstraps(PhenolAllData[PhenolAllData$Region == "EastCoast",], times = 1000)

east_coast_cor <- east_coast %>% 
  mutate(model = purrr::map(splits, fit_cor_gd),
         coef_info = purrr::map(model, tidy)) %>%
  unnest(coef_info) %>% mutate(Source = "EastCoast")

midwest <- bootstraps(PhenolAllData[PhenolAllData$Region == "Midwest",], times = 1000)

midwest_cor <- midwest %>% 
  mutate(model = purrr::map(splits, fit_cor_gd),
         coef_info = purrr::map(model, tidy)) %>%
  unnest(coef_info) %>% mutate(Source = "Midwest")

west <- bootstraps(PhenolAllData[PhenolAllData$Region == "West",], times = 1000)

west_cor <- west %>% 
  mutate(model = purrr::map(splits, fit_cor_gd),
         coef_info = purrr::map(model, tidy)) %>%
  unnest(coef_info) %>% mutate(Source = "West")

region_cor_df <- dplyr::bind_rows(east_coast_cor, midwest_cor, west_cor)


region_cors <- region_cor_df %>%
  select(estimate, Source) %>%
  group_by(Source) %>% 
  summarize(mean_region = mean(estimate),
            sd_region = sd(estimate),
            lower_ci = quantile(estimate, 0.025),
            higher_ci = quantile(estimate, 0.975))



## ---- echo = F, warning=F, message=F, warning=F-------
suppressWarnings(library(dplyr))
suppressWarnings(library(tidyr))
suppressWarnings(library(ggplot2))
library(purrr)
library(broom)

## binning
binx<-0.5 ## long bin size in degrees
biny<-0.5 ## lat bin size in degrees

PhenolAllData <- read.csv("outputs/PhenolAllData.csv")

fits <- nls(phind~a^exp(-exp(1)*(b + c * time + d * GD + e * GD * time) * GDDs), data=PhenolAllData, start=list(a = 0.1, b=0.0001, c = 0.00001, d = 0.000001, e = 0.0000001))

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



## ---- echo = F----------------------------------------
extract_coefficient <- function(x, coefficient){
  x %>% select(age_range, tidy_model) %>% unnest(tidy_model) %>% select(age_range, term, estimate, std.error) %>% filter(term == coefficient)
}

b_coef <- map_df(coef_list, extract_coefficient, "b")

b_coef$age <- recode_factor(b_coef$age_range, "[0,30]" = 15, "(30,60]" = 45, "(60,90]" = 75, "(90,120]" = 105, "(120,150]" = 135, "(150,250]" = 200)

b_coef$age <- as.numeric(as.character(b_coef$age))
b_coef$age2 <- b_coef$age^2

linear_b <- lm(estimate ~ age, data = b_coef)

quadratic_b <- lm(estimate ~age + age2, data = b_coef)

d_coef <- map_df(coef_list, extract_coefficient, "d")

d_coef$age <- recode_factor(d_coef$age_range, "[0,30]" = 15, "(30,60]" = 45, "(60,90]" = 75, "(90,120]" = 105, "(120,150]" = 135, "(150,250]" = 200)

d_coef$age <- as.numeric(as.character(d_coef$age))
d_coef$age2 <- d_coef$age^2

linear_d <- lm(estimate ~ age, data = d_coef)
quadratic_d <- lm(estimate ~age + age2, data = d_coef)


## ----contingency-table, echo=FALSE--------------------
PhenolAllData <- read.csv("outputs/PhenolAllData.csv")
knitr::kable(table(PhenolAllData$Region, PhenolAllData$Era), caption="Summary of herbarium specimens in the data set according to geographic region and time of sampling.")


## ----bootstrap-slope, echo=F--------------------------
knitr::kable(source_slopes, caption = "(ref:slope-table-ref)", col.names = c("Source","Mean", "Standard Deviation", "95% Lower Bound", "95% Upper Bound"), digits = 4)


## ----bootstrap-cor, echo=F----------------------------
knitr::kable(cor_source_slopes, caption = "(ref:cor-table-ref)", col.names = c("Source","Mean", "Standard Deviation", "95% Lower Bound", "95% Upper Bound"), digits = 4)


## ---- echo=F, warning = F, include = F----------------

library(lme4)

mod1 <- lme4::lmer(fti ~ Latitude + Source + (1|Pop_Code), data = AllData)
mod2 <- lme4::lmer(fti ~ Latitude + Source + Latitude*Source + (1|Pop_Code), data = AllData)

anova(mod1, mod2)


## ----region-cor, echo=F-------------------------------
knitr::kable(region_cors, caption = "(ref:cor-region-ref)", col.names = c("Source","Mean", "Standard Deviation", "95% Lower Bound", "95% Upper Bound"), digits = 4)


## ----full-nls, echo=F---------------------------------
load("outputs/full_pheno_mod_coefs.RData")

bootstrap_coef_df <- full_coef_df %>%
  select(term, estimate) %>%
  group_by(term) %>% 
  summarize(mean_estimate = mean(estimate),
            sd = sd(estimate),
            lower_ci = quantile(estimate, 0.025),
            higher_ci = quantile(estimate, 0.975))

bootstrap_coef_df <- as.data.frame(bootstrap_coef_df)

bootstrap_coef_df [, 2:5] <- format(bootstrap_coef_df [, 2:5], digits = 3, scientific = TRUE)

knitr::kable(bootstrap_coef_df, caption = "Estimated coefficients of the nonlinear least squares model predicting phenological growth (see Equation S1). ", digits = 4)


## ----linear-b, echo = F-------------------------------
pander(linear_b, caption = "(\\#tab:linear-b) Estimated model coefficients for the linear model estimating the effect of age on coefficient b.", missing = "", digits = 3)
# summary(linear_b)


## ----quadratic-b, echo = F----------------------------
pander(quadratic_b, caption = "(\\#tab:quadratic-b) Estimated model coefficients for the quadratic model estimating the effect of age on coefficient b.", missing = "")
# summary(quadratic_b)


## ----anova-b, echo = F--------------------------------
pander(anova(linear_b, quadratic_b), caption = "(\\#tab:anova-b) ANOVA comparing linear and quadratic models estimating the effect of age on coefficient b.", missing = "")


## ----linear-d, echo = F-------------------------------
pander(linear_d, caption = "(\\#tab:linear-d) Estimated model coefficients for the linear model estimating the effect of age on coefficient d.", missing = "")
# summary(linear_d)


## ----quadratic-d, echo = F----------------------------
pander(quadratic_d, caption = " (\\#tab:quadratic-d) Estimated model coefficients for the quadratic model estimating the effect of age on coefficient d.", missing = "")
# summary(quadratic_d)


## ----anova-d, echo = F--------------------------------
pander(anova(linear_d, quadratic_d), caption = "(\\#tab:anova-d) ANOVA comparing linear and quadratic models estimating the effect of age on coefficient d.", missing = "")
# anova(linear_d, quadratic_d)

