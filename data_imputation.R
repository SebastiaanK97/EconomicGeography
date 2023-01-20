
library(lattice)
library(mice)
library(tidyverse)
library(naniar)

# https://cran.r-project.org/web/packages/imputeTS/vignettes/imputeTS-Time-Series-Missing-Value-Imputation-in-R.pdf
# https://cran.r-project.org/web/packages/imputeTS/imputeTS.pdf
# https://www.kaggle.com/code/kmkarakaya/missing-data-and-time-series-prediction-by-prophet
# https://stats.stackexchange.com/questions/104565/how-to-use-auto-arima-to-impute-missing-values

# ---- data ----

# set working directory
setwd("/Users/sebastiaan/Desktop/Innovation Sciences/Thesis/Code/Missing Data")
setwd("/Users/sebastiaan/Desktop/Innovation Sciences/Thesis/Data/Regional")

# import data

GDP_capita <- read.csv("GDP_capita.csv", sep=",")
population_density <- read.csv("population_density.csv", sep=",")

GDP_capita <- GDP_capita %>% 
  select(geo, year = TIME_PERIOD, GDP = OBS_VALUE)
population_density <- population_density %>% 
  select(geo, year = TIME_PERIOD, PD = OBS_VALUE)

range(population_density$year)
range(GDP_capita$year)

geo <- unique(population_density$geo)
year <- c(1990:2020)

region <- as.data.frame(cbind(geo=rep(geo, each=length(year)), year)) %>%
  mutate(year=as.integer(year)) %>%
  left_join(population_density) %>%
  left_join(GDP_capita)

cor(log(region$PD), log(region$GDP), use="complete.obs")

# ---- missing data exploration ----

region %>% 
  arrange(year) %>%
  #filter(year >= 2000) %>%
  select(PD, GDP) %>%
  vis_miss()

gg_miss_upset(region)

ggplot(region, aes(x=PD, y=GDP)) + geom_miss_point() +
  scale_x_log10() + scale_y_log10()




