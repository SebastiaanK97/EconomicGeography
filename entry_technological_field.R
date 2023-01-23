
library(tidyverse)
# http://econ.geo.uu.nl/peeg/peeg1709.pdf
library(EconGeo)
# http://stavrakoudis.econ.uoi.gr/r-eurostat/drawing-maps-of-europe.html
library(eurostat)
library(sf)

library(ggpubr)
library(ggsci)
library(scales)

c("#E8B63C", "#93BA5A", "#27A581", "#1A7789", "#003764")
c("#27A581", "#93BA5A", "#E8B63C", "#DE8C3F", "#CF553A")
c("#CF553A", "#DE8C3F", "#E8B63C", "#93BA5A", "#27A581")

library(classInt)

# ---- import data ----

setwd("../Data/")
CPC4_NUTS2_1994_2018 <- read.csv("CPC4_NUTS2_1994_2018.csv", sep=",")
head(CPC4_NUTS2_1994_2018, 10)

# ---- data to matrix to entry ----

setwd("../")
setwd("../../EconomicGeography/Functions/")
getwd()
dir()
source("function_entry_period.R")
entry_period <- function_entry_period(df=CPC4_NUTS2_1994_2018, end_year_input=2018, number_period_input=5, length_period_input=5)

head(CPC4_NUTS2_1994_2018)

# descriptive output
summary(entry_period$entry, na.rm=TRUE)
sd(entry_period$entry, na.rm=TRUE)
sum(is.na(entry_period$entry))

length(unique(entry_period$region))
length(unique(entry_period$industry))

getwd()
write.csv(entry_period, "../Data/entry_period.csv", row.names=FALSE)
entry_period <- read.csv("../Data/entry_period.csv", sep=",")

# ---- data to matrix to relatedness density ----







