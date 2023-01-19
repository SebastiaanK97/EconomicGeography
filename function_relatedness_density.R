
library(tidyverse)
# http://econ.geo.uu.nl/peeg/peeg1709.pdf
library(EconGeo)
# http://stavrakoudis.econ.uoi.gr/r-eurostat/drawing-maps-of-europe.html
library(eurostat)

library(ggpubr)
library(ggsci)
library(scales)

# ---- patent data  ----

setwd("../Data/Patents/OECD_REGPAT_202208/")
dir()

IPC <- read.csv("202208_EPO_IPC.txt", sep = "|")
APP <- read.csv("202208_EPO_App_reg.txt", sep = "|")
INV <- read.csv("202208_EPO_Inv_reg.txt", sep = "|")

# ---- relatedness density

technological_entry_period <- function_technological_entry_period(IPC=IPC, APP=APP, geo_data=geo_data)

