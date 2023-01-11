
library(tidyverse)
library(EconGeo)

library(ggpubr)
library(ggsci)

# ---- data ----

setwd("Data/Patents/OECD_REGPAT_202208/")
dir()

IPC <- read.csv("202208_EPO_IPC.txt", sep = "|")
APP <- read.csv("202208_EPO_App_reg.txt", sep = "|")
INV <- read.csv("202208_EPO_Inv_reg.txt", sep = "|")

# European Union (EU) member states
ctry_EU_long <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                  "Czech Rep.","Denmark","Estonia","Finland","France",
                  "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                  "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                  "Portugal","Romania","Slovakia","Slovenia","Spain",
                  "Sweden","United Kingdom")
ctry_EU_short <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
                   "DE", "EL", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
                   "PL", "PT", "RO", "SK", "SI", "ES", "SE", "UK")
# European Free Trade Association (EFTA) member states
ctry_EFTA_long <- c("Iceland", "Norway", "Liechtenstein", "Switzerland")
ctry_EFTA_short <- c("IS", "NO", "LI", "CH")

# binding data and creating data frame for countries
ctry_long <- c(ctry_EU_long, ctry_EFTA_long)
ctry_short <- c(ctry_EU_short, ctry_EFTA_short)
ctry <- as.data.frame(cbind(ctry_short, ctry_long))

# filter EU & EFTA member states
IPC_NUTS <- IPC %>%
  left_join(APP) %>%
  filter(ctry_code %in% ctry$ctry_short)

# amount of technological classes
length(unique(substr(IPC$IPC, 1, 4)))
# set IPC4 to four-digit Cooperative Patent Classification (CPC)
IPC$IPC4 <- substr(IPC$IPC, 1, 4)

# amount of regions
length(unique(substr(APP$reg_code, 1, 4)))
# set NUTS2 regions to four-digit Nomenclature of Territorial Units for Statistics
APP$NUTS2 <- substr(APP$reg_code, 1, 4)

# list of NUTS2 regions
NUTS_list <- read.csv("../../NUTS_2013.csv", sep = ",") %>%
  select(NUTS2 = NUTS.Code)
NUTS_list <- NUTS_list[nchar(NUTS_list$NUTS2) == 4, ]

IPC4_NUTS2_2014_2018 <- IPC %>%
  filter(prio_year >= 2014 & prio_year <= 2018) %>%
  left_join(APP) %>%
  filter(NUTS2 %in% NUTS_list) %>%
  select(NUTS2, IPC4) %>%
  mutate(count = 1) %>%
  filter(IPC4 != "")

IPC4_NUTS2_2014_2018_count <- IPC4_NUTS2_2014_2018 %>%
  group_by(NUTS2) %>%
  count(IPC4) %>%
  filter(IPC4 != "")

# amount of technological classes
length(unique(IPC4_NUTS2_2014_2018_count$IPC4))
# amount of regions
length(unique(IPC4_NUTS2_2014_2018_count$NUTS2))

ggplot(IPC4_NUTS2_2014_2018_count, aes(x=n)) + geom_density() + scale_x_log10() +
  theme_pubr(base_size = 18, base_family = "Georgia") +
  ylab("Density\n") + xlab("\nNumber of Patents per Technological Field per Region")

IPC4_NUTS2_2014_2018_count %>%
  arrange(-n)

IPC_year <- IPC %>%
  left_join(APP) %>%
  filter(NUTS2 %in% NUTS_list) %>%
  group_by(prio_year) %>%
  summarise(freq = n()) %>%
  filter(prio_year < 2025)

# ---- Matrix ----

mat_count_2014_2018 <- get.matrix(IPC4_NUTS2_2014_2018)
mat_entry_2014_2018 <- location.quotient(mat_count_2014_2018, binary = TRUE)







