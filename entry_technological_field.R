
library(tidyverse)
library(EconGeo)
#http://stavrakoudis.econ.uoi.gr/r-eurostat/drawing-maps-of-europe.html
library(eurostat)

library(ggpubr)
library(ggsci)

# ---- patent data  ----

setwd("../Data/Patents/OECD_REGPAT_202208/")
dir()

IPC <- read.csv("202208_EPO_IPC.txt", sep = "|")
APP <- read.csv("202208_EPO_App_reg.txt", sep = "|")
INV <- read.csv("202208_EPO_Inv_reg.txt", sep = "|")

# ---- technological demarcation ----

# amount of technological classes
length(unique(substr(IPC$IPC, 1, 4)))
# set IPC4 to four-digit Cooperative Patent Classification (CPC)
IPC$IPC4 <- substr(IPC$IPC, 1, 4)

# ---- regional demarcation ----

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
colnames(ctry) <- c("short", "long")

# amount of regions
length(unique(substr(APP$reg_code, 1, 4)))
# set NUTS2 regions to four-digit Nomenclature of Territorial Units for Statistics
APP$NUTS2 <- substr(APP$reg_code, 1, 4)

geo_data <- get_eurostat_geospatial(output_class = "sf", resolution = "60", nuts_level = "2", year = "2013") %>%
  filter(CNTR_CODE %in% ctry$short)
# amount of regions EU + EFTA
length(unique(geo_data$geo))

geo_data_UK <- get_eurostat_geospatial(output_class = "sf", resolution = "60", nuts_level = "2", year = "2010") %>%
  filter(geo %in% c("UKI1", "UKI2"))

# replace regions United Kingdom
geo_data <- geo_data %>%
  bind_rows(geo_data_UK) %>%
  filter(!geo %in% c("UKI3", "UKI4", "UKI5", "UKI6", "UKI7"))
# rename Iceland NUTS2
geo_data[["geo"]][geo_data[["geo"]] == "IS00"] <- "IS01"
length(unique(geo_data$geo))

# ---- sample validation ----

IPC4_NUTS2_1994_2018 <- IPC %>%
  filter(prio_year >= 1994 & prio_year <= 2018) %>%
  left_join(APP) %>%
  filter(NUTS2 %in% geo_data$geo) %>%
  select(NUTS2, IPC4) %>%
  mutate(count = 1) %>%
  filter(IPC4 != "")

IPC4_NUTS2_1994_2018_count <- IPC4_NUTS2_1994_2018 %>%
  group_by(NUTS2) %>%
  count(IPC4)

# amount of technological classes
length(unique(IPC4_NUTS2_1994_2018$IPC4))
# amount of regions
length(unique(IPC4_NUTS2_1994_2018$NUTS2))
# amount of countries
length(unique(substr(IPC4_NUTS2_1994_2018$NUTS2, 1, 2)))
# missingness in islands of France (FRA1-5), Cueta in Spain (ES63)
setdiff(geo_data$geo, IPC4_NUTS2_1994_2018$NUTS2)

ggplot(IPC4_NUTS2_1994_2018_count, aes(x=n)) + geom_density(n=16) + scale_x_log10() +
  theme_pubr(base_size = 18, base_family = "Georgia") +
  ylab("Density\n") + xlab("\nNumber of Patents per Technological Field per Region")

# ---- data to matrix to entry ----

setwd("../../../EconomicGeography/")
getwd()
dir()
source("function_entry_period.R")
tecnological_entry_period <- function_tecnological_entry_period(IPC=IPC, APP=APP, geo_data=geo_data)

# descriptive output
mean(tecnological_entry_period$entry, na.rm=TRUE)
sd(tecnological_entry_period$entry, na.rm=TRUE)
sum(is.na(tecnological_entry_period$entry))

# ---- entry to map ----

tecnological_entry_period %>%
  group_by(region) %>%
  summarise(n=sum(entry, na.rm=TRUE)) %>%
  arrange(-n)




