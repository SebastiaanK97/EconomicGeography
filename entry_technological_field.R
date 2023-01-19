
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

# ---- patent data  ----

setwd("../../Data/Patents/OECD_REGPAT_202208/")
dir()

IPC <- read.csv("202208_EPO_IPC.txt", sep="|")
CPC <- read.csv("202208_CPC_Classes.txt", sep="|")
APP <- read.csv("202208_EPO_App_reg.txt", sep="|")
INV <- read.csv("202208_EPO_Inv_reg.txt", sep="|")

IPC_CPC <- IPC %>%
  left_join(CPC)

# ---- technological demarcation ----

# amount of technological classes
length(unique(substr(IPC_CPC$CPC_Class, 1, 4)))
# set IPC4 to four-digit Cooperative Patent Classification (CPC)
IPC_CPC$CPC4 <- substr(IPC_CPC$CPC_Class, 1, 4)

CPC_clean <- IPC_CPC %>%
  select(appln_id, prio_year, CPC4) %>%
  unique()

head(CPC_clean)

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
length(unique(substr(INV$reg_code, 1, 4)))
# set NUTS2 regions to four-digit Nomenclature of Territorial Units for Statistics
INV$NUTS2 <- substr(INV$reg_code, 1, 4)
length(unique(INV$reg_code))

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
geo_data$NUTS2 <- geo_data$geo
length(unique(geo_data$NUTS2))

# ---- sample validation ----

IPC4_NUTS2_1994_2018 <- IPC %>%
  filter(prio_year >= 1994 & prio_year <= 2018) %>%
  left_join(INV) %>%
  filter(NUTS2 %in% geo_data$geo) %>%
  select(NUTS2, IPC4) %>%
  mutate(count = 1) %>%
  filter(IPC4 != "")

IPC4_NUTS2_1994_2018 <- IPC %>%
  filter(prio_year >= 1994 & prio_year <= 2018) %>%
  left_join(INV) %>%
  filter(NUTS2 %in% geo_data$NUTS2) %>%
  right_join(geo_data) %>%
  select(NUTS2, IPC4) %>%
  mutate(count = 1) %>%
  filter(IPC4 != "")
  
length(unique(IPC4_NUTS2_1994_2018$NUTS2))

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
setdiff(geo_data$NUTS2, IPC4_NUTS2_1994_2018$NUTS2)

ggplot(IPC4_NUTS2_1994_2018_count, aes(x=n)) + geom_density(n=16) + scale_x_log10() +
  theme_pubr(base_size = 18, base_family = "Georgia") +
  ylab("Density\n") + xlab("\nNumber of Patents per Technological Field per Region")

# ---- data to matrix to entry ----

setwd("../../../EconomicGeography/Functions/")
getwd()
dir()
source("function_entry_period.R")
entry_period <- function_entry_period(IPC=IPC, APP=APP, geo_data=geo_data,
                                      end_year_input=2018, number_period_input=5, length_period_input=5)

# descriptive output
mean(entry_period$entry, na.rm=TRUE)
sd(entry_period$entry, na.rm=TRUE)
sum(is.na(entry_period$entry))

getwd()
write.csv(entry_period, "../Data/entry_period.csv", row.names=FALSE)
entry_period <- read.csv("../Data/entry_period.csv", sep=",")

# ---- data to matrix to relatedness density ----







