
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

setwd("../Data/Patents/OECD_REGPAT_202208/")
dir()

IPC <- read.csv("202208_EPO_IPC.txt", sep="|")
APP <- read.csv("202208_EPO_App_reg.txt", sep="|")
INV <- read.csv("202208_EPO_Inv_reg.txt", sep="|")

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

# ---- data set ----

IPC4_NUTS2_1994_2018 <- IPC %>%
  filter(prio_year >= 1994 & prio_year <= 2018) %>%
  left_join(APP) %>%
  filter(NUTS2 %in% geo_data$geo) %>%
  filter(IPC4 != "") %>%
  mutate(period = case_when(prio_year >= 1994 & prio_year <= 1998 ~ 1,
                            prio_year >= 1999 & prio_year <= 2003 ~ 2,
                            prio_year >= 2004 & prio_year <= 2008 ~ 3,
                            prio_year >= 2009 & prio_year <= 2013 ~ 4,
                            prio_year >= 2014 & prio_year <= 2018 ~ 5,))

# ---- time analysis ----

IPC4_NUTS2_1994_2018 %>%
  count(period)

# ---- region analysis ----

IPC4_NUTS2_1994_2018 %>%
  group_by(NUTS2, period) %>%
  count() %>%
  ggplot(aes(x=n+1)) + geom_histogram(bins=50, colour="white") +
  scale_y_log10() +
  theme_pubr(base_size = 18, base_family = "Georgia")

IPC4_NUTS2_1994_2018 %>%
  group_by(NUTS2, period) %>%
  count() %>%
  ggplot(aes(y=n)) + geom_boxplot() +
  theme_pubr(base_size = 18, base_family = "Georgia")

IPC4_NUTS2_1994_2018 %>%
  group_by(NUTS2, period) %>%
  count() %>%
  arrange(n) %>%
  print(n=25)

# ---- technological field analysis

IPC4_NUTS2_1994_2018 %>%
  group_by(IPC4, period) %>%
  count() %>%
  ggplot(aes(x=n+1)) + geom_histogram(bins=50, colour="white") +
  scale_y_log10() +
  theme_pubr(base_size = 18, base_family = "Georgia")

IPC4_NUTS2_1994_2018 %>%
  group_by(IPC4, period) %>%
  count() %>%
  ggplot(aes(y=n)) + geom_boxplot() +
  theme_pubr(base_size = 18, base_family = "Georgia")

IPC4_NUTS2_1994_2018 %>%
  group_by(IPC4, period) %>%
  count() %>%
  arrange(n) %>%
  print(n=25)

# ---- conjunction analysis ----

IPC4_NUTS2_1994_2018 %>%
  group_by(IPC4, NUTS2, period) %>%
  count() %>%
  ggplot(aes(y=n)) + geom_boxplot() +
  scale_y_log10() +
  theme_pubr(base_size = 18, base_family = "Georgia")



