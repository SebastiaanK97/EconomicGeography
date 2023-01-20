
library(eurostat)
library(tidyverse)

# ---- patent data  ----

setwd("../Data/Patents/OECD_REGPAT_202208/")
dir()

IPC <- read.csv("202208_EPO_IPC.txt", sep="|")
CPC <- read.csv("202208_CPC_Classes.txt", sep="|")
APP <- read.csv("202208_EPO_App_reg.txt", sep="|")
INV <- read.csv("202208_EPO_Inv_reg.txt", sep="|")

CPC_clean <- IPC %>%
  select(appln_id, prio_year) %>%
  unique() %>%
  left_join(CPC) %>%
  mutate(CPC4 = substr(CPC_Class, 1, 4)) %>%
  select(-CPC_Class) %>%
  #unique() %>%
  # filter primary technological field for technological identification
  group_by(appln_id) %>%
  filter(row_number() == 1)

head(CPC_clean, 10)

CPC_INV_clean <- CPC_clean %>%
  left_join(INV) %>%
  # filter primary inventor for regional identification
  group_by(appln_id) %>%
  filter(row_number() == 1) %>%
  mutate(NUTS2 = substr(reg_code, 1, 4)) %>%
  select(appln_id, prio_year, CPC4, NUTS2)

head(CPC_INV_clean, 10)

setwd("../../../../Analysis/EconomicGeography/Data/")
write.csv(CPC_INV_clean, "CPC_INC_clean.csv", row.names=FALSE)
CPC_INV_clean <- read.csv("CPC_INC_clean.csv", sep=",")

# ---- technological demarcation ----

# amount of technological classes
length(unique(substr(IPC_CPC$CPC_Class, 1, 4)))
# set IPC4 to four-digit Cooperative Patent Classification (CPC)
IPC_CPC$CPC4 <- substr(IPC_CPC$CPC_Class, 1, 4)

CPC_clean <- IPC_CPC %>%
  select(appln_id, prio_year, CPC4) %>%
  unique()

head(CPC_clean, 10)

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
