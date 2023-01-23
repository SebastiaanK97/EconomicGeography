
library(eurostat)
library(tidyverse)
library(grid)

# ---- import data ----

setwd("../Data/Patents/OECD_REGPAT_202208/")
dir()

IPC <- read.csv("202208_EPO_IPC.txt", sep="|")
CPC <- read.csv("202208_CPC_Classes.txt", sep="|")
APP <- read.csv("202208_EPO_App_reg.txt", sep="|")
INV <- read.csv("202208_EPO_Inv_reg.txt", sep="|")

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

# final sample of countries
length(unique(geo_data$NUTS2))

# ---- pipeline ----

CPC_clean <- IPC %>%
  select(appln_id, prio_year) %>%
  unique() %>%
  left_join(CPC) %>%
  # technological demarcation
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
  select(appln_id, prio_year, CPC4, NUTS2) %>%
  filter(NUTS2 %in% geo_data$NUTS2)

head(CPC_INV_clean, 10)

length(unique(CPC_INV_clean$CPC4))
length(unique(CPC_INV_clean$NUTS2))

CPC4_NUTS2_1994_2018 <- CPC_INV_clean %>%
  filter(prio_year >= 1994 & prio_year <= 2018) %>%
  filter(NUTS2 %in% geo_data$NUTS2) %>%
  #right_join(geo_data) %>%
  select(appln_id, prio_year, NUTS2, CPC4) %>%
  mutate(count = 1)

#CPC4_NUTS2_1994_2018 <- CPC4_NUTS2_1994_2018 %>%
  mutate(CPC4 = replace_na(CPC4, "")) %>%
  mutate(count = 1)

head(CPC4_NUTS2_1994_2018, 10)

setwd("../../../../Analysis/EconomicGeography/Data/")
write.csv(CPC4_NUTS2_1994_2018, "CPC4_NUTS2_1994_2018.csv", row.names=FALSE)
CPC4_NUTS2_1994_2018 <- read.csv("CPC4_NUTS2_1994_2018.csv", sep=",")

# ---- sample validation ----

# amount of countries (32)
length(unique(substr(CPC4_NUTS2_1994_2018$NUTS2, 1, 2)))
# amount of regions (289)
length(unique(CPC4_NUTS2_1994_2018$NUTS2))
# missingness in regions due to islands of France (FRA1-5) and Cueta in Spain (ES63)
setdiff(geo_data$NUTS2, CPC4_NUTS2_1994_2018$NUTS2)

# amount of technological classes (669)
length(unique(CPC4_NUTS2_1994_2018$CPC4))
# missingness in technological classes due to EU + EFTA sample
setdiff(substr(CPC$CPC_Class, 1, 4), CPC4_NUTS2_1994_2018$CPC4)

CPC4_NUTS2_1994_2018 %>%
  count(NUTS2) %>%
  ggplot(aes(x=n)) + geom_histogram(bins=50, colour="white", fill="#003764") +
  theme_pubr(base_size = 18, base_family = "Georgia") +
  ylab("Patent Frequency\n")

CPC4_NUTS2_1994_2018 %>%
  count(NUTS2) %>%
  ggplot(aes(y=n)) + geom_boxplot(fill="white", colour="#003764") +
  theme_pubr(base_size = 18, base_family = "Georgia") +
  ylab("Patent Frequency\n") + scale_y_log10()
  