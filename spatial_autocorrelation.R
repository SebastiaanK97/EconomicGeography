
library(spdep)

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

# ---- patent data  ----

technological_entry_period <- read.csv("Data/technological_entry_period.csv", sep=",")

sf_use_s2(FALSE)

geo_clean <- geo_data %>%
  filter(geo %in% technological_entry_period$region)

EU_nb <- poly2nb(geo_clean$geometry, queen=TRUE)
EU_lw <- nb2listw(EU_nb, zero.policy=TRUE)

field <- technological_entry_period %>%
  filter(industry == "G06F", period == 2) %>%
  mutate(entry = replace_na(entry, 0))

field <- technological_entry_period %>%
  group_by(region) %>%
  summarise(entry = sum(entry, na.rm = TRUE)) 

# ---- spatial autocorrelation (continuous) ----

moran.plot(field$entry, EU_lw, labels=FALSE, zero.policy=TRUE,
           xlab="Observed Median", ylab="Spatial lag median")

moran.test(field$entry, EU_lw, zero.policy=TRUE,
           randomisation=FALSE)

#Calculation of the range of Moran’s I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat+t(wmat))/2)$values))
}
moran.range(EU_lw)

# ---- spatial autocorrelation (binary) ----

#https://rstudio-pubs-static.s3.amazonaws.com/223305_944ddc517306448f8fb0d60ca29dd94b.html
EU_nb <- poly2nb(geo_clean$geometry, queen=TRUE)
EU_lw <- nb2listw(EU_nb, style='B', zero.policy=TRUE)
joincount.test(as.factor(field$entry), EU_lw, zero.policy=TRUE)


