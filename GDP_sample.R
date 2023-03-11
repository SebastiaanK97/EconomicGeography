
library(tidyverse)
library(rnaturalearthdata)

library(eurostat)
library(sf)

# ---- import ----

getwd()

GDP_share <- read.csv("Data/GDP_euro_per_capita_percent_average.csv", sep=",") %>%
  select(NUTS2 = geo, year = TIME_PERIOD, GDP_share = OBS_VALUE) %>%
  mutate(country = substr(NUTS2, 1, 2))

GDP <- read.csv("Data/GDP_pc.csv", sep=",") %>%
  select(NUTS2 = geo, year = TIME_PERIOD, GDP_pc = OBS_VALUE) %>%
  right_join(GDP_share) %>%
  filter(year <= 2020) %>%
  mutate(objective = case_when(GDP_share < 75 ~ "Periphery", GDP_share >= 75 ~ "Core"))

NUTS2_fix <- recode_nuts(GDP, geo = "NUTS2", nuts_year = 2016)

GDP <- GDP %>%
  left_join(NUTS2_fix) %>%
  mutate(code_2016 = coalesce(code_2016, NUTS2)) %>%
  select(NUTS2 = code_2016, year, GDP_share, GDP_pc, country)

geo_map <- get_eurostat_geospatial(output_class = "sf", resolution = "1", nuts_level = "2", year = "2016", make_valid=TRUE) %>%
  select(NUTS2 = geo, geometry, country = CNTR_CODE)

worldmap <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')

# ---- visualisation ----

GDP_map <- geo_map %>%
  left_join(GDP) %>%
  mutate(objective = case_when(GDP_share < 75 ~ "Periphery", GDP_share >= 75 ~ "Core")) %>%
  filter(year == 2020)

ggplot() +
  geom_sf(data = worldmap, colour = "white", linewidth = 0.05) +
  geom_sf(data = GDP_map, aes(fill = objective), colour = "white", linewidth = 0.05) +
  coord_sf(xlim = c(-12, 46), ylim = c(33, 73), expand = FALSE) +
  theme_void(base_size = 18, base_family = "Georgia") +
  scale_fill_manual(values=c("#93BA5A", "#E8B63C"), name="Objective")

GDP_trend <- GDP %>%
  mutate(objective = case_when(GDP_share < 75 ~ "Periphery", GDP_share >= 75 ~ "Core")) %>%
  group_by(objective, year) %>%
  summarise(mean_GDP_pc = mean(GDP_pc, na.rm=TRUE))

y_mean_GDP_pc <- GDP_trend %>%
  filter(year %in% c(2000, 2020)) %>%
  filter(objective == "Core") %>%
  pull(mean_GDP_pc) %>%
  rep(2)

y_diff_mean_GDP_pc <- GDP_trend %>%
  filter(year %in% c(2000, 2020)) %>%
  group_by(year) %>%
  summarise(difference = 1 / (mean_GDP_pc[objective == "Periphery"] / mean_GDP_pc[objective == "Core"])) %>%
  pull(difference) %>%
  round(2) %>%
  rep(2)

GDP_trend %>%
  filter(year %in% c(2000, 2020)) %>%
  ggplot(aes(x=year, y=mean_GDP_pc, fill=objective)) +
  geom_bar(stat="identity", position=position_dodge(width=20), colour="white") +
  geom_errorbarh(aes(xmax=(year + 5), xmin=(year - 5), y=(y_mean_GDP_pc + 2500)), height=1000) +
  scale_fill_manual(values=c("#93BA5A", "#E8B63C"), name="Objective") +
  geom_text(aes(x=year), y=y_mean_GDP_pc+3500, label=y_diff_mean_GDP_pc) +
  theme_void(base_size = 18, base_family = "Georgia") +
  theme(legend.position = "none")





