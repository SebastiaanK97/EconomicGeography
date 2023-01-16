
#...

# ---- spatial autocorrelation ----

library(spdep)

sf_use_s2(FALSE)

geo_clean <- geo_data %>%
  filter(geo %in% technological_entry_period$region)

EU_nb <- poly2nb(geo_clean$geometry)
EU_lw <- nb2listw(EU_nb, zero.policy=TRUE)

A01B <- technological_entry_period %>%
  filter(industry == "A01B", period == 2) %>%
  mutate(entry = replace_na(entry, 0))

moran.plot(A01B$entry, EU_lw, labels=FALSE,
           xlab="...", ylab="...")

moran.test(A01B$entry, EU_lw, zero.policy=TRUE,
           randomisation=FALSE)