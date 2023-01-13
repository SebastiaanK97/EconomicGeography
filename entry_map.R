
library(tidyverse)
# http://econ.geo.uu.nl/peeg/peeg1709.pdf
library(EconGeo)
# http://stavrakoudis.econ.uoi.gr/r-eurostat/drawing-maps-of-europe.html
library(eurostat)
library(sf)

library(ggpubr)
library(ggsci)
library(scales)
#library(patchwork)

c("#E8B63C", "#93BA5A", "#27A581", "#1A7789", "#003764")
c("#27A581", "#93BA5A", "#E8B63C", "#DE8C3F", "#CF553A")
c("#CF553A", "#DE8C3F", "#E8B63C", "#93BA5A", "#27A581")

library(classInt)

# ---- entry to map ----

technological_entry_period <- read.csv("technological_entry_period.csv", sep=",")

ggplot(EU_EFTA, aes(x=n)) + geom_histogram(fill="#003764", colour="white", bins=25) +
  theme_pubr(base_size = 18, base_family = "Georgia") +
  xlab("\nEntry of Technological Fields per Region") + ylab("Density Distribution\n")

sum_entry <- technological_entry_period %>%
  group_by(region, period) %>%
  summarise(n=sum(entry, na.rm=TRUE)) %>%
  select(geo=region, n, period)

EU_EFTA <- geo_data %>%
  filter(geo %in% unique(technological_entry_period$region)) %>%
  left_join(sum_entry) %>%
  mutate(class=cut(n, classIntervals(sum_entry$n, n=5, style="jenks")$brks, include.lowest=T))

par(mfrow=c(2, 2))

map1 <- EU_EFTA %>%
  filter(period == 2) %>%
  ggplot(aes(fill=class)) + geom_sf(size=0.1, colour="#4D4D4D") + 
  theme_void(base_size = 18, base_family = "Georgia") +
  scale_x_continuous(limits=c(-25, 35)) + scale_y_continuous(limits=c(35, 70)) +
  scale_fill_manual(values=c("#E8B63C", "#93BA5A", "#27A581", "#1A7789", "#003764"), name="Entries") +
  labs(subtitle="1999-2003")

map2 <- EU_EFTA %>%
  filter(period == 3) %>%
  ggplot(aes(fill=class)) + geom_sf(size=0.1, colour="#4D4D4D") + 
  theme_void(base_size = 18, base_family = "Georgia") +
  scale_x_continuous(limits=c(-25, 35)) + scale_y_continuous(limits=c(35, 70)) +
  scale_fill_manual(values=c("#E8B63C", "#93BA5A", "#27A581", "#1A7789", "#003764"), name="Entries") +
  labs(subtitle="2004-2008")

map3 <- EU_EFTA %>%
  filter(period == 4) %>%
  ggplot(aes(fill=class)) + geom_sf(size=0.1, colour="#4D4D4D") + 
  theme_void(base_size = 18, base_family = "Georgia") +
  scale_x_continuous(limits=c(-25, 35)) + scale_y_continuous(limits=c(35, 70)) +
  scale_fill_manual(values=c("#E8B63C", "#93BA5A", "#27A581", "#1A7789", "#003764"), name="Entries") +
  labs(subtitle="2009-2013")

map4 <- EU_EFTA %>%
  filter(period == 5) %>%
  ggplot(aes(fill=class)) + geom_sf(size=0.1, colour="#4D4D4D") + 
  theme_void(base_size = 18, base_family = "Georgia") +
  scale_x_continuous(limits=c(-25, 35)) + scale_y_continuous(limits=c(35, 70)) +
  scale_fill_manual(values=c("#E8B63C", "#93BA5A", "#27A581", "#1A7789", "#003764"), name="Entries") +
  labs(subtitle="2014-2018")

ggarrange(map1, map2, map3, map4, ncol=2, nrow=2, common.legend=TRUE, legend="bottom")



