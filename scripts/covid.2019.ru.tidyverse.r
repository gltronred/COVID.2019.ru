library(maps)
library(tidyverse)
library(lubridate)

################################################################################
### Load

covid.2019.ru <- read_tsv("../data/momentary.txt",
                          col_types = "Tfffficfccc",
                          trim_ws = T,
                          comment = "#") %>%
  mutate(LOCUS = fct_relevel(LOCUS, sort))
#covid.2019.breaks <- read_tsv("../misc/breaks.txt",
#                              col_types = "T",
#                              comment = "#")
covid.2019.coord <- read_tsv("../misc/coord.txt",
                             col_types = "fddic")
covid.2019.population <- read_tsv("../misc/population.txt",
                                  col_types = "fddic")

################################################################################
### Timeseries

covid.2019.ts <- covid.2019.ru %>%
  mutate(DAY = date(TIMESTAMP)) %>%
  group_by(EVENT,LOCUS,DAY) %>%
  summarise(NUMBER = sum(NUMBER)) %>%
  #arrange(DAY, .by_group = T) %>%
  #mutate(SUM = cumsum(NUMBER)) %>%
  ungroup() %>%
  left_join(covid.2019.population)

################################################################################
### Models

################################################################################
### Plots

### Cumulated cases (total)
g <- ggplot(covid.2019.ts %>%
            filter(EVENT == "detected") %>%
            group_by(DAY) %>%
            summarise(NUMBER = sum(NUMBER)) %>%
            arrange(DAY) %>%
            mutate(SUM = cumsum(NUMBER))) +
  geom_bar(aes(DAY,NUMBER, fill="detected"), stat="identity") +
  geom_line(aes(DAY,SUM)) +
  scale_x_date(date_breaks = "week") +
  guides(fill = "none") +
  labs(x = NULL, y = "Total COVID-19 cases detected", title = "Russian Federation") +
  theme(axis.text.x = element_text(angle = 90))
ggsave("../newplots/COVID.2019.cumulated.png",g, height = 6.25, width = 8.33, units = "in")
ggsave("../newplots/COVID.2019.cumulated.log10.png",g + scale_y_log10(), height = 6.25, width = 8.33, units = "in")

### Cumulated cases (by region)

covid.2019.cum.lump2 <- covid.2019.ts %>%
  filter(EVENT == "detected") %>%
  mutate(LOCUS = fct_lump(LOCUS, 2, other = "The rest of Russia")) %>%
  group_by(LOCUS,DAY) %>%
  summarise(NUMBER = sum(NUMBER)) %>%
  arrange(DAY) %>%
  mutate(SUM = cumsum(NUMBER),
         PREV = lag(SUM))

g <- ggplot(covid.2019.cum.lump2) +
  geom_line(aes(DAY,SUM, colour = LOCUS)) +
  scale_x_date(date_breaks = "week") +
  labs(x = NULL, y = "Total COVID-19 cases detected", colour = "Region", title = "Russian Federation") +
  theme(axis.text.x = element_text(angle = 90))
ggsave("../newplots/COVID.2019.cumulated.by_regions.png",g, height = 6.25, width = 8.33, units = "in")
ggsave("../newplots/COVID.2019.cumulated.log10.by_regions.png",g + scale_y_log10(), height = 6.25, width = 8.33, units = "in")

### Growth ratio
g <- ggplot(covid.2019.cum.lump2) +
  geom_line(aes(DAY,SUM / PREV, colour = LOCUS)) +
  coord_cartesian(xlim = c(ymd("2020-03-06"),max(covid.2019.ts$DAY)), ylim = c(1,1.8)) +
  labs(x = NULL, y = "The ratio of COVID-19 cases detected day N to day N-1", colour = NULL, title = "Russian Federation")
ggsave("../newplots/COVID.2019.growth_ratio.png",g, height = 6.25, width = 8.33, units = "in")

### Regional centres
g <- ggplot(covid.2019.ts %>%
            filter(EVENT == "detected") %>%
            mutate(LOCUS = fct_other(LOCUS,
                                     keep = c("Moscow", "St. Petersburg", "Volgograd", "Voronezh", "Ekaterinburg",
                                              "Kazan", "Krasnoyarsk", "Nizhnii Novgorod", "Omsk", "Perm krai",
                                              "Rostov-on-Don", "Samara", "Bashkortostan", "Cheliabinsk"))) %>%
            filter(!(LOCUS %in% c("Moscow","St. Petersburg","Other"))) %>%
            arrange(DAY) %>%
            group_by(LOCUS,DAY) %>%
            summarise(NUM = sum(NUMBER)) %>%
            mutate(SUM = cumsum(NUM))) +
  geom_line(aes(DAY,SUM, colour = LOCUS)) +
  scale_x_date(date_breaks = "week") +
  labs(x = NULL, y = "Total COVID-19 cases detected", colour = "Region", title = "Russian Federation, regions with capitals of 1,000 K population and more") +
  theme(axis.text.x = element_text(angle = 90))
ggsave("../newplots/COVID.2019.cumulated.1M.png",g, height = 6.25, width = 8.33, units = "in")
ggsave("../newplots/COVID.2019.cumulated.log10.1M.png",g + scale_y_log10(), height = 6.25, width = 8.33, units = "in")

#ggplot(covid.2019.ts %>%
#            filter(EVENT == "detected") %>%
#            mutate(LOC = fct_lump(LOCUS,7, other = "The rest of Russia")) %>%
#            arrange(DAY) %>%
#            group_by(LOC,DAY) %>%
#            summarise(NUM = sum(NUMBER)) %>%
#            group_by(LOC) %>%
#            mutate(SUM = cumsum(NUM))) +
#  geom_line(aes(DAY,SUM, colour = LOC)) +
#  scale_x_date(date_breaks = "week") +
#  scale_y_log10() +
#  labs(x = NULL, y = "Total COVID-19 cases detected", colour = "Region", title = "Russian Federation") +
#  theme(axis.text.x = element_text(angle = 90))

covid.2019.cum <- covid.2019.ts %>%
  group_by(EVENT,LOCUS) %>%
  summarise(SUM = sum(NUMBER))

### Regions barplot
g <- ggplot(covid.2019.cum %>%
            filter(EVENT == "detected") %>%
            mutate(LOCUS = fct_reorder(LOCUS, SUM, .desc=T))) +
  geom_bar(aes(LOCUS, SUM), stat = "identity") +
  labs(x = NULL, y = paste0("Total COVID-2019 cases, as of ", max(covid.2019.ts$DAY)), title = "Russian Federation") +
  theme(axis.text.x = element_text(angle = 90))
ggsave("../newplots/COVID.2019.barplot.regions.png",g, height = 6.25, width = 8.33, units = "in")
ggsave("../newplots/COVID.2019.barplot.regions.log10.png",g + scale_y_log10(), height = 6.25, width = 8.33, units = "in")

### Regions barplot per 100K
g <- ggplot(covid.2019.cum %>%
            filter(EVENT == "detected") %>%
            left_join(covid.2019.population) %>%
            mutate(PER100K = SUM/POPULATION*100000,
                   LOCUS = fct_reorder(LOCUS, PER100K, .desc=T))) +
  geom_bar(aes(LOCUS, PER100K), stat = "identity") +
  labs(x = NULL, y = paste0("Total COVID-2019 cases per 100K, as of ", max(covid.2019.ts$DAY)), title = "Russian Federation") +
  theme(axis.text.x = element_text(angle = 90))
ggsave("../newplots/COVID.2019.barplot.regions.per_100K.png",g, height = 6.25, width = 8.33, units = "in")

### Map total cases
g <- ggplot(covid.2019.cum %>%
            filter(EVENT == "detected") %>%
            left_join(covid.2019.population)) +
  borders("world", "Russia") +
  geom_point(aes(x = LON, y = LAT, color = SUM, size = SUM), alpha = 0.8) +
  scale_size_continuous(trans = "sqrt", range = c(1,20)) +
  scale_color_continuous(trans = "sqrt", low = "pink", high = "red") +
  guides(size = "none") +
  coord_quickmap() +
  labs(colour = "Cases", title = paste0("Total COVID-19 cases, as of ", max(covid.2019.ts$DAY)))
ggsave("../newplots/COVID.2019.map.regions.png",g, height = 6.25, width = 8.33, units = "in")

### Map total cases per 100K
g <- ggplot(covid.2019.cum %>%
            filter(EVENT == "detected") %>%
            left_join(covid.2019.population) %>%
            mutate(PER100K = SUM/POPULATION*100000)) +
  borders("world", "Russia") +
  geom_point(aes(x = LON, y = LAT, color = PER100K, size = PER100K), alpha = 0.8) +
  scale_size_continuous(range = c(1,20)) +
  scale_color_continuous(low = "pink", high = "red") +
  guides(size = "none") +
  coord_quickmap() +
  labs(colour = "Cases per 100K", title = paste0("Total COVID-19 cases, as of ", max(covid.2019.ts$DAY)))
ggsave("../newplots/COVID.2019.map.regions.per_100K.png",g, height = 6.25, width = 8.33, units = "in")

### TARD
g <- ggplot(covid.2019.ts %>%
            arrange(DAY) %>%
            group_by(EVENT,DAY) %>%
            summarise(NUMBER = sum(NUMBER)) %>%
            mutate(SUM = cumsum(NUMBER),
                   NUMBER = NULL) %>%
            spread(EVENT,SUM) %>%
            fill(detected:deceased) %>%
            replace_na(list(recovered = 0, deceased = 0)) %>%
            mutate(active = detected - recovered - deceased) %>%
            gather("EVENT", "SUM", detected:active)) +
  geom_line(aes(DAY,SUM, colour = EVENT)) +
  labs(x = NULL, y = "COVID-19 cases", title = "Russian Federation") +
  theme(axis.text.x = element_text(angle = 90))
ggsave("../newplots/COVID.2019.cumulated.TARD.png",g, height = 6.25, width = 8.33, units = "in")
ggsave("../newplots/COVID.2019.cumulated.TARD.log10.png",g + scale_y_log10(), height = 6.25, width = 8.33, units = "in")
