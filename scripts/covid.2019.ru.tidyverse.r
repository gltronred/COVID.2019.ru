library(drc)
#library(aomisc)
library(maps)
library(tidyverse)
library(lubridate)
library(RcppRoll)
library(modelr)

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
#covid.2019.coord <- read_tsv("../misc/coord.txt",
#                             col_types = "fddic")
covid.2019.population <- read_tsv("../misc/population.txt",
                                  col_types = "fddic")

################################################################################
### Timeseries

covid.2019.ts <- covid.2019.ru %>%
  mutate(DAY = date(TIMESTAMP)) %>%
  group_by(EVENT,LOCUS,DAY) %>%
  summarise(NUMBER = sum(NUMBER)) %>%
  ungroup() %>%
  left_join(covid.2019.population)

covid.2019.cumul.lump2 <- covid.2019.ts %>%
  filter(EVENT == "detected") %>%
  mutate(LOCUS = fct_lump(LOCUS, 2, other = "The rest of Russia")) %>%
  group_by(LOCUS,DAY) %>%
  summarise(NUMBER = sum(NUMBER)) %>%
  arrange(DAY) %>%
  mutate(SUM = cumsum(NUMBER),
         PREV = lag(SUM))

covid.2019.cumul <- covid.2019.ts %>%
  group_by(EVENT,LOCUS) %>%
  summarise(SUM = sum(NUMBER))

covid.2019.cumul.daily <- covid.2019.ts %>%
  filter(EVENT == "detected") %>%
  arrange(DAY) %>%
  group_by(DAY) %>%
  summarise(NUMBER = sum(NUMBER)) %>%
  mutate(SUM = cumsum(NUMBER),
         START = (DAY - ymd("2020-01-31"))/ddays(1))

################################################################################
### Models

#covid.2019.short.exp <- drm(covid.2019.cumul.daily$SUM ~ covid.2019.cumul.daily$DAY,
#                            fct = DRC.expoGrowth())

covid.2019.short.ll3 <- drm(SUM ~ START,
                            data = covid.2019.cumul.daily %>%
                              filter(DAY > "2020-03-03"),
                            fct = LL.3())
covid.2019.full.ll3 <- drm(SUM ~ START,
                            data = covid.2019.cumul.daily,
                            fct = LL.3())
#summary(covid.2019.short.ll3)

g <- ggplot(data.frame(START = seq(0,120)) %>%
            add_predictions(covid.2019.short.ll3, var="short") %>%
            add_predictions(covid.2019.full.ll3, var="full") %>%
            left_join(covid.2019.cumul.daily) %>%
            fill(SUM, .direction="up")) +
  geom_point(aes(START,SUM, colour="sum")) +
  geom_line(aes(START,full, colour="full")) +
  geom_vline(aes(xintercept = covid.2019.full.ll3$fit$par[3])) +
  scale_y_log10() +
  coord_cartesian(ylim = c(1,1000000)) +
  guides(colour = "none") +
  labs(x = "Days since 2020-01-31", y = "Total cases registered", title = "Russian Federation")
ggsave("../newplots/COVID.2019.fitting.rmc.log10.png",g, height = 6.25, width = 8.33, units = "in")

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

g <- ggplot(covid.2019.cumul.lump2) +
  geom_line(aes(DAY,SUM, colour = LOCUS)) +
  scale_x_date(date_breaks = "week") +
  labs(x = NULL, y = "Total COVID-19 cases detected", colour = "Region", title = "Russian Federation") +
  theme(axis.text.x = element_text(angle = 90))
ggsave("../newplots/COVID.2019.cumulated.by_regions.png",g, height = 6.25, width = 8.33, units = "in")
ggsave("../newplots/COVID.2019.cumulated.log10.by_regions.png",g + scale_y_log10(), height = 6.25, width = 8.33, units = "in")

### Growth ratio
g <- ggplot(covid.2019.cumul.lump2) +
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

### Top-7 regions
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

### Regions barplot
g <- ggplot(covid.2019.cumul %>%
            filter(EVENT == "detected") %>%
            mutate(LOCUS = fct_reorder(LOCUS, SUM, .desc=T))) +
  geom_bar(aes(LOCUS, SUM), stat = "identity") +
  labs(x = NULL, y = paste0("Total COVID-2019 cases, as of ", max(covid.2019.ts$DAY)), title = "Russian Federation") +
  theme(axis.text.x = element_text(angle = 90))
ggsave("../newplots/COVID.2019.barplot.regions.png",g, height = 6.25, width = 8.33, units = "in")
ggsave("../newplots/COVID.2019.barplot.regions.log10.png",g + scale_y_log10(), height = 6.25, width = 8.33, units = "in")

### Regions barplot per 100K
g <- ggplot(covid.2019.cumul %>%
            filter(EVENT == "detected") %>%
            left_join(covid.2019.population) %>%
            mutate(PER100K = SUM/POPULATION*100000,
                   LOCUS = fct_reorder(LOCUS, PER100K, .desc=T))) +
  geom_bar(aes(LOCUS, PER100K), stat = "identity") +
  labs(x = NULL, y = paste0("Total COVID-2019 cases per 100K, as of ", max(covid.2019.ts$DAY)), title = "Russian Federation") +
  theme(axis.text.x = element_text(angle = 90))
ggsave("../newplots/COVID.2019.barplot.regions.per_100K.png",g, height = 6.25, width = 8.33, units = "in")

### Map total cases
g <- ggplot(covid.2019.cumul %>%
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
g <- ggplot(covid.2019.cumul %>%
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

################################################################################
### Regions

### Common utils
scales_for_selection <- list(
  scale_colour_manual(values = c("TRUE" = "red", "FALSE" = "black")),
  scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = 1)),
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.1)),
  guides(colour = "none", size = "none", alpha = "none")
)

covid.2019.reg <- covid.2019.ts %>%
  filter(EVENT == "detected") %>%
  arrange(DAY) %>%
  group_by(LOCUS) %>%
  mutate(SUM = cumsum(NUMBER),
         PREV = lag(SUM),
         RT = roll_meanr(SUM/PREV, n=3, by=1, fill=NA))
covid.2019.race <- covid.2019.reg %>%
  filter(SUM >= 50) %>%
  group_by(LOCUS) %>%
  mutate(START = DAY - min(DAY))

### Directories
dir.create("../newplots/regions")
dir.create("../newplots/regions/rt")
dir.create("../newplots/regions/race")
dir.create("../newplots/regions/rt.race")

for(i in 1:length(levels(covid.2019.ts$LOCUS))){
  region <- levels(covid.2019.ts$LOCUS)[i]

### Pure detected
  g <- ggplot(covid.2019.reg %>%
              mutate(SEL = LOCUS==region)) +
    geom_line(aes(DAY,SUM, group = LOCUS, size = SEL, colour = SEL, alpha = SEL)) +
    scale_x_date(breaks = "week", minor_breaks = "day") +
    scale_y_log10() +
    scales_for_selection +
    labs(x = NULL, y = "Total COVID-19 cases detected", title = paste0("Russian Federation / ", region)) +
    theme(axis.text.x = element_text(angle = 90))
  ggsave(paste0("../newplots/regions/COVID.2019.cumulated.log10.", i, ".png"),g, height = 6.25, width = 8.33, units = "in")

### Pure Rt
  g <- ggplot(covid.2019.reg %>%
              mutate(SEL = LOCUS==region)) +
    geom_line(aes(DAY, RT, group=LOCUS, size=SEL, colour=SEL, alpha=SEL)) +
    scale_x_date(breaks = "week", minor_breaks = "day") +
    scale_y_continuous(breaks = seq(1,1.7,0.1), minor_breaks = seq(1,1.7,0.05)) +
    scales_for_selection +
    coord_cartesian(xlim = c(ymd("2020-03-11"), max(covid.2019.ts$DAY)), ylim = c(1,1.7)) +
    labs(x = NULL, y = "Rt, rolling average for 3 days", title = paste0("Russian Federation / ", region))
  ggsave(paste0("../newplots/regions/rt/COVID.2019.rt.log10.", i, ".png"),g, height = 6.25, width = 8.33, units = "in")

### Race detected
  g <- ggplot(covid.2019.race %>%
              mutate(SEL = LOCUS==region)) +
    geom_line(aes(START,SUM, group = LOCUS, size = SEL, colour = SEL, alpha = SEL)) +
    scale_x_continuous(breaks = seq(0,max(covid.2019.race$START)/ddays(1),5), minor_breaks = seq(0,max(covid.2019.race$START))) +
    scale_y_log10() +
    scales_for_selection +
    labs(x = "Days since N=50 threshold", y = "Total COVID-19 cases detected", title = paste0("Russian Federation / ", region))
  ggsave(paste0("../newplots/regions/race/COVID.2019.race.log10.", i, ".png"),g, height = 6.25, width = 8.33, units = "in")

### Race Rt
  g <- ggplot(covid.2019.race %>%
              mutate(SEL = LOCUS==region)) +
    geom_line(aes(START, RT, group=LOCUS, size=SEL, colour=SEL, alpha=SEL)) +
    scale_x_continuous(breaks = seq(0,max(covid.2019.race$START)/ddays(1),5), minor_breaks = seq(0,max(covid.2019.race$START))) +
    scale_y_continuous(breaks = seq(1,1.7,0.1), minor_breaks = seq(1,1.7,0.05)) +
    scales_for_selection +
    coord_cartesian(xlim = c(0, max(covid.2019.race$START)), ylim = c(1,1.7)) +
    labs(x = "Days since N=50 threshold", y = "Rt, rolling average for 3 days", title = paste0("Russian Federation / ", region))
  ggsave(paste0("../newplots/regions/rt.race/COVID.2019.rt.race.log10.", i, ".png"),g, height = 6.25, width = 8.33, units = "in")

}
