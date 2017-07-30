library(tidyverse)
source("henrik.r")

df.fertility <- read_csv('https://ourworldindata.org/grapher/children-per-woman-UN.csv')
df.footprint <- read_csv('footprint.csv')


df <- df.footprint %>%
  transmute(country=Country,
            population = `Population (millions)` * 1e6,
            footprint = `Total Ecological Footprint`) %>%
  mutate(country_code = countrycode::countrycode(country, 'country.name', 'iso3c')) %>%
  inner_join(df.fertility %>%
               filter(Year == 2015) %>%
               transmute(country_code = `Country code`,
                         fertility = `UN – Population Division (Fertility) – 2015 revision`),
             by = 'country_code')



df.tmp <- df %>%
  mutate(mean.footprint = sum(population * footprint) / sum(population),
         mean.fertility = sum(population * fertility) / sum(population)) %>%
  mutate(footkids = fertility * footprint / mean.fertility)




mean.fertility = first(df.tmp$mean.fertility)

df.grid <- expand.grid(fertility = seq(1, 8.3, length.out = 1000),
                       out = 1:10) %>%
  mutate(footprint = out * mean.fertility / fertility)

df.tmp %>%
  ggplot(aes(fertility, footprint)) +
  geom_line(data = df.grid, aes(group = out), size=0.1) +
  geom_point(aes(fill=fertility * footprint/mean.fertility, size=footprint * fertility / mean.fertility), shape=21, color='black', alpha=1) +
  #geom_point(aes(size=population)) +
  scale_size_area(max_size=8, breaks=c(10e6, 100e6, 1000e6)) +
  #viridis::scale_fill_viridis(option='D', limits=c(0, 10), breaks = 0:5 * 2) +
  viridis::scale_fill_viridis() +
  #scale_x_continuous(limits=c(0, NA), breaks=0:4 * 2) +
  scale_y_continuous(limits=c(NA, 17), breaks=0:3 * 5) +
  labs(x="Fertility (children per woman)", y="Ecological footprint (global hectares per capita)", title="Resource adjusted fertility rates", caption="@hnrklndbrg\nSource: https://ourworldindata.org, http://www.footprintnetwork.org/") +
  theme_henrik(grid = 'Y', legend.position = 'none') +
  theme(axis.ticks.x = element_line(size=0.1))

ggsave('/tmp/out.svg', width=7, height=10)











