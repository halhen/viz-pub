library(tidyverse)
library(rvest)

SOURCE='https://ourworldindata.org/famines/'

h <- read_html(SOURCE)
h %>%
  html_table() %>%
  .[[1]] %>%
  mutate(year_start = as.integer(substring(Year, 1, 4)),
         year_end = as.integer(substring(Year, 6)),
         digits = ceiling(log10(year_end)),
         year_end = as.integer(floor(year_start / 10^digits) * 10^digits + year_end),
         year_end = coalesce(year_end, year_start)) %>%
  select(-digits) %>%
  mutate(mortality = as.numeric(gsub(',', '', `Excess Mortality- mid-point`))) %>%
  transmute(year_start, year_end, mortality, country=Country, continent=`OWID continent`) %>%
  mutate(continent = recode(continent, 'Europe/Asia' = 'Asia')) %>%
  group_by(continent, year = round((year_start + year_end)/2)) %>%
  summarize(mortality = sum(mortality, na.rm=TRUE)) %>%
  filter(continent != '') %>%
  arrange(-mortality) %>%
  ggplot(aes(year, continent, size=mortality)) +
  geom_point(alpha=0.5, fill='red', color='white', shape=21) +
  scale_size_continuous(range=c(1, 25), breaks=c(1, 10, 20)*1e6) +
  scale_x_continuous(breaks=seq(from=1850, to=2015, by=10), labels=function(x) {ifelse(x %% 100 == 0 | x == 1850, paste0(x), paste0("'", x%%100))}) +
  labs(x="", y="", title="Famines", caption=paste0("@hnrklndbrg | Source: ", SOURCE)) +
  theme_henrik(grid='XY')

ggsave('/tmp/out.svg', width=10, height=5)