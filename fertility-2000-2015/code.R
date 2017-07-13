library(tidyverse)
source('henrik.r')

df <- read_csv('https://ourworldindata.org/grapher/children-per-woman-UN.csv')
colnames(df) <- c('country', 'year', 'country_code', 'fertility')


df.cont <- read_csv('https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv')

df.cont <- df.cont %>%
  transmute(country_code = `alpha-3`,
            region,
            subregion = `sub-region`)




df %>%
  filter(year %in% c(2000, 2015)) %>%
  spread(year, fertility) %>%
  #filter(is.na(country_code)) %>%
  #filter(!is.na(country_code) & (`2015` < 2.1)) %>%
  inner_join(df.cont) %>%
  group_by(region) %>%
  arrange(desc(`2015`)) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  mutate(country = reorder(country, -row)) %>%
  ggplot(aes(country, color=as.character(floor((row-1) / 5) %% 2))) +
    geom_hline(yintercept=2.1) +
    geom_segment(aes(xend=country, y=`2000`, yend=`2015`)) +
    geom_point(aes(y=`2015`)) +
    scale_y_continuous(breaks=1:8) +
    scale_color_manual(values=c('0'='#786721', '1'='#dab936')) +
    coord_flip() +
    facet_grid(region ~ ., scales='free', space='free') +
    theme_henrik(grid='Xx', legend.position='none') +
    theme(panel.background = element_rect(fill='white', size=0.3))

ggsave('/tmp/out.svg', width=7, height=25)