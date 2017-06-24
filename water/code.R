library(tidyverse)

# From https://stats.oecd.org/Index.aspx?DataSetCode=ALFS_POP_LABOUR on 2017-06-24
df.force <- read_csv('labour-force.csv')

# From https://stats.oecd.org/Index.aspx?DataSetCode=ANHRS on 2017-06-24
df.hours <- read_csv('hours-worked.csv')

df.force %>%
  filter(Time==2014, Subject == 'Civilian Labour force') %>% # 2014 is the most complete set
  transmute(country = Country,
            n = Value * 1000) %>%
  filter(! country %in% c('Brazil', 'Colombia')) %>%
  arrange(country) %>%
  left_join(df.hours %>%
              filter(Time==2014, `Employment status` == 'Total employment') %>%
              mutate(Country = recode(Country, "Russian Federation"="Russia")) %>%
              transmute(country = Country,
                        hours = Value), by='country') %>%
  mutate(hours = case_when(country == 'Turkey' ~ 1832L, # At the time of writing, Turkey has no data
                           TRUE ~ hours)) %>%
  mutate(total = hours * n) %>%
  arrange(-total) %>%
  transmute(country, total) %>%
  rbind(tibble(country='Water', total=365.25 * 200e6)) %>% # 200 million/day from https://www.unicef.org/media/media_92690.html
  ggplot(aes(reorder(country, total), total)) +
  geom_bar(stat='identity', aes(fill=ifelse(country=='Water', 'Water', 'Country')), width=0.9) +
  geom_text(aes(label=ifelse(total >= 15e9, paste0(round(total/1e9), " "), NA)), color='white', hjust=1, family="Lato", size=3) +
  coord_flip() +
  scale_y_continuous(labels=function(x) {paste0(x/1e9, "")}, position='right', breaks=NULL) +
  scale_fill_manual(values=c('Country'='#7aa1ae', Water='#044c66')) +
  labs(x="", y="", title="The price of water", caption="@hnrklndbrg | Source: OECD / UNICEF") +
  theme_henrik(grid='X') +
  theme(legend.position='none')

ggsave('/tmp/water.svg', width=5.5, height=7)