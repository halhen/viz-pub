library(tidyverse)
library(haven)
source("henrik.r")


# Data from http://www.europeansocialsurvey.org/data/
df.5 <- tibble(filename = list.files('/home/henrik/private/vizyns/data/ess/', 'ESS5.*.sav', full.names=TRUE)) %>%
  mutate(data = map(filename, haven::read_sav)) %>%
  unnest(data)



df.tmp <- df %>%
  filter(agea >= 15, agea <= 65) %>%
  mutate(value = wmcpwrk) %>% filter(value != 8) 



df.tmp %>%
  filter(!is.na(nacer2), !is.na(gndr)) %>%
  group_by(nacer2) %>%
  summarize(score = sum((value >= 4) * pspwght * pweight, na.rm=TRUE) / sum(pspwght * pweight),
            p.men = sum((gndr==1) * pspwght * pweight, na.rm=TRUE) / sum(pspwght * pweight),
            n = n()) %>%
  filter(n >= 250) %>%
  ungroup() %>%
  inner_join(read_tsv('/home/henrik/private/vizyns/data/ess/nacer2.csv'), by=c('nacer2'='code')) %>%
  gather(key, score, score:p.men) %>%
  mutate(out = forcats::fct_reorder(name, score * (key == 'score'))) %>%
  ggplot(aes(out, score)) +
  geom_point() +
  geom_segment(aes(xend=out, yend=ifelse(key == 'score', 0, 0.5))) +
  coord_flip() +
  scale_y_continuous(labels=scales::percent) +
  labs(x="", y="", caption="@hnrklndbrg | Source: European Social Survey") +
  theme_henrik(grid='Xx') +
  facet_wrap(~ key, scales='free_x')

ggsave('out.svg', width=10, height=8)