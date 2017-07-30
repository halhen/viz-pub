library(tidyverse)
source('henrik.r')

df <- read_tsv('pendling.scb', locale = locale(encoding = 'iso8859-1')) %>%
  transmute(region,
            variable = recode(tabellinnehåll,
                              `Inpendlare över kommungräns`='coming_in',
                              `Utpendlare över kommungräns`='going_out',
                              `Bor och arbetar i kommunen`='staying'),
            year = år,
            n = `Förvärvsarbetande pendlare 16+ år`) %>%
  separate(region, into=c('code', 'name'), sep=' ', extra = 'merge')



df %>%
  filter(year == max(year)) %>%
  group_by(name) %>%
  spread(variable, n) %>%
  mutate(p = (coming_in - going_out) / (going_out + staying),
         `in`=coming_in + staying,
         out=going_out + staying) %>%
  arrange(p) %>%
  ggplot(aes(going_out + staying, p, size=coming_in + staying)) +
    geom_point(color='black', fill='#D3BC5F', alpha=0.5, shape=21) +
    scale_size_area(name="Arbetande", max_size = 20, breaks=c(10, 100, 500) * 1e3) +
    scale_y_continuous(labels=scales::percent, breaks = -2:4/4) +
    scale_x_log10(limits=c(NA, 700e3)) +
    annotation_logticks(sides = 'b') +
    labs(x="", y="", caption="@hnrklndbrg | Källa: SCB") +
    theme_henrik(grid='XY', legend.position='bottom')

ggsave('/tmp/out.svg', width=8, height=7)









