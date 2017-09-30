library(tidyverse)
library(haven)
source('henrik.r')

# Fetch from respective source; see caption
df.atus <- read_csv('atussum.csv')
df.cdc <- read_xpt('LLCP2016.xpt')




df.all <- rbind(
  df.atus %>%
    filter(tuyear >= 2010) %>%
    mutate(t010101 = t010101 / 60) %>%
    transmute(study = 'atus', age = teage, sleep = t010101, weight = tufnwgtp / sum(tufnwgtp)),
  df.cdc %>%
    mutate(weight = `_LLCPWT`) %>%
    filter(SLEPTIM1 <= 24) %>%
    transmute(study = 'brfss', age = `_AGE80`, sleep = SLEPTIM1, weight = weight / sum(weight))
)


df.tmp <- df.all %>%
  group_by(study) %>%
  do({
    this <- .
    tibble(p = 0:500/500) %>%
      rowwise() %>%
      mutate(h = quantile(this$sleep, p))
  })

df.tmp %>%
  filter(p >= 0.05, p <= 0.95) %>%
  ggplot(aes(p, h, color=study)) +
  geom_line() +
  scale_x_continuous(labels = function(x) {x*100}) +
  scale_y_continuous(labels = function(x) {paste0(x, 'h:00')}, breaks=seq(from=4, to=14, by=2)) +
  scale_color_manual(values=c('atus'='#990000', 'brfss'='#075290')) +
  labs(x="", y="", title="", caption="@hnrklndbrg\nSources: https://www.bls.gov/tus/\nhttps://www.cdc.gov/brfss/annual_data/annual_2016.html") +
  theme_henrik(grid="XYy", legend.position = 'none')

ggsave('/tmp/out.svg', width=6, height = 7)