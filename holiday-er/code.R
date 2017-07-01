# devtools::install_github('hadley/neiss')

library(neiss)
library(tidyverse)


theme_henrik <- function(grid=TRUE, legend.position=NA, base_family='Lato Light', highlight_family='Lato') {
  #th <- ggplot2::theme_minimal(base_family = 'LM Roman Dunhill 10', base_size = 13)
  #th <- ggplot2::theme_minimal(base_family = 'Playfair Display', base_size = 13)
  #th <- ggplot2::theme_minimal(base_family = 'Lato Light', base_size = 13)
  th <- ggplot2::theme_minimal(base_family = base_family, base_size = 12)
  
  th <- th + theme(text = element_text(color='#333333'))
  
  th <- th + theme(legend.background = element_blank())
  th <- th + theme(legend.key = element_blank())
  
  # Straight out of hrbrthemes
  if (inherits(grid, "character") | grid == TRUE) {
    th <- th + theme(panel.grid=element_line(color="#cccccc", size=0.3))
    th <- th + theme(panel.grid.major=element_line(color="#cccccc", size=0.3))
    th <- th + theme(panel.grid.minor=element_line(color="#cccccc", size=0.15))
    
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) th <- th + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) th <- th + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) th <- th + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) th <- th + theme(panel.grid.minor.y=element_blank())
    }
    
  } else {
    th <- th + theme(panel.grid=element_blank())
  }
  
  th <- th + theme(axis.text = element_text(family=highlight_family))
  th <- th + theme(axis.ticks = element_blank())
  
  th <- th + theme(axis.text.x=element_text(margin=margin(t=0.5)))
  th <- th + theme(axis.text.y=element_text(margin=margin(r=0.5)))
  
  th <- th + theme(plot.title = element_text(family="Playfair Display"),
                   plot.subtitle = element_text(margin=margin(b=15), family="Playfair Display"),
                   plot.caption = element_text(face='italic', size=10))
  
  if (!is.na(legend.position)) th <- th + theme(legend.position = legend.position)
  
  return (th)
}





df <- injuries %>%
  left_join(products %>% transmute(code, product1=title), by=c('prod1'='code')) %>%
  left_join(products %>% transmute(code, product2=title), by=c('prod2'='code')) %>%
  do({ # Some events become duplicated, but I'd rather have all products
    rbind(
      transmute(., date=trmt_date, weight, product = product1), 
      transmute(., date=trmt_date, weight, product = product2)
    )
  })  %>%
  filter(!is.na(product)) %>%
  mutate(norm_date = lubridate::ymd(20120000 + lubridate::month(date) * 100 + lubridate::mday(date))) %>% # normalize day-of-year for one arbitary year
  group_by(product, norm_date, date) %>%
  summarize(n = sum(weight)) %>%
  ungroup() %>%
  complete(product, nesting(norm_date, date), fill=list(n=0)) %>%
  group_by(product) %>%
  filter(max(n) > 100) %>% # remove the least common, arbitrarily set to 100 weighted cases
  mutate(daily.n = mean(n),
         sd.n = sd(n)) %>% # daily average cases 
  ungroup()

df.holidays <- df %>%
  mutate(month = lubridate::month(date),
         wday = lubridate::wday(date),
         mday = lubridate::mday(date)) %>%
  # Select holidays; those who turned out to not be interesting are commented out
  mutate(holiday = case_when(norm_date == '2012-01-01' ~ 'New year’s day',
                             (month == 1) & (wday == 2) & (between(mday, 15, 21)) ~ 'Martin Luther King, Jr. Day',
                             (month == 2) & (wday == 2) & (between(mday, 15, 21)) ~ 'George Washington’s Birthday',
                             #(month == 5) & (wday == 2) & (between(mday, 31, 25)) ~ 'Memorial day',
                             norm_date == '2012-07-04' ~ 'Independence day',
                             #(month == 9) & (wday == 2) & (between(mday, 1, 7)) ~ 'Labor day',
                             #(month == 10) & (wday == 2) & (between(mday, 8, 14)) ~ 'Coloumbus day',
                             #norm_date == '2012-11-11' ~ 'Veterans day',
                             norm_date == '2012-10-31' ~ 'Halloween',
                             (month == 11) & (wday == 5) & (between(mday, 22, 28)) ~ 'Thanksgiving day',
                             norm_date == '2012-12-25' ~ 'Christmas'
  )) %>%
  filter(!is.na(holiday)) 

df.holidays %>%
  mutate(holiday = factor(holiday, ordered=TRUE, levels=unique(holiday[order(norm_date)]))) %>%
  group_by(holiday, product) %>%
  filter(mean(n) > 50,
         mean((n - daily.n)/sd.n) > 2, # On average 2 SDs more visits this day
         all(n > 0)) %>%
  group_by(holiday, product) %>%
  summarize(holiday.n = mean(n),
            all.n = mean(daily.n)) %>%
  ungroup() %>%
  mutate(product = gsub(', not specified', '', product),
         product = gsub(' *\\(.*\\)*', '', product),
         product = case_when(grepl('scissors', product) ~ 'scissors',
                             grepl('water slides', product) ~ 'water slides',
                             grepl('grills', product) ~ 'grills',
                             (grepl('knives', product) | grepl('slicers and choppers', product)) ~ 'knives',
                             TRUE ~ product)) %>%
  group_by(holiday, product) %>%
  summarize(holiday.n = sum(holiday.n),
            all.n = sum(all.n)) %>%
  ggplot(aes(reorder(product, holiday.n, FUN=max), holiday.n)) +
    geom_col(width=0.8) +
    geom_col(aes(y=all.n), fill='white', width=0.4) +
    geom_hline(yintercept=0, size=0.3) +
    coord_flip() +
    labs(x="", y="", title="Holiday at the ER", caption="@hnrklndbrg | Source: US Consumer Product Safety Commission") +
    facet_grid(holiday ~ ., scales='free_y', space='free_y') +
    theme_henrik(grid='X')

ggsave('/tmp/out.svg', width=8, height=8)


