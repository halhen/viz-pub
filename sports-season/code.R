# atus*.csv data from https://www.kaggle.com/bls/american-time-use-survey

library(tidyverse)

df.sum <- read_csv('../data/atus/atussum.csv')
df.resp <- read_csv('../data/atus/atusresp.csv')
df.actcode <- read_csv2('codes.csv') %>%
  mutate(activity = as.integer(code))

df.sum %>%
  select(tucaseid, tufnwgtp, matches('t1301..')) %>% # Select doing sports columns
  inner_join(df.resp %>% select(tucaseid, tumonth), by='tucaseid') %>% # Attach month
  gather(key, time, matches('t[0-9]+')) %>%
  group_by(key, tumonth) %>% 
  summarize(time = sum(tufnwgtp * (time))/sum(tufnwgtp)) %>% # Get mean time spent per sport and month
  mutate(activity = as.integer(substring(key, 2))) %>%
  inner_join(df.actcode, by='activity')  %>% # Get names for activities
  filter(!grepl('^Security', name), !grepl('n\\.e\\.c', name), !grepl('other', name)) %>% # remove Security, n.e.c., and "other"
  mutate(name = gsub('\\(.*\\)', '', name)) %>% # Remove parentheses
  group_by(name) %>%
  filter(max(time) >= 0.05) %>% # Keep only sports with some activity
  mutate(p = time/max(time),
         within.p = time/sum(time)) %>% # sums to 1
  do({ # Find the shortest consecutive streak of months that add up to >= 50%.
       # I'm sure there are prettier ways though...
    df <- arrange(., tumonth)
    start <- NA
    months <- NA
    
    for (n in 6:1) {
      sums <- sapply(1:12, function(x) { sum(df$within.p[(x:(x+(n-1)) - 1) %% 12 + 1])})
      if (max(sums) >= 0.5) {
        months <- n
        start <- which.max(sums)
      }
    }
    
    df %>%
      mutate(high = tumonth %in% ((start:(start+(months-1)) - 1) %% 12 + 1), # Boolean to set whether part of the peak season
             high.start = start, # First month of peak season
             high.n = months) # Number of months in peak season
  }) %>%
  ungroup() %>%
  mutate(name = reorder(name, (high.start * 100) + high.n)) %>% # For a nice order
  filter(high) %>% # Keep only the ones in peak season
  
  ggplot(aes(tumonth, name, fill=factor((high.start - 1) %% 2))) + # Zebra pattern
    geom_tile(color='black', size=0.5) +
    #geom_vline(data=tibble(x=1:12 - 0.5), aes(xintercept=x), color='white') +
    scale_x_continuous(breaks=NULL) +
    scale_y_discrete(expand=c(0.3, 1)) + # For a hole in the middle
    scale_fill_manual(values=c('#E8CF76', '#7c916f')) +
    coord_polar() +
    labs(x="", y="") +
    theme_henrik(grid='') + 
    theme(plot.background = element_rect(fill='#051515'),
          legend.position = 'none',
          axis.text = element_text(color='#dddddd', family='Lato'))


ggsave('/tmp/test.svg', width=20, height=20)