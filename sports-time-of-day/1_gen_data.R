library(tidyverse)
library(lubridate)

# Load data; see https://www.kaggle.com/bls/american-time-use-survey

df.act <- read_csv('~henrik/private/vizyns/data/atus/atusact.csv')
df.sum <- read_csv('~henrik/private/vizyns/data/atus/atussum.csv')


# Get a data frame with activity (trcodep), observation weight (tufnwgtp)
# and interval for doing (start - end)

df.tmp <- df.act %>%
  filter(trtier2p == 1301) %>% # Doing sports/exercise
  
  # start and end are minutes since midnight
  transmute(tucaseid,
            trcodep,
            start = as.numeric(substring(tustarttim, 1, 2)) * 60 + as.numeric(substring(tustarttim, 4, 5)),
            end = as.numeric(substring(tustoptime, 1, 2)) * 60 + as.numeric(substring(tustoptime, 4, 5))) %>%
  inner_join(df.sum %>% select(tucaseid, tufnwgtp)) %>%
  
  # some activities wrap over midnight, which shows as end < start
  # when this happens, add one row before and another after midnight
  do({
    rbind(filter(., start <= end), # within a calendar day
          filter(., start > end) %>% # before and up until midnight
            mutate(end = 24*60-1),
          filter(., start > end) %>% # after midnight until end
            mutate(start = 0))
  })



# For each X minute point in time, see how large percentage do each activity
# I would guess that there are more elegant ways to go about this, but I
# haven't bothered to.
df.tmp2 <- expand.grid(time = seq(from = 0, to = 24 * 60 - 1, by = 5),
            trcodep = unique(df.tmp$trcodep)) %>%
  mutate(p = map2_dbl(trcodep, time, function(p_trcodep, p_time) {
    df.tmp %>%
      filter(trcodep == p_trcodep,
             start <= p_time,
             end >= p_time) %>%
      with(sum(tufnwgtp) / sum(df.sum$tufnwgtp)) # tufnwgtp is an observation weight
                                                 # p is the sum(tufnwgtp) of those doing divided
                                                 # by sum(tufnwgtp) for everybody
  }))
  

# Get some names, rename and order, and write csv
df.tmp2 %>%
  inner_join(
    read_csv2('codes.csv') %>%
      mutate(trcodep = as.integer(code)), by='trcodep'    
  ) %>%
  transmute(activity=name, time, p) %>%
  write_tsv('activity.tsv')






