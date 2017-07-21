library(tidyverse)
library(rworldmap)
map.world <- map_data(map="world")



distances <- map.world %>%
  group_by(country = region) %>%
  summarize() %>%
  do({
    expand.grid(country1 = .$country,
                country2 = .$country) %>%
      filter(as.character(country1) < as.character(country2))
  }) %>%
  mutate(distance = map2_dbl(country1, country2, function(from, to) {
    df.from = map.world %>%
      filter(region == from)
    df.to = map.world %>%
      filter(region == to)
    
    dists <- sp::spDists(select(df.from, long, lat) %>% as.matrix,
                         select(df.to, long, lat) %>% as.matrix,
                         longlat = TRUE)
    min(dists)
  }))

distances %>%
  write_tsv('distances.csv')