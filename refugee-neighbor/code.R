library(tidyverse)
source("henrik.r")

df <- read_csv('unhcr_popstats_export_time_series_all_data.csv', skip=3, na='*') %>%
  transmute(
    year = Year,
    residence = `Country / territory of asylum/residence`,
    origin = Origin,
    type = `Population type`,
    n = coalesce(Value, 4L)
  ) %>%
  mutate(residence_corr = recode(residence, `Central African Rep.` = 'Central African Republic'),
         residence_code = countrycode::countrycode(residence_corr, 'country.name', 'iso3c'),
         origin_corr = recode(origin, `Central African Rep.` = 'Central African Republic'),
         origin_code = countrycode::countrycode(origin_corr, 'country.name', 'iso3c'))




df.distances <- read_tsv('distances.csv') %>%
  do({
    rbind(.,
          mutate(.,
                 tmp = country1,
                 country1 = country2,
                 country2 = tmp) %>%
            select(-tmp)
    )
  }) %>%
  mutate(from_code = countrycode::countrycode(country1, origin='country.name', destination='iso3c'),
         to_code = countrycode::countrycode(country2, origin='country.name', destination='iso3c')) %>%
  filter(!is.na(from_code), !is.na(to_code))









df %>%
  filter(year == 2016) %>%
  filter(grepl('^Refugees', type) | grepl('^Asylum-seekers', type)) %>%
  
  # Save the large (> quarter million) ones
  group_by(origin) %>%
  filter(sum(n) > 250000) %>%
  ungroup() %>%
  
  left_join(df.distances, by=c('origin_code'='from_code', 'residence_code'='to_code')) %>%
  group_by(origin, residence) %>%
  summarize(n=sum(n),
            distance = first(distance)) %>%
  mutate(neighbor = distance <= 0, # Only includes shared land borders, i.e. not neighbors separated by sea
         residence = ifelse(neighbor, residence, NA)) %>% # Merge non-neighbors
  
  group_by(origin, neighbor, residence) %>%
  summarize(n = sum(n)) %>%
  
  group_by(origin) %>%
  arrange(origin, ifelse(neighbor, 0, 1), is.na(residence), -n) %>%
  
  mutate(start.n = cumsum(n),
         start.n = lag(start.n, default=0)) %>% # To place labels
  arrange(start.n) %>%
  mutate(residence = gsub(' \\(.*\\)', '', residence)) %>%
  
  {
    ggplot(., aes(reorder(origin, n, FUN=sum), n, fill=ifelse(neighbor, '1', '0'))) +
      geom_col(size=0.2, color='white') +
      geom_text(data=filter(., !is.na(residence) & n > 250000), aes(label=paste0(' ', residence), y=start.n), family='Lato', color='white', hjust=0, size=2) +
      scale_y_continuous(breaks=0:6*1e6, labels=function(x) { paste0(x/1e6, ifelse(x > 0, 'M', ''))}, position='right') +
      scale_fill_manual(values=c('1' = '#374845', '0'='#93ACA7')) +
      coord_flip() +
      labs(x="", y="", caption="@hnrklndbrg | Source: UNHCR") +
      theme_henrik(grid='X', legend.position = 'none') 
  }

ggsave('/tmp/out.svg', width=9, height=5)