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
