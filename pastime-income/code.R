library(tidyverse)

theme_henrik <- function(grid=TRUE, legend.position=NA, base_family='Lato Light', highlight_family='Lato') {
  #th <- ggplot2::theme_minimal(base_family = 'LM Roman Dunhill 10', base_size = 13)
  #th <- ggplot2::theme_minimal(base_family = 'Playfair Display', base_size = 13)
  #th <- ggplot2::theme_minimal(base_family = 'Lato Light', base_size = 13)
  th <- ggplot2::theme_minimal(base_family = base_family, base_size = 13)
  
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
  
  th <- th + theme(plot.title = element_text(family=highlight_family),
                   plot.subtitle = element_text(margin=margin(b=15)),
                   plot.caption = element_text(face='italic', size=10))
  
  if (!is.na(legend.position)) th <- th + theme(legend.position = legend.position)
  
  return (th)
}






# Read data
# (Some of this not used in this particular article)
# Data from https://www.kaggle.com/bls/american-time-use-survey


library(tidyverse)

df.who <- read_csv('../data/atus/atuswho.csv') %>%
  filter(tuwho_code > -0)

df.act <- read_csv('../data/atus/atusact.csv')

df.sum <- read_csv('../data/atus/atussum.csv')

df.resp <- read_csv('../data/atus/atusresp.csv')

df.whocode <- tribble(~tuwho_code, ~who, ~category,
                      18, 'Alone', 'Alone',
                      19, 'Alone', 'Alone',
                      20, 'Spouse', 'Partner',
                      21, 'Unmarried partner', 'Partner',
                      22, 'Own household child', 'Children',
                      23, 'Grandchild', 'Children',
                      24, 'Parent', 'Family',
                      25, 'Brother/sister', 'Family',
                      26, 'Other related person', 'Family',
                      27, 'Foster child', 'Children',
                      28, 'Housemate/roommate', 'Friend',
                      29, 'Roomer/boarder', 'Friend',
                      30, 'Other nonrelative', 'Other',
                      40, 'Own nonhousehold child < 18', 'Children',
                      51, 'Parents (not living in household)', 'Family',
                      52, 'Other nonhousehold family members < 18', 'Children',
                      53, 'Other nonhousehold family members 18 and older (including parents-in-law)', 'Family',
                      54, 'Friends', 'Friend',
                      55, 'Co-workers/colleagues/clients', 'Co-worker',
                      56, 'Neighbors/acquaintances', 'Other',
                      57, 'Other nonhousehold children < 18', 'Other',
                      58, 'Other nonhousehold adults 18 and older', 'Other',
                      59, 'Boss or manager', 'Co-worker',
                      60, 'People whom I supervise', 'Co-worker',
                      61, 'Co-workers', 'Co-worker',
                      62, 'Co-workers', 'Other',
                      NA, 'Unknown', 'Unknown'
)

df.wherecode <- tribble(~tewhere, ~where, ~where_category,
                        1, "Respondent's home or yard", "Home",
                        2, "Respondent's workplace", "Work",
                        3, "Someone else's home", "Other",
                        4, "Restaurant or bar", "Restaurant",
                        5, "Place of worship", "Other",
                        6, "Grocery store", "Store",
                        7, "Other store/mall", "Store",
                        8, "School", "Scool",
                        9, "Outdoors away from home", "Other",
                        10, "Library", "Other",
                        11, "Other place", "Other",
                        12, "Car, truck, or motorcycle (driver)", "Travel",
                        13, "Car, truck, or motorcycle (passenger)", "Travel",
                        14, "Walking",  "Travel",
                        15, "Bus", "Travel",
                        16, "Subway/train", "Travel",
                        17, "Bicycle", "Travel",
                        18, "Boat/ferry", "Travel",
                        19, "Taxi/limousine service", "Travel",
                        20, "Airplane", "Travel",
                        21, "Other mode of transportation", "Travel",
                        30, "Bank","Other",
                        31, "Gym/health club","Other",
                        32, "Post Office","Other",
                        89, "Unspecified place","Other",
                        99, "Unspecified mode of transportation", "Travel")

df.occcode <- tribble(~trdtocc1, ~occupation,
                      1, "Management occupations",
                      2, "Business and financial operations occupations",
                      3, "Computer and mathematical science occupations",
                      4, "Architecture and engineering occupations",
                      5, "Life, physical, and social science occupations",
                      6, "Community and social service occupations",
                      7, "Legal occupations",
                      8, "Education, training, and library occupations",
                      9, "Arts, design, entertainment, sports, and media occupations",
                      10, "Healthcare practitioner and technical occupations",
                      11, "Healthcare support occupations",
                      12, "Protective service occupations",
                      13, "Food preparation and serving related occupations",
                      14, "Building and grounds cleaning and maintenance occupations",
                      15, "Personal care and service occupations",
                      16, "Sales and related occupations",
                      17, "Office and administrative support occupations",
                      18, "Farming, fishing, and forestry occupations",
                      19, "Construction and extraction occupations",
                      20, "Installation, maintenance, and repair occupations",
                      21, "Production occupations",
                      22, "Transportation and material moving occupations")


df.occrecode <- tribble(~trmjocc1, ~occupation,
                        1, "Management, business, and financial occupations",
                        2, "Professional and related occupations",
                        3, "Service occupations",
                        4, "Sales and related occupations",
                        5, "Office and administrative support occupations",
                        6, "Farming, fishing, and forestry occupations",
                        7, "Construction and extraction occupations",
                        8, "Installation, maintenance, and repair occupations",
                        9, "Production occupations",
                        10, "Transportation and material moving occupations"
)

df.cps <- read_csv('../data/atus/atuscps.csv')

df.actcode <- read_csv2('../data/atus/codes.csv') %>%
  mutate(activity = as.integer(code))


df <- df.act %>%
  left_join(df.who, by=c('tucaseid', 'tuactivity_n')) %>%
  inner_join(df.whocode, by='tuwho_code') 





# Aggregate



df.total <- df.sum %>%
  select(tucaseid, tufnwgtp) %>%
  inner_join(df.cps %>% filter(tulineno==1) %>% select(tucaseid, hufaminc, hefaminc), by='tucaseid') %>%
  mutate(income = ifelse(hufaminc >= 1, hufaminc, hefaminc)) %>% # Get income
  filter(income >= 1) %>%
  mutate(income = case_when(income < 4 ~ 1, # Aggregate income
                            income < 7 ~ 2,
                            income < 9 ~ 3,
                            income < 12 ~ 4,
                            income < 14 ~ 5,
                            income < 15 ~ 6,
                            income < 16 ~ 7,
                            income == 16 ~ 8)) %>%
  group_by(income=income) %>%
  summarize(n = sum(tufnwgtp)) %>%
  mutate(p = n/sum(n)) 

df.tmp <- df.act %>%
  inner_join(df.sum %>% select(tucaseid, tufnwgtp), by='tucaseid') %>%
  inner_join(df.cps %>% filter(tulineno==1) %>% select(tucaseid, hufaminc, hefaminc), by='tucaseid') %>%
  mutate(income = ifelse(hufaminc >= 1, hufaminc, hefaminc)) %>%
  
  filter(income >= 1) %>%
  mutate(income = case_when(income < 4 ~ 1,
                            income < 7 ~ 2,
                            income < 9 ~ 3,
                            income < 12 ~ 4,
                            income < 14 ~ 5,
                            income < 15 ~ 6,
                            income < 16 ~ 7,
                            income == 16 ~ 8)) %>%
  group_by(trtier1p, trtier2p, trcodep, income) %>%
  summarize(time = sum(tuactdur24 * tufnwgtp),
            n = sum(tufnwgtp)) %>%
  inner_join(df.actcode %>% transmute(trcodep=activity, activity = name), by=c('trcodep')) %>%
  inner_join(df.actcode %>% transmute(trtier2p=activity, subgroup = name), by=c('trtier2p')) %>%
  inner_join(df.actcode %>% transmute(trtier1p=activity, group = name), by=c('trtier1p')) 



df.tmp %>%
  filter(trtier1p %in% c(12, 13)) %>% # Socializing, Leisure and sports
  filter(!grepl('n\\.e\\.c', activity)) %>% # Remove "not elsewhere classified"
  filter(!grepl('^Waiting', activity)) %>% # Remove all "waiting associated with..."
  ungroup() %>%
  
  filter(as.integer(factor(activity, ordered=TRUE, levels=unique(activity[order(-time)]))) <= 50) %>%
  group_by(activity) %>%
  mutate(p = time/sum(time)) %>%
  #mutate(p = n/sum(n)) %>%
  ungroup() %>%
  transmute(group='activity', activity, p, income) %>%
  as.data.frame() %>%
  rbind(df.total %>% transmute(group = ' _MEDIAN_ ', activity='median', income, p)) %>%
  group_by(activity) %>%
  mutate(p.over = sum(ifelse(income > 4, p, 0))) %>%
  ungroup() %>%
  mutate(income.f = factor(income)) %>%
  mutate(activity.f = factor(activity, ordered=TRUE, levels=unique(activity[order(p.over)]))) %>%
  mutate(group=1) %>% # Disable faceting (code artifact)
  {
    ggplot(., aes(activity.f, p, fill=income.f)) +
      geom_bar(stat='identity', position=position_stack(reverse=TRUE), size=0.85) +
      geom_hline(data=df.total %>% ungroup() %>% arrange(income) %>% mutate(cum.p = cumsum(p)), aes(yintercept=cum.p), color='white') +
      coord_flip() +
      scale_fill_manual(values=c('1'='#0D3D81', '2'='#0766B3', '3'='#2C8EB7', '4'='#4AB3D2', '5'='#41A952', '6'='#238151', '7'='#006A32', '8'='#044326')) +
      scale_y_continuous(labels=scales::percent, breaks=NULL) +
      theme_henrik(grid = '', legend.position='none') +
      labs(x="", y="", title="Income distributions in Americans' pastimes", caption="@hnrklndbrg | Source: American Time Use Survey") +
      facet_grid(group ~ ., scales='free_y', space='free_y')
  }

ggsave('/tmp/test.svg', width=9, height=9)