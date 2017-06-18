# Pasted together from a larger analytics notebook.
# There is code in here not nescessary to generate the final image,
# but everything needed should be.
# Source data from https://www.kaggle.com/bls/american-time-use-survey


library(tidyverse)

df.who <- read_csv('../data/atus/atuswho.csv') %>%
  filter(tuwho_code > -0)

df.act <- read_csv('../data/atus/atusact.csv')

df.sum <- read_csv('../data/atus/atussum.csv')

df.resp <- read_csv('../data/atus/atusresp.csv')

df.cps <- read_csv('../data/atus/atuscps.csv')


df.whocode <- tribble(~tuwho_code, ~who, ~category,
                      18, 'Alone', 'Alone',
                      19, 'Alone', 'Alone',
                      20, 'Spouse', 'Family',
                      21, 'Unmarried partner', 'Family',
                      22, 'Own household child', 'Family',
                      23, 'Grandchild', 'Family',
                      24, 'Parent', 'Family',
                      25, 'Brother/sister', 'Family',
                      26, 'Other related person', 'Family',
                      27, 'Foster child', 'Family',
                      28, 'Housemate/roommate', 'Acqiantences',
                      29, 'Roomer/boarder', 'Acqiantences',
                      30, 'Other nonrelative', 'Other',
                      40, 'Own nonhousehold child < 18', 'Family',
                      51, 'Parents (not living in household)', 'Family',
                      52, 'Other nonhousehold family members < 18', 'Family',
                      53, 'Other nonhousehold family members 18 and older (including parents-in-law)', 'Family',
                      54, 'Friends', 'Acqiantences',
                      55, 'Co-workers/colleagues/clients', 'Co-worker',
                      56, 'Neighbors/acquaintances', 'Acqiantences',
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
                        2, "Respondent's workplace", "Work / School",
                        3, "Someone else's home", "Home",
                        4, "Restaurant or bar", "Restaurant",
                        5, "Place of worship", "Service",
                        6, "Grocery store", "Service",
                        7, "Other store/mall", "Service",
                        8, "School", "Work / School",
                        9, "Outdoors away from home", "Other",
                        10, "Library", "Service",
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
                        30, "Bank","Service",
                        31, "Gym/health club","Service",
                        32, "Post Office","Service",
                        89, "Unspecified place","Other",
                        99, "Unspecified mode of transportation", "Travel")


df.occcode <- tribble(~prdtocc1, ~occupation,
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


df.occrecode <- tribble(~prmjocc1, ~occupation,
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

df.occupation <- readxl::read_excel('../data/atus/2010_OccCodeswithCrosswalkfrom2002-2011nov04.xls', skip=4) %>%
  transmute(full_occupation = `Occupation 2010 Description`,
            code = `2010 Census Code`) %>%
  mutate(code = as.integer(code)) %>%
  filter(!is.na(code))

df.actcode <- read_csv2('../data/atus/codes.csv') %>%
  mutate(activity = as.integer(code))


df <- df.act %>%
  left_join(df.who, by=c('tucaseid', 'tuactivity_n')) %>%
  inner_join(df.whocode, by='tuwho_code') %>%
  inner_join(df.wherecode, by='tewhere')









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









df.tmp <- df.sum %>%
  filter(t050101 >= 60) %>% # Working days
  select(tucaseid, tufnwgtp, teage, tesex, tryhhchild, trsppres) %>%
  inner_join(df.cps %>% filter(tulineno == 1, prwkstat == 2) %>% select(tucaseid, prdtocc1, prmjocc1, peio1ocd)) %>%
  group_by(peio1ocd) %>%
  ungroup() %>%
  inner_join(df.act %>%
               select(tucaseid, tuactivity_n, tuactdur24, trtier1p, trtier2p, trcodep)) %>%
  inner_join(df.who %>% select(tucaseid, tuactivity_n, tuwho_code), by=c('tucaseid', 'tuactivity_n')) %>%
  inner_join(df.whocode %>% rename(who_category = category), by='tuwho_code') %>%
  inner_join(df.occupation, by=c('peio1ocd'='code')) %>%
  mutate(full_occupation = stringr::str_trim(full_occupation)) %>%
  inner_join(df.occrecode %>% rename(group_occupation = occupation), by='prmjocc1') %>%
  #inner_join(df.occcode %>% rename(group_occupation = occupation), by='prdtocc1') %>%
  group_by(tucaseid, who=who_category, tuactivity_n) %>%
  summarize(tufnwgtp = first(tufnwgtp), # A person can spend time with e.g. many children simultaneously. Collapse into one row
            tryhhchild = first(tryhhchild),
            teage = first(teage),
            tesex = first(tesex),
            trsppres = first(trsppres),
            tuactdur24 = first(tuactdur24),
            full_occupation = first(full_occupation),
            group_occupation = first(group_occupation)) %>%
  summarize(tufnwgtp = first(tufnwgtp), # Then summarize per person
            tryhhchild = first(tryhhchild),
            teage = first(teage),
            tesex = first(tesex),
            trsppres = first(trsppres),
            tuactdur24 = sum(tuactdur24),
            full_occupation = first(full_occupation),
            group_occupation = first(group_occupation))



summary(lm.out <- lm(tuactdur24 ~ 0 + full_occupation + splines::bs(teage - 40, 4) + factor(tesex)*I(tryhhchild >= 0)*factor(trsppres), weights=tufnwgtp, data=df.tmp %>% filter(who == 'Alone')))

df.tmp %>%
  filter(who == 'Alone') %>%
  ungroup() %>%
  modelr::data_grid(full_occupation, .model=lm.out) %>%
  modelr::add_predictions(lm.out) %>%
  inner_join(df.tmp %>% group_by(group_occupation, full_occupation) %>% summarize(n=n()), by='full_occupation') %>%
  arrange(full_occupation)%>%
  
  #manually edit list of jobs, kind of arbitrarily but for cleaner groupings
  mutate(full_occupation = gsub("First-line s", "S", full_occupation),
         full_occupation = case_when(grepl("Software developers", full_occupation) ~ "Software developers",
                                     grepl("Computer programmers", full_occupation) ~ "Software developers",
                                     grepl("^Operating engineers", full_occupation) ~ "Operating engineers",
                                     grepl("^Nursing.*aides$", full_occupation) ~ NA_character_,
                                     grepl("all other", full_occupation) ~ NA_character_,
                                     grepl("iscellaneous", full_occupation) ~ NA_character_,
                                     
                                     TRUE ~ full_occupation),
         group_occupation = case_when(grepl("teacher", full_occupation) ~ 'Kids',
                                      grepl("^Childcare", full_occupation) ~ 'Kids',
                                      grepl('ngineer', full_occupation) ~ 'Engineers',
                                      grepl("managers$", full_occupation) ~ "Managers",
                                      grepl("executives$", full_occupation) ~ NA_character_,
                                      grepl("^Supervisors", full_occupation) ~ "Supervisors",
                                      grepl("Education administrators", full_occupation) ~ "Office and administrative support occupations",
                                      grepl("Inspectors", full_occupation) ~ "Management, business, and financial occupations", # actually auding/
                                      grepl("Automotive .* mechanics$", full_occupation) ~ "Transportation and material moving occupations", 
                                      TRUE ~ group_occupation)) %>%
  filter(!is.na(full_occupation), !is.na(group_occupation)) %>%
  
  # Collapse groups that we've merged
  group_by(group_occupation, full_occupation) %>%
  summarize(n=sum(n),
            pred = mean(pred)) %>%
  ungroup() %>%
  top_n(50, n) %>% # largest n jobs
  group_by(group_occupation) %>%
  mutate(group_pred = sum(pred*n)/sum(n)) %>% # for sorting
  ungroup() %>%
  mutate(group_occupation = factor(group_occupation, ordered=TRUE, levels=unique(group_occupation[order(-group_pred)]))) %>%
  ggplot(aes(reorder(full_occupation, pred), pred)) +
    geom_bar(stat='identity', aes(fill=pred)) +
    geom_text(aes(label=sprintf("  %d:%02d", as.integer(floor(pred/60)), as.integer(pred %% 60)), y = 0), hjust=0, color='white', size=3, family='Lato', fontface='bold') +
    scale_fill_gradient(high='#286a8d', low='#4c9dc8') +
    scale_y_continuous(breaks=NULL) +
    labs(x="", y="", caption='@hnrklndbrg | Source: American Time Use Survey') +
    coord_flip() +
    theme_henrik(grid='', legend.position='none') +
    facet_grid(group_occupation ~ ., scales='free_y', space='free_y')


ggsave('/tmp/out.svg', width=8, height=10)






