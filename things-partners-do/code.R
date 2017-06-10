library(tidyverse)

# Data from https://www.kaggle.com/bls/american-time-use-survey

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

df.actcode <- read_csv2('../data/atus/codes.csv') %>%
  mutate(activity = as.integer(code))

df <- df.act %>%
  left_join(df.who, by=c('tucaseid', 'tuactivity_n')) %>%
  inner_join(df.whocode, by='tuwho_code') 


# === Theme ====


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






# ==== Aggregate ==== 

df.tmp <- df %>%
  filter(who != 'Unknown') %>% # Remove activities where no participation is registered
  filter(!(trtier1p %in% c(18, 50))) %>% # Remove travel and uncodable
  inner_join(df.sum %>% select(tucaseid, teage, trsppres), by='tucaseid') %>%
  filter(trsppres != 3) %>% # Remove singles
  
  # Collapse into parther yes/no per activity
  group_by(tucaseid, trcodep) %>%
  summarize(partner = any(who %in% c('Spouse', 'Unmarried partner')),
            tuactdur24 = first(tuactdur24)) %>%
  
  # Sum by activity
  inner_join(df.actcode %>% rename(activity_name = name), by=c('trcodep' = 'activity')) %>%
  group_by(activity_name, partner) %>%
  summarize(time = sum(tuactdur24)) %>%
  
  # Rename ungly-ish categories; also collapses some activities into a single one
  # This list was built over time, starting with a bunch of gsub:s in the beginning
  # and ending with manual replacements for the final viz. I couldn't be bothered
  # to do it all in the same way.
  ungroup() %>%
  mutate(activity_name = gsub(', n\\.e\\.c.*', '', activity_name)) %>%
  mutate(activity_name = gsub(' \\(.*\\)', '', activity_name)) %>%
  mutate(activity_name = gsub(' hh ', ' ', activity_name)) %>%
  mutate(activity_name = gsub(' nonhh ', ' ', activity_name)) %>%
  mutate(activity_name = gsub(' HH ', ' ', activity_name)) %>%
  mutate(atcivity_name = gsub(', not sports', '', activity_name)) %>%
  mutate(activity_name = case_when(activity_name == 'Attending or hosting parties/receptions/ceremonies' ~ 'Attending social events',
                                   activity_name == 'Socializing and communicating with others' ~ 'Socializing and communicating',
                                   activity_name == 'Shopping, except groceries, food and gas' ~ 'Shopping',
                                   activity_name == 'Storing interior items, inc. food' ~ 'Storing items',
                                   activity_name == 'Playing with nonhh children' ~ 'Playing with children',
                                   activity_name == 'Household & personal organization and planning' ~ 'Household organization',
                                   activity_name == 'Interior arrangement, decoration, & repairs' ~ 'Interior decoration & repairs',
                                   activity_name == 'Participarion in religious practices' ~ 'Religious practices',
                                   activity_name == 'Research/homework for class for degree, certification, or licensure' ~ 'Homework',
                                   activity_name == 'Using health and care services outside the home' ~ 'Using health and care services',
                                   activity_name == 'Arts and crafts as a hobby' ~ 'Arts and crafts',
                                   activity_name == 'HH & personal e-mail and messages' ~ 'Personal e-mail and messages',
                                   activity_name == 'Working out, unspecified' ~ 'Working out',
                                   activity_name == 'Work, main job' ~ 'Work',
                                   activity_name == 'Work, other job(s)' ~ 'Work',
                                   activity_name == 'Attending meetings, conferences, & training' ~ 'Meetings, conferences, & training',
                                   activity_name == 'Appliance, tool, and toy set-up, repair, & maintenance' ~ 'Appliance maintenence',
                                   activity_name == 'Exterior repair, improvements, & decoration' ~ 'Exterior repair & decoration',
                                   activity_name == 'Sewing, repairing, & maintaining textiles' ~ 'Sewing',
                                   TRUE ~ activity_name)) %>%
  group_by(activity_name, partner) %>%
  summarize(time = sum(time)) %>%
  # The summarize() above removes the partner grouping; now aggregate by activity
  summarize(p = sum(time * partner) / sum(time),
            time = sum(time))


# ==== Plot =====

df.tmp %>%
  arrange(-time) %>%
  head(50) %>% # Top 50 activities by total time spent
  ggplot(aes(reorder(activity_name, p))) +
  geom_bar(aes(y = p), stat='identity', fill='#216778', width=0.85) +
  geom_bar(aes(y = p-1), stat='identity', fill='#8fbdc7', width=0.85) +
  geom_text(aes(label=paste0('', round(p*100), '%'), y = p), hjust=1.2, color='white', family='Lato', size=2.5) +
  scale_y_continuous(breaks=NULL, labels=function(x) {scales::percent(abs(x))}, limits=c(-1, 1)) +
  labs(x="", y="", title="Quality time", caption="@hnrklndbrg | Source: American Time Use Survey") +
  coord_flip() +
  theme_henrik(grid='X') 

ggsave('/tmp/test.svg', width=9, height=10)