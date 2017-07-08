# This time, you can run this file and generate the chart with only files in this
# directory. 1_gen_data.R generates the activity.tsv file that is used in here, if you're
# looking to do similar analyses on other activtites.


library(tidyverse)
source('henrik.r') # for the horribly named theme_henrik

df <- read_tsv('activity.tsv')

df %>%
  group_by(activity) %>% 
  filter(max(p) > 3e-04, # Keep the most popular ones
         !grepl('n\\.e\\.c', activity)) %>% # Remove n.e.c. (not elsewhere classified)
  arrange(time) %>%
  mutate(p_peak = p / max(p), # Normalize as percentage of peak popularity
         p_smooth = (lag(p_peak) + p_peak + lead(p_peak)) / 3, # Moving average
         p_smooth = coalesce(p_smooth, p_peak)) %>% # When there's no lag or lead, we get NA. Use the pointwise data
  ungroup() %>%
  do({ # 24:00:00 is missing from the source data; add for a complete cycle
    rbind(.,
          filter(., time == 0) %>%
            mutate(time = 24*60))
  }) %>%
  mutate(time = ifelse(time < 3 * 60, time + 24 * 60, time)) %>% # Set start of chart to 03:00; few things overlap this hour
  
  mutate(activity = reorder(activity, p_peak, FUN=which.max)) %>% # order by peak time
  
  # This is some trickery to render our data in a way that lets the foreground data appear on top.
  # I'm sure there are reasons why this works, but I haven't bothered to figure out why. Also,
  # I suppose it might break at some point
  arrange(activity) %>%
  mutate(activity.f = reorder(as.character(activity), desc(activity))) %>%
  
  # Run it it's own block as we're doing some trickery with the activity levels
  {
    activities <- levels(.$activity)
    
    # Plot each activity on base Y as the integer of each factor, up to 2 "levels" high.
    # The two liner tricks mentioned a few lines above make this work; without the ribbons
    # renders on top of each others in a way that makes it look like nothing
    ggplot(., aes(time/60, group=activity.f, fill=factor(as.integer(activity.f) %% 2))) +
      geom_ribbon(aes(ymin = as.integer(activity), ymax = as.integer(activity) + 2 * p_smooth), color='white', size=0.4) +
      scale_x_continuous(breaks=seq(from = 3, to = 27, by = 3), labels = function(x) {sprintf("%02d:00", as.integer(x %% 24))}) +
      # "Re-add" activities by names as labels in the Y scale
      scale_y_continuous(breaks = 1:length(activities), labels = function(y) {activities[y]}) +
      
      # Zebra color for readability; will change colors of labels in Inkscape later
      scale_fill_manual(values = c('0' = '#2A7FFF', '1' = '#5599FF')) +
      labs(x="", y="", caption='@hnrklndbrg | Source: American Time Use Survey') +
      theme_henrik(grid='', legend.position='none') +
      theme(axis.ticks.x = element_line(size=0.3))
  }


ggsave('/tmp/out.svg', width=6, height=7)