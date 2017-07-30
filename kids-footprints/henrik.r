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
