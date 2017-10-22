library(tidyverse)
library(vcd)

# Makeover Monday w 43

# Instructions at http://www.makeovermonday.co.uk/data/
# Original at http://www.myersbriggs.org/my-mbti-personality-type/my-mbti-results/how-frequent-is-my-type.htm
# Data from https://onedrive.live.com/view.aspx?resid=43EBDBC5D5265516!11541&ithint=file%2cxlsx&app=Excel&authkey=!AP7iHMB-Hvf5gPM


df <- readxl::read_xlsx('/tmp/MyersBriggsTypes.xlsx')
names(df) <- c('EI', 'SN', 'TF', 'JP', 'p')

# I'm sure there's a way to get vcd::mosaic to use the pre-calculated proportions, but hey...
df.tmp <- df %>%
  sample_n(10000, replace=TRUE, weight = p) %>%
  mutate(full = paste0(EI, SN, TF, JP)) %>%
  mutate(EI = factor(EI, ordered=TRUE, levels=c('I', 'E')),
         SN = factor(SN, ordered=TRUE, levels=c('S', 'N')),
         TF = factor(TF, ordered=TRUE, levels=c('F', 'T')))


svg(filename='/tmp/out.svg',
    width=5,
    height=5)
mosaic( ~ EI + SN + TF + JP, highlighting_fill = full, data=df.tmp)
dev.off()
