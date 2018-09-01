library(tidyverse)
data_url <- 'https://fairmodel.econ.yale.edu/vote2012/football.txt'
# There are 1582 records. For some reason half of the columns appear *below*
# the other data so we have to read them separately:
football1 <- read_table(data_url, n_max = 1582)
football2 <- read_table(data_url, skip = 1582 + 1) # skip header and first 1582
football <- full_join(football1, football2)
write_csv(football, 'fair_football.csv')
rm(list = ls())
