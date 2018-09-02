# Code for processing the data to get it in the form in which it appears on
# my website
library(tidyverse)
data_url <- 'https://fairmodel.econ.yale.edu/vote2012/football.txt'
# There are 1582 records. For some reason half of the columns appear *below*
# the other data so we have to read them separately:
football1 <- read_table(data_url, n_max = 1582)
football2 <- read_table(data_url, skip = 1582 + 1) # skip header and first 1582
football <- full_join(football1, football2)
football$ID <- NULL
write_csv(football, 'fair_football.csv')
rm(list = ls())

football <- read_csv('fair_football.csv')



# In games with a home team (i.e. not bowl games) how often does the home
# team win?
football %>%
  filter(H != 0) %>%
  mutate(Hwin = SPREAD * H > 0) %>%
  summarize(mean(Hwin))

# In games with a home team (i.e. not bowl games) how many more points does the
# home team score on average?
football %>%
  filter(H != 0) %>%
  summarize(mean(SPREAD * H))

# Regression to predict SPREAD using H *without* a constant
reg1 <- lm(SPREAD ~ H - 1, football)
summary(reg1)

# Explain why it makes sense *not* to include a constant. Hint: which team was
# designated A and which was designated B was *arbitrary*. How does it affect
# the regression prediction?

# Plot each of the seven systems one another using ggpairs                   
library(GGally)
football %>%
  select(MAT:REC) %>%
  ggpairs
''

# Does each ranking system contain independent information?
# (Regression *without* an intercept)
reg1 <- lm(SPREAD ~ H + MAT + SAG + BIL + COL + MAS + DUN + REC - 1, football)
summary(reg1)

# Neither MAT nor MAS are significant individually. What about jointly?
library(car)
linearHypothesis(reg1, c('MAT = 0', 'MAS = 0'))

# The preceding results suggest that MAT and MAS do not add additional predictive
# information beyond that contained in the other predictors, so it makes sense
# to try a regression that doesn't include them:
reg2 <- lm(SPREAD ~ H + SAG + BIL + COL + DUN + REC - 1, football)
summary(reg2)

# The preceding results suggest that MAT and MAS do not add additional predictive
# information beyond that contained in the other predictors, so it makes sense
# to try a regression that doesn't include them:
reg2 <- lm(SPREAD ~ H + SAG + BIL + COL + DUN + REC - 1, football)
summary(reg2)

# Once we add the Las Vegas line point spread (LV) nothing else is significant!
reg3 <- lm(SPREAD ~ LV + H + SAG + BIL + COL + DUN + REC - 1, football)
summary(reg3)
linearHypothesis(reg3, c('H = 0', 'SAG = 0', 'BIL = 0', 'COL = 0', 'DUN = 0',
                        'REC = 0'))

# How well does LV predict on its own? Can we reject the null that the coef is 1?
# What would it mean for this coef to equal 1? Make a plot.
reg4 <- lm(SPREAD ~ LV - 1, football)
summary(reg4)
linearHypothesis(reg4, c('LV = 1'))
ggplot(football, aes(x = LV, y = SPREAD)) +
  geom_point() +
  geom_smooth(method = 'lm')






