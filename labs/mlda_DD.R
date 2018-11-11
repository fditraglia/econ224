library(tidyverse)
library(haven)
library(estimatr)

mlda <- read_dta('~/econ224/labs/deaths.dta')


# The dataset is in a weird format: multiple outcome variables are all stored in the same column (mrate) by stacking them on top of each other. The variable dtype indicates which outcome mrate refers to. We will only look at dtype == 1 (all deaths) and agegr == 2 (18-20 years)

mlda <- mlda %>%
  filter(year <= 1983, agegr == 2, dtype == 1) %>%
  mutate(year = factor(year), state = factor(state))

# legal is the proportion of legal drinkers in the state/year

# Look only at all deaths: "all"
# We don't include an intercept because we instead want to have all the year effects (just so we know how to interpret all of the results)
reg1 <- lm_robust(mrate ~ legal + state + year - 1, 
                  data = mlda, clusters = state, se_type = 'stata')
reg2 <- lm_robust(mrate ~ legal + state + year + state:year - 1, 
                  data = mlda, clusters = state, se_type = 'stata')
reg3 <- lm_robust(mrate ~ legal + state + year - 1, 
                  data = mlda, weights =  pop, clusters = state, se_type = 'stata')

# This doesn't actually work: I don't know what STATA does in this case
reg4 <- lm_robust(mrate ~ legal + state + year + state:year - 1, 
                  data = mlda, weights =  pop, clusters = state, se_type = 'stata')

# There's way too much output here! 
# We just want to extract the estimate and std error for legal.  

# Try controlling for beer taxes
reg5 <- lm_robust(mrate ~ legal + beertaxa + state + year - 1,
                  data = mlda, clusters = state, se_type = 'stata')
summary(reg5)
