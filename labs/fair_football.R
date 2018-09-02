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


# Some basic manipulations with regression using the (boring) built-in dataset
# mtcars. Later in the problem set you'll work with a real and very interesting
# dataset!

#------- Simple linear regression
reg1 <- lm(mpg ~ disp, mtcars)
reg1

#------- Multiple linear regression
reg2 <- lm(mpg ~ disp + hp, mtcars)
reg2

#-------- Display more information with summary
summary(reg1) 
summary(reg2)

# lm returns a *list* containing lots of stuff, as does summary(...)
# To look inside you can use str. E.g. if you just want the standar error of
# the regression or the r-squared, or the fstatistic
str(summary(reg2))
summary(reg2)$sigma
summary(reg2)$r.squared
summary(reg2)$fstatistic

#-------- Regression *without an intercept*
# Don't do this unless you have a very good reason
lm(mpg ~ disp - 1, mtcars)

#-------- Regression with *only* an intercept
# What does this do?
lm(mpg ~ 1, mtcars)

#------- Extracting coefficients, residuals, etc
# We could do this by using the `$` with the result of lm, but there's an 
# easier way
coef(reg2)
head(resid(reg2))
ggplot() +
  geom_histogram(aes(x = resid(reg1)), binwidth = 1.5)

# Calculate residuals "by hand"
head(fitted.values(reg2))
all.equal(mtcars$mpg - fitted.values(reg2), resid(reg2))

# Calculate the residual standard error by hand and compare to the regression
# output
sqrt(sum(residuals(reg2)^2 / (length(residuals(reg2)) - 3)))


# How does the predict function work?
# If we don't give it any other information, it just returns the fitted values
all.equal(predict(reg1), fitted.values(reg1))
ggplot(mtcars) +
  geom_line(aes(x = disp, y = predict(reg1))) +
  geom_point(aes(x = disp, y = mpg))

# If we supply a dataframe "newdata" it will predict a new observation. This
# dataframe should have names to match those in the original regression
mydat <- data.frame(disp = c(100, 200, 300),
                    hp = c(150, 100, 200))
mydat
predict(reg2, newdata = mydat)

# Use coef to check that you get the same predictions "by hand" for mydat as you did using the predict function
b <- coef(reg2)
b0 <- b[1]
b1 <- b[2]
b2 <- b[3]
b0 + b1 * mydat$disp + b2 * mydat$hp 

# Some F-tests
# -------------- Run a regression with more predictors
reg3 <- lm(mpg ~ disp + hp + wt + cyl, mtcars)
# -------------- Joint significance of everything in the regression
# H0: b0 = b1 = ... = bp = 0
# H1: at least one of b1, ..., bp is non-zero
# -------------
# Using summary:
summary(reg3)
# By hand:
summary(reg3)$fstatistic
curve(expr = df(x, 4, 27), 0, 40, n = 1001,
      ylab = 'density',
      main = 'We would not expect an F(2,29) RV to spit out 37.8')
abline(v = 37.84413, lty = 2, col = 'red')
1 - pf(37.84413, 4, 27)

# Testing a subset of coefficients
library(car)
linearHypothesis(reg3, c('wt = 0', 'cyl = 0'))

# Generate fake data x ~ N(0,1) and z ~ N(0,1) and add them to reg3 to create
# reg4. Then F-test that these add no information for predicting mpg. Explain
# the results.
n <- nrow(mtcars)
x <- rnorm(n)
z <- rnorm(n)
reg4 <- lm(mpg ~ disp + hp + wt + cyl + x + z, mtcars)
linearHypothesis(reg4, c('x = 0', 'z = 0'))

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

# Regression to predict SPREAD using H:
reg1 <- lm(SPREAD ~ H - 1, football)
summary(reg1)

# How to interpret the coefficient on H? How much better does the home team do
# relative to the away team?
football %>%
  filter(H != 0) %>%
  mutate(Hspread = H * SPREAD) %>%
  summarize(mean(SPREAD))



                         
# Plot each of the six rankings against one another using ggpairs                         
library(GGally)
football %>%
  select(MAT:DUN) %>%
  ggpairs


# Some F-tests









