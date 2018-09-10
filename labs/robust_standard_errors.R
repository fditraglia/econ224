# This script demonstrates two different ways of getting robust standard
# errors in R and shows that they produce the same result if we specify
# the same covariance matrix formula:
#       lm_robust from estimatr
#       coef_test from lm_test combined with hccm from car
library(tidyverse)
library(lmtest)
library(car)
library(estimatr)
library(ggplot2)

set.seed(4321)
n <- 100
x <- runif(n)
e1 <- rnorm(n, mean = 0, sd = sqrt(2 * x))
e2 <- rnorm(n, mean = 0, sd = 1)
intercept <- 0.2
slope <- 0.9
y1 <- intercept + slope * x + e1
y2 <- intercept + slope * x + e2
mydat <- tibble(x, y1, y2)

ggplot(mydat, aes(x, y1)) +
  geom_smooth(method = 'lm') + 
  geom_point()
ggplot(mydat, aes(x, y2)) +
  geom_smooth(method = 'lm') +
  geom_point()
reg1 <- lm(y1 ~ x)
coeftest(reg1)
coeftest(reg1, vcov = hccm(reg1, type = 'hc1'))
reg1_robust <- lm_robust(y1 ~ x, se_type = 'stata')
summary(reg1_robust)
reg1_robust <- lm_robust(y1 ~ x, se_type = 'classical')
summary(reg1_robust)
