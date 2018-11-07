library(tidyverse)
library(haven)
mlda <- read_dta('~/econ224/labs/mlda.dta')

mlda

# Center age around the cutoff
mlda <- mlda %>% mutate(age = agecell - 21,
                        over21 = 1 * (agecell >= 21))

# Linear Regression Discontinuity, all causes mortality
linear <-  lm(all ~ over21 + age + age:over21, mlda)
summary(linear)

# Reproduce Figure 4.2 (not quite: that figure assumes common slope!)
make_RD_plot <- function(reg, dat, inc = 0.01) {
  plot(all ~ agecell, dat, xlab = 'Age', 
       ylab = 'Mortality Rate (per 100,000)',
       pch = 20, col = 'blue')
  abline(v = 21, lty = 2)
  x_min <- min(dat$agecell)
  x_max <- max(dat$agecell)
  x_before <- seq(x_min, 21 - inc, inc)
  x_after <- seq(21, x_max, inc)
  y_before <- predict(reg, data.frame(age = x_before - 21,  
                                         over21 = 1 * (x_before >= 21)))
  y_after <- predict(reg, data.frame(age = x_after - 21,  
                                        over21 = 1 * (x_after >= 21)))
  points(x_before, y_before, type = 'l', lwd = 2)
  points(x_after, y_after, type = 'l', lwd = 2)
}

make_RD_plot(linear, mlda)

# How does it change if you use quadratic?
quadratic <-  lm(all ~ over21 + age + I(age^2) +  
                   age:over21 + I(age^2):over21, mlda)
summary(quadratic)
make_RD_plot(quadratic, mlda)


# Previous results use everyone from age 19-23. How do the results change if you restrict attention to ages 20-22? Do both linear and quadratic and make new plots.
mlda_subset <- mlda %>% filter(agecell >= 20 & agecell <= 22)

linear2 <-  lm(all ~ over21 + age + age:over21, mlda_subset)
summary(linear2)
make_RD_plot(linear2, mlda_subset)

quadratic2 <-  lm(all ~ over21 + age + I(age^2) + 
                    age:over21 + I(age^2):over21, mlda_subset)
summary(quadratic2)
make_RD_plot(quadratic2, mlda_subset)


