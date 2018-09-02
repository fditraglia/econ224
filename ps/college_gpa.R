library(wooldridge)
data('gpa2')
?gpa2
#A data.frame with 4137 observations on 12 variables:
#sat: combined SAT score
#tothrs: total hours through fall semest
#colgpa: GPA after fall semester
#athlete: =1 if athlete
#verbmath: verbal/math SAT score
#hsize: size grad. class, 100s
#hsrank: rank in grad. class
#hsperc: high school percentile, from top
#female: =1 if female
#white: =1 if white
#black: =1 if black
#hsizesq: hsize^2

library(tidyverse)
gpa <- as_tibble(gpa2)
gpa$hsizesq <- NULL # remove hsizesq since they'll generate that!
write_csv(gpa, 'college_gpa.csv')

rm(list = ls())

gpa <- read_csv('college_gpa.csv')

# Carry out linear regression to predict colgpa using hsperc and sat
reg1 <- lm(colgpa ~ hsperc + sat, gpa)
summary(reg1)

# Add hsize, hsize squared, female, athlete
reg2 <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + sat + female + athlete, gpa)
summary(reg2)

# Allow effect of being an athlete to differ by sex
reg3 <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + sat + female + 
             athlete + athlete:female, gpa)
summary(reg3)

# Allow effect of sat to differ by sex
reg4 <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + sat + female + 
             sat:female + athlete, gpa)
summary(reg4)

# Carry out polynomial regression to predict sat using hsize and its square
reg3 <- lm(sat ~ hsize + I(hsize^2), gpa)
summary(reg3)

# Add female, black and their interaction
reg4 <- lm(sat ~ hsize + I(hsize^2) + female + black + female:black, gpa)
summary(reg4)
