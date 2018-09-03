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
