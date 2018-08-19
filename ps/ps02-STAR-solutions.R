library(tidyverse)
library(ggplot2)

star <- read_csv('http://ditraglia.com/econ224/STAR.csv')

# Question 1
star <- star %>%
  mutate(kinder = case_when(classtype == 1 ~ 'small',
                            classtype == 2 ~ 'regular',
                            classtype == 3 ~ 'regular with aid'),
         race = case_when(race == 1 ~ 'white',
                          race == 2 ~ 'black',
                          race == 3 ~ 'asian',
                          race == 4 ~ 'hispanic',
                          race == 5 ~ 'native american',
                          race == 6 ~ 'other'),
         small = (kinder == 'small'))

# Question 2
star %>% 
  group_by(small) %>%
  summarize(avg_math = mean(g4math, na.rm = TRUE),
            sd_math = sd(g4math, na.rm = TRUE),
            avg_read = mean(g4reading, na.rm = TRUE),
            sd_read = sd(g4reading, na.rm = TRUE),
            p_grad = mean(hsgrad, na.rm = TRUE))

test_by_small <- function(varname) {
  x_small <- star %>% 
    filter(small == TRUE) %>%
    pull(varname)
  x_regular <- star %>%
    filter(small == FALSE) %>%
    pull(varname)
  t.test(x_small, x_regular)
}

test_by_kindersmall('g4math')
test_by_kindersmall('g4reading')
test_by_kindersmall('hsgrad')

# Question 3
star %>%
  group_by(small) %>%
  summarize(math33 = quantile(g4math, 1/3, na.rm = TRUE),
            math66 = quantile(g4math, 2/3, na.rm = TRUE),
            read33 = quantile(g4reading, 1/3, na.rm = TRUE),
            read66 = quantile(g4reading, 2/3, na.rm = TRUE))

# Question 4
star %>%
  group_by(kinder, yearssmall) %>%
  summarize(n())

star %>% 
  group_by(yearssmall) %>%
  summarize(avg_math = mean(g4math, na.rm = TRUE),
            avg_read = mean(g4reading, na.rm = TRUE),
            p_grad = mean(hsgrad, na.rm = TRUE))

ggplot(star) +
  geom_boxplot(aes(x = factor(yearssmall), y = g4reading))
  


















