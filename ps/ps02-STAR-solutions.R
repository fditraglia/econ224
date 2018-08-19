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
            sd_read = sd(g4reading, na.rm = TRUE))



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
            avg_read = mean(g4reading, na.rm = TRUE))

ggplot(star) +
  geom_boxplot(aes(x = factor(yearssmall), y = g4reading)) +
  ggtitle('Grade 4 Reading Score by Years in Small Class') + 
  xlab('Years') + ylab('Reading Score')

ggplot(star) +
  geom_boxplot(aes(x = factor(yearssmall), y = g4math)) +
  ggtitle('Grade 4 Math Score by Years in Small Class') + 
  xlab('Years') + ylab('Math Score')

# Test those who spent 4 years in small classes against those who spent 4 in regular
test_by_4small <- function(varname) {
  x_4small <- star %>%
    filter(yearssmall == 4) %>%
    pull(varname)
  x_4regular <- star %>%
    filter(yearssmall == 0) %>%
    pull(varname)
  t.test(x_4small, x_4regular)
}
test_by_4small('g4reading')
test_by_4small('g4math')


# Question 5
star <- star %>%
  mutate(minority = (race == 'black' | race == 'hispanic'))

star %>%
  group_by(kinder, minority) %>%
  summarize(avg_math = mean(g4math, na.rm = TRUE),
            avg_read = mean(g4reading, na.rm = TRUE))

test_by_minority <- function(varname) {
  x_minority <- star %>%
    filter(minority == TRUE) %>%
    pull(varname)
  x_white <- star %>%
    filter(minority == FALSE) %>%
    pull(varname)
  t.test(x_minority, x_white)
}

test_by_minority('g4reading')
test_by_minority('g4math')

# Question 6
test_by_kindersmall('hsgrad')
test_by_4small('hsgrad')
test_by_minority('hsgrad')













