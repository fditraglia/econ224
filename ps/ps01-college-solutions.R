# install.packages('ISLR')
library(ISLR)
library(tidyverse)

college <- as.tibble(College, rownames = 'name')

college <- college %>% mutate(selectivity = Accept / Apps, 
                              matricRate = Enroll / Accept,
                              sticker = Outstate + Room.Board + Books + Personal)

# What are the 5 most selective? Are any of them public?
college %>% 
  select(name, selectivity, Private) %>%
  arrange(selectivity)

# What are the 5 most selective public?
college %>% 
  filter(Private == 'No') %>%
  select(name, selectivity) %>%
  arrange(selectivity)
  
# What are the 5 with the highest matriculation rate? Are any of them public?
college %>% 
  select(name, matricRate, Private) %>%
  arrange(desc(matricRate))

# Make a scatterplot of selectivity versus matriculation rate using different colors to indicate public and private. Be sure to add an appropriate title to your plot
ggplot(college) + 
  geom_point(aes(x = selectivity, y = matricRate, color = Private)) +
  ggtitle('Matriculation Rate vs. Selectivity')

# Calculate the sample correlation between selectivity and matriculation rate separately for public and private. Discuss briefly.
college %>% 
  group_by(Private) %>%
  summarize(cor(selectivity, matricRate))


# Sticker price and instructional expenditure per student by selectivity decile





