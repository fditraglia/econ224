# install.packages('ISLR')
library(ISLR)
library(tidyverse)

college <- as.tibble(College, rownames = 'name')

# For simplicity, create all the extra variables used below at once
college <- college %>% 
  mutate(acceptRate = Accept / Apps,  
         matricRate = Enroll / Accept, 
         sticker = Outstate + Room.Board + Books + Personal)

# Which schools have the lowest acceptance rate? Are any of them public?
college %>% 
  select(name, acceptRate, Private) %>%
  arrange(acceptRate)

# Which public schools have the lowest acceptance rate?
college %>% 
  filter(Private == 'No') %>%
  select(name, acceptRate) %>%
  arrange(acceptRate)
  
# Make a scatterplot of acceptance versus matriculation rate using different 
# colors to indicate public and private. Be sure to add an appropriate title to 
# your plot
ggplot(college) + 
  geom_point(aes(x = acceptRate, y = matricRate, color = Private)) +
  ggtitle('Matriculation Rate vs. Acceptance Rate')

# Calculate the sample correlation between acceptance and matriculation rate 
# separately for public and private. Discuss briefly.
college %>% 
  group_by(Private) %>%
  summarize(cor(acceptRate, matricRate))

# Come up with your own question and answer it!
                           
