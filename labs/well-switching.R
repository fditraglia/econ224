library(tidyverse)
library(ggplot2)
#wells <- read.table('~/econ224/labs/wells.dat')
#wells <- as.tibble(wells)
#write.csv(wells, '~/econ224/labs/wells.csv', row.names = FALSE)
#rm(wells)


wells <- read_csv('~/econ224/labs/wells.csv')

# dist = distance in meters to closest known safe well
# arsenic = arsenic level of respondent's well (100s of micrograms/liter)
# switch = 1 if switched to a new well
# assoc = 1 if any members of household are active in community organizations
# educ = education level of head of household (in years)

#---------------------- Histogram of arsenic levels in the unsafe wells
unsafe <- wells %>%
  filter(arsenic > 0.5)
ggplot(unsafe) + 
  geom_histogram(aes(x = arsenic)) + 
  xlab('Arsenic Concentration (100s of micrograms/liter)') + 
  ggtitle('Arsenic Concentrations in Unsafe Wells')

#----------------------- Logit regression using only distance to safe well
fit1 <- glm(switch ~ dist, family = binomial(link = 'logit'), wells)
summary(fit1)

#----------------------- Change distance to 100s of meters
wells <- wells %>%
  mutate(dist100 = dist / 100)

fit2 <- glm(switch ~ dist100, family = binomial(link = 'logit'), wells)
summary(fit2)

#----------------------- Plot histogram of dist100
ggplot(wells) + 
  geom_histogram(aes(x = dist100)) + 
  xlab('Distance to Nearest Safe Well (100s of Meters)')

#----------------------- Plot logistic with jittering
ggplot(wells, aes(x = dist100, y = switch)) +
  geom_jitter(height = 0.1) +
  stat_smooth(method='glm', 
              method.args = list(family = "binomial"),
              formula = y ~ x) + 
  xlab('Distance to Nearest Safe Well (100s of meters)') +
  ylab("P(Switch)")

#----------------------- Prob(switch) if dist100 = 0
# Using predict:
predict(fit2, newdata = data.frame(dist100 = 0), type = 'response')
# What happens if you don't specify type = 'response'

# By hand:
Lambda <- function(x) {
  1 / (1 + exp(-x))
}
Lambda(0.60596)

#---------------------- Prob(switch) at the avg of dist100
avgdist <- wells %>% 
  summarize(avgdist = mean(dist100)) %>% 
  pull(avgdist)
avgdist
# Using predict
predict(fit2, newdata = data.frame(dist100 = avgdist), type = 'response')
# By hand
Lambda(coef(fit2)[1] + coef(fit2)[2] * avgdist)


#------------------------ Marginal effect of dist100 at the average
b0 <- coef(fit2)[1]
b1 <- coef(fit2)[2]

linear_predictor <- b0 + b1 * avgdist

b1 / ((1 + exp(-1 * linear_predictor))^2)

# Divide by 4 rule
avgdist / 4

# Confidence interval for slope

fit3 <- glm(switch ~ dist100 + arsenic, wells, family = binomial(link = 'logit'))
summary(fit3)

# Plot the logistic decision boundary!
ggplot(wells) +
  geom_point(aes(x = dist100, y = arsenic, col = factor(switch))) +
  xlab('Distance to Nearest Safe Well (100s of meters)') +
  ylab('Arsenic Concentration (100s of micrograms/liter)')

# Show them how to graph a simple line using geom_abline()
coef(fit3)

