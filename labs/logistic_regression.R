library(tidyverse)
library(haven)
bm <- read_dta('~/econ224/labs/lakisha_aer.dta')


sort(names(bm))

bm <- bm %>% 
  mutate(black = 1 * (race == 'b'), female = 1 * (sex == 'f')) %>%
  select(call, black, female, yearsexp, volunteer, military, email, empholes, 
         workinschool, computerskills, specialskills)

reg1 <- glm(call ~ yearsexp, family = 'binomial', data = bm)
summary(reg1)

Lambda <- function(x) {
  1 / (1 + exp(-x))
}

# The default is to give the linear predictor (log odds)
predict(reg1, newdata = data.frame(yearsexp = 0))
# Optionally, give on the scale of the response (i.e. predicted prob)
predict(reg1, newdata = data.frame(yearsexp = 0), type = 'response')

# Verify by hand
coef(reg1)[1]
Lambda(coef(reg1)[1])

# Plot probabilities for persons with 0, 1, 2, ..., 20 years of experience
p <- predict(reg1, newdata = data.frame(yearsexp = 0:9), type = 'response')
plot(0:9, p)

#------------------------ Try using the Titanic dataset
# Data description: http://ditraglia.com/econ103/titanic3.txt

# Maybe this is a good lab to use for teaching them the "test" versus 
# "training" dataset idea. Then we could calculate the specificity and
# sensitivity out of sample... 

titanic <- read_csv('http://www.ditraglia.com/econ103/titanic3.csv')

reg1 <- glm(survived ~ age, family = 'binomial', data = titanic)
summary(reg1)

reg2 <- glm(survived ~ age + I(age^2), family = 'binomial', data = titanic)
summary(reg2)

# pclass should really be treated as categorical...
reg3 <- glm(survived ~ factor(pclass), family = 'binomial', titanic)
summary(reg3)

reg4 <- glm(survived ~ fare, family = 'binomial', titanic)
summary(reg4)

reg5 <- glm(survived ~ fare + I(fare^2), family = 'binomial', titanic)
summary(reg5)

reg6 <- glm(survived ~ fare + factor(pclass), family = 'binomial', titanic)
summary(reg6)

reg7 <- glm(survived ~ fare + sex + factor(pclass) +
              sex:factor(pclass) + sex:fare, family = 'binomial', titanic)
summary(reg7)

reg8 <- glm(survived ~ fare + sex + fare:sex, family = 'binomial', titanic)
summary(reg8)


summary(titanic$fare)
x <- seq(0, 500, 0.1)
p <- predict(reg4, newdata = data.frame(fare = x), type = 'response')
plot(x, p, type = 'l', ylim = c(0, 1))
points(titanic$fare, titanic$survived)

titanic %>%
  ggplot(aes(x = fare, y = survived)) +
  geom_jitter(height = 0.2, width = 5) +
  stat_smooth(method='glm', 
              method.args = list(family = "binomial"),
              formula = y ~ x) + 
  xlab('Fare (Pounds Stering)') +
  ylab("P(Survive)")

titanic %>%
  ggplot(aes(x = fare, y = survived, color = sex)) +
  geom_jitter(height = 0.2, width = 5) +
  stat_smooth(method='glm', 
              method.args = list(family = "binomial"),
              formula = y ~ x) + 
  xlab('Fare (Pounds Stering)') +
  ylab("P(Survive)") +
  xlim(c(0, 150))

# Predict survival for different individuals...
