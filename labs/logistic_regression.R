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



# Credit Card Default
# import from excel using the readxl package contained in tidyverse
library(readxl)
# Notice that we have to skip the first row!
credit <- read_xls('~/econ224/labs/default of credit card clients.xls', skip = 1)
names(credit)

#------------------------ Data Documentation
# https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients#
#------------------------
# This research employed a binary variable, default payment (Yes = 1, No = 0), as the response variable. This study reviewed the literature and used the following 23 variables as explanatory variables:
#  X1: Amount of the given credit (NT dollar): it includes both the individual consumer credit and his/her family (supplementary) credit.
#  X2: Gender (1 = male; 2 = female).
#  X3: Education (1 = graduate school; 2 = university; 3 = high school; 4 = others).
#  X4: Marital status (1 = married; 2 = single; 3 = others).
#  X5: Age (year).
#  X6 - X11: History of past payment. We tracked the past monthly payment records (from April to September, 2005) as follows: X6 = the repayment status in September, 2005; X7 = the repayment status in August, 2005; . . .;X11 = the repayment status in April, 2005. The measurement scale for the repayment status is: -1 = pay duly; 1 = payment delay for one month; 2 = payment delay for two months; . . .; 8 = payment delay for eight months; 9 = payment delay for nine months and above.
# X12-X17: Amount of bill statement (NT dollar). X12 = amount of bill statement in September, 2005; X13 = amount of bill statement in August, 2005; . . .; X17 = amount of bill statement in April, 2005.
# X18-X23: Amount of previous payment (NT dollar). X18 = amount paid in September, 2005; X19 = amount paid in August, 2005; . . .;X23 = amount paid in April, 2005. 

credit %>% rename(default = `default payment next month`)

credit$`default payment next month`







