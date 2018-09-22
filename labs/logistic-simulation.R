#-------------------------- Simulate data from a logistic regression
library(MASS)
set.seed(1234)
n <- 500
x <- rnorm(n, mean = 1.5, sd = 2)
b0 <- -0.5
b1 <- 1
ystar <- b0 + b1 * x + rlogis(n)
y <- 1 * (ystar > 0)
mydat <- data.frame(x, y)


#-------------------------- Plot true logistic regression function for the sim
Lambda <- function(x) {
  1 / (1 + exp(-x))
}
curve(Lambda(b0 + b1 * x), -8, 8, ylab = 'P(x)')


#-------------------------- Estimate the coefficients
lreg <- glm(y ~ x, mydat, family = binomial(link = 'logit'))
summary(lreg)
coef(lreg)

#--------------------------- Confidence interval for b0 and b1
-0.78119 + 2 * c(-1, 1) * 0.15389
1.07532 + 2 * c(-1, 1) * 0.09972


#-------------------------- Predicted probability that y = 1 when x = 0
# By hand:
bhat_0 <- coef(lreg)[1]
Lambda(bhat_0)

# Using predict:
predict(lreg, newdata = data.frame(x = 0), type = 'response')
# What happens if you don't specify type = 'response'?


#-------------------------- Predicted probability that y = 1 at the average x
# By hand:
bhat_1 <- coef(lreg)[2]
Lambda(bhat_0 + bhat_1 * mean(x))

# Using predict:
predict(lreg, newdata = data.frame(x = mean(x)), type = 'response')

#--------------------------- Marginal effect at average x
linear_predictor <- bhat_0 + bhat_1 * mean(x)
bhat_1 * exp(linear_predictor) / (1 + exp(linear_predictor))^2

# Divide by 4 rule
bhat_1 / 4

#--------------------------- Plot of "Jittered" data and the logistic function
library(ggplot2)
ggplot(mydat, aes(x, y)) +
  geom_jitter(height = 0.1) +
  stat_smooth(method='glm', 
              method.args = list(family = "binomial"),
              formula = y ~ x) 

  

  
  
  