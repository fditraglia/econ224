# ------------ Part 1: College GPA

library(tidyverse)
gpa <- read_csv('college_gpa.csv')

# Carry out linear regression to predict colgpa using hsperc and sat
reg1 <- lm(colgpa ~ hsperc + sat, gpa)
summary(reg1)

# Predict colgap using hsize and its square
reg2 <- lm(colgpa ~ hsize + I(hsize^2), gpa)
summary(reg2)

# Add hsperc, sat, female, athlete
reg3 <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + sat + female + athlete, gpa)
summary(reg3)

# Allow effect of being an athlete to differ by sex
reg4 <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + sat + female + 
             athlete + athlete:female, gpa)
summary(reg4)

# Allow effect of sat to differ by sex
reg5 <- lm(colgpa ~ hsize + I(hsize^2) + hsperc + sat + female + 
             sat:female + athlete, gpa)
summary(reg5)


# ------------ Part 2: Implement KNN Regression

# Suppose we had another vector y with values that somehow correspond to x.
# E.g. in a regression setting where we are using x to predict y. Suppose we
# wanted to know the y-value that corresponds to the smallest value of x:
y <- c(1, 3, 2, 4, 5)
y[order(x)]
cbind(sort(x), y[order(x)])

# Implement KNN regression in one dimension: function should take as its inputs
# (x, y, K, newdata) 

knn_reg <- function(x, y, K, newdata = NULL) {
  if(is.null(newdata)) {
    newdata <- x
  }
  n <- length(newdata)
  yhat <- rep(NA_real_, n)
  for(i in 1:n) {
    dist_i <- abs(newdata[i] - x)
    N0 <- order(dist_i)[1:K]
    yhat[i] <- mean(y[N0])
  }
  return(yhat)
}


knn_plot <- function(K) {
  disp <- mtcars$disp
  mpg <- mtcars$mpg
  newdata <- seq(min(disp), max(disp), length.out = 1001)
  yhat <- knn_reg(disp, mpg, K, newdata)
  plot(y ~ x, main = bquote(K == .(K)))
  points(newdata, yhat, col = 'red', type = 'l')
}

par(mfrow = c(2, 2), mar = c(4,3,2,1), mgp = c(2,.7,0), tck = -0.01)
for(K in c(20, 10, 5, 1)) {
  knn_plot(K)
}
par(mfrow = c(1,1))
