# Some R Programming
# Learned basic R programming in Econ 103, e.g. how to write a function, basic 
# operations with vectors, etc.
# Now cover a few other basic programming facilities in R.

# if/else
is_positive <- function(x) {
 if(x > 0){
   return(TRUE)
 } else {
   return(FALSE)
 }
}
is_positive(3)
is_positive(0)
is_positive(-2)

# You don't have to have an else
g <- function(x) {
    y <- 0
  if(x > 3) {
    y <- y + 2
  }
  return(y)
}
g(2)
g(3)
g(3.01)

# Write a function called mysqrt to take square roots. If the input is negative
# it should print out "invalid input" otherwise it should return the square root.
# Test it out on several values
mysqrt <- function(x) {
  if(x >= 0) {
    return(sqrt(x))
  } else {
    return('Invalid Input!')
  }
}
mysqrt(3)
mysqrt(-2)

# for loop
x <- rep(NA_real_, 10) # create empty numeric vector
x
for(i in 1:length(x)) {
  x[i] <- 2 * i
}
x

# Write a function called f that takes a vector x and uses it to generate and
# return a new vector y as follows:
#    y[j] <- x[j] + 2 * j
# vector x add 2 * j
f <- function(x) {
  n <- length(x)
  y <- rep(NA_real_, n)
  for(j in 1:n) {
    y[j] <- x[j] + 2 * j
  }
  return(y)
}
f(1:5)

# Some other useful things: sort versus order
x <- c(101, 99, 100, 98, 102)
sort(x)
order(x)

# Find locations of two smallest values of x
order(x)[1:2]
x[c(20, 19)]
sort(x)[1:2]


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
