# four different but equivalent ways to produce a time series with environmental stochasticity

## vector of lambdas outside of for-loop

tmax <- 15 # length of the time series
lambda <- runif(tmax, 0.5, 1.5) # the assumed value for lambda
N0 <- 10 # initial population size
n <- c() # creates an empty vector in which we can store the data of the time series
n[1] <- N0 # stores the initial population size as the first entry of the vector n 
for(t in 1:tmax) {
  n[t+1] <- n[t] * lambda[t]
}
n
plot(n)

## new lambda determined inside the for-loop after population update, requires initial lambda value inside for-loop

tmax <- 15 # length of the time series
lambda <- runif(1, 0.5, 1.5) # the assumed value for lambda
N0 <- 10 # initial population size
n <- c() # creates an empty vector in which we can store the data of the time series
n[1] <- N0 # stores the initial population size as the first entry of the vector n 
for(t in 1:tmax) {
  n[t+1] <- n[t] * lambda
  lambda <- runif(1, 0.5, 1.5)
}
n
plot(n)

## lambda inside the for loop, before population update, maybe most elegant version

tmax <- 15 # length of the time series
N0 <- 10 # initial population size
n <- c() # creates an empty vector in which we can store the data of the time series
n[1] <- N0 # stores the initial population size as the first entry of the vector n 
for(t in 1:tmax) {
  lambda <- runif(1, 0.5, 1.5)
  n[t+1] <- n[t] * lambda
}
n
plot(n)

## same as above but replacing lambda directly with runif(1, min, max)

tmax <- 15 # length of the time series
N0 <- 10 # initial population size
n <- c() # creates an empty vector in which we can store the data of the time series
n[1] <- N0 # stores the initial population size as the first entry of the vector n 
for(t in 1:tmax) {
  n[t+1] <- n[t] * runif(1, 0.5, 1.5) # runif determines a value of lambda
}
n
plot(n)

# functions
##function computing the area of a circle

circle.area <- function(radius){
  radius^2 * pi
}

circle.area(radius = 2)

## function computing the area of a rectangle

rectangle.area <- function(width, length){
  width*length
}

rectangle.area(3, 5)

## function producing a deterministic time series

time.series <- function(tmax, lambda, N0){
  n <- c(N0) # creates a vector with the initial population size N0 in its first position
  for (t in 1:tmax) {
    n[t+1] <- n[t] * lambda
  }
  return(n)
}

n <- time.series(tmax = 20, lambda = 1.1, N0 = 10) # Here we export the values of the time series from the function.
# This allows us to plot the data since they data are now stored in the global environment (the environment tab)
plot(n)

## function producing a stochastic time series without extinction 

stoch.time.series <- function(N0, tmax, min, max){
  n <- c(N0)
  for(t in 1:tmax) {
    lambda <- runif(1, min, max)
    n[t+1] <- n[t] * lambda
  }
  return(n)
}

n <- stoch.time.series(10, 20, 0.5, 1.5)
plot(n)

## function producing a stochastic time series with extinction threshold equal to 1 

stoch.time.series <- function(N0, tmax, min, max){
  n <- c(N0)
  for(t in 1:tmax) {
    lambda <- runif(1, min, max)
    n[t+1] <- if (n[t] * lambda < 1) 0 else n[t] * lambda
  }
  return(n)
}

n <- stoch.time.series(10, 20, 0.5, 1.5)
plot(n)
