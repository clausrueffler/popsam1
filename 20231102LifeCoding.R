## for-loop
tmax <- 100
lambda <- 1.1
N0 <- 10
n <- c(N0)
for(t in 1:tmax) {
  n[t +1] <- n[t]*lambda
  t <- t + 1
}
plot(n)

plot(log(n)) # plot of the logarithm of population size.
# On the log-scale, the time series becomes linear

## while-loop
# number of time steps as test condition
Nmax <- 100
lambda <- 1.1
t <- 1
N0 <- 10
n <- c(N0)
while(n[t] <= Nmax) {
  n[t +1] <- n[t]*lambda
  t <- t + 1
}
n
plot(n)

# population size as test condition
Nmax <- 100
lambda <- 1.1
t <- 1
N0 <- 10
n <- c(N0)
while(n[t] <= Nmax) {
  n[t +1] <- n[t]*lambda
  t <- t + 1
}
n
plot(n)

## functions
circle.area <- function(radius) {
  pi * radius^2
}
circle.area(pi)

rectangle.area <- function(length, height){
  length*height
}
rectangle.area(3,4)

cube.volume <- function(length, height, depth){
  length * height * depth
}
cube.volume(3,5,7)

# function for population dynamical time series

time.series <- function(lambda, N0, tmax){
  n <- c(N0)
  for (t in 1:tmax) {
    n[t + 1] <- n[t] * lambda
  }
  time.seris.data <<- n # this command exports the vector with the time series our of the local environment into the global environment
}

time.series(1.1, 10, 10)

plot(log(time.seris.data))

## Random numbers

# Californian condors survive from one year to the next with probability 0.85.
# Assume two condors are released into the wild.
# How many have survived to the next year?
rbinom(n = 1, size = 2, p = 0.85)

# A population consists of 5 females. The expected number of offspring per female is 2.
# How many offspring to they have if the number of offspring is Poisson distributed?
rpois(5, lambda = 2)
