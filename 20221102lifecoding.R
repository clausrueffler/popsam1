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

## function producing a stochastic time series

stoch.time.series <- function(N0, tmax, min, max){
  
}