lambda <- 1.1
N0 <- 10

time.series <- c()
for (t in 0:10) {
  time.series[t+1] <- (lambda^t)*N0
}

plot(time.series)

for (i in 1:5) {
  a <- i ^ 2
  print(a)
}

plot(a)

b <- (1:10)^2
plot(b)

result <- 1
i <- 1
while (i <= 6) {
  result <- result * i
  print(result)
  i <- i + 1
}

result <- 1
for (i in 1:6) {
  result <- result * i
  print(result)
}

results <- c(1)
res <- c()
for (i in 1:6) {
  results <- results * i
  res[i] <- results
}
res
plot(res)

last

time.series <- c(100)
lambda <- 0.98
t <- 1
# produce a time series that starts at N0
# and stops once the population drops below 1
while(tail(time.series, 1) > 1){
  time.series[t+1] <- time.series[t]*lambda
  t <- t + 1
}

plot(time.series)

time.series[50]

lessthantwenty <- which(time.series < 20)
lessthantwenty[1]

square.number <- function(x){
  x^2
}

circle.area <- function(r){
  pi*r^2
}

circle.area(4)

triangle.area <- function(height, width){
  (height*width)/2
}
triangle.area(4, 5.5)

is.x.three <- function(x){
if (x == 3) "the value of x is 3" else "the value of x is not 3"
}

is.x.three(3)
  
sign_of_a_number <- function(x){
  if (x < 0) {print("Negative number")} else
    if (x > 0) {print("Positive number")} else
      print("Zero")
}

sign_of_a_number(-1)

my.factorial <- function(n){
  result <- 1
  i <- 1
  while (i <=  n){
    result <- result * i
    i <- i + 1
  }
  result
}

my.factorial(7)

time.series <- function(N0, lambda, tmax){
  population <- c(N0)
  for (t in 1:tmax) {
    population[t+1] <- population[t]*lambda 
  }
  return(population)
}

(data <- time.series(10, 0.95, 100))

plot(data)
plot(log(data))




rnorm(4, mean = 1, sd = 5)

rbinom(n = 1, size = 1, p = 1-0.5)

runif(1)
runif(1, 0, 10)


rnorm(1)

#################################
time.series <- function(N0, tmax, possible_outcomes, probabilities){
  population <- c(N0)
  for (t in 1:tmax) {
    lambda <- sample(possible_outcomes, 1, probabilities, replace=TRUE)
    population[t+1] <- population[t]*lambda
  }
  return(population)
}


lambda_mean <- 1.01
deviation <- 0.1
lambda1 <- (lambda_mean + deviation)
lambda2 <- (lambda_mean - deviation)
possible_outcomes <- c(lambda1, lambda2)
probabilities <- c(0.5, 0.5)
N0 <- 10

time.series.ext <- function(N0, tmax, possible_outcomes, probabilities){
  population <- c(N0)
  for (t in 1:tmax) {
  lambda <- sample(possible_outcomes, 1, probabilities, replace=TRUE)
  population[t+1] <- if (population[t]*lambda < 1) 0 else population[t]*lambda
  }
  return(population)
}

sim <- 500
tmax <- 200
data <- matrix(nrow = tmax +1, ncol = sim)

for (i in 1:sim) {
  data[, i] <- time.series.ext(N0, tmax, possible_outcomes, probabilities)
}



time.series.lambdamean <- function(N0, tmax, lambda_mean){
  population <- c(N0)
  for (t in 1:tmax) {
    population[t+1] <- if (population[t]*lambda_mean < 1) 0 else population[t]*lambda_mean
  }
  return(population)
}

lambda.geo <- sqrt(lambda_mean^2 - deviation^2)

time.series.geo <- function(N0, tmax, lambda_geo){
  population <- c(N0)
  for (t in 1:tmax) {
    population[t+1] <- if (population[t]*lambda.geo < 1) 0 else population[t]*lambda.geo
  }
  return(population)
}

log.row.means <- rowMeans(log(data))

data.mean <- time.series.lambdamean(N0, tmax, lambda_mean)
data.geo <- time.series.geo(N0, tmax, lambda_mean)

matplot(log(data+1), type = "l", lty = 1, xlab = "time", ylab = "ln(population size)")
matlines(rowMeans(log(data+1)), type = "l", lwd = 2, lty = 1, col = "red")
matlines(log(data.mean+1), type = "l", lty = 1, lwd = 2, col = "blue")
matlines(log(data.geo+1), type = "l", lty = 1, lwd = 2, col = "black")


#########################

matplot(log(data), type = "l", lty = 1, xlab = "time", ylab = "population size")
matlines(log.row.means, type = "l", lty = 1, lwd = 2, col = "red")
matlines(log(data.mean), type = "l", lty = 1, lwd = 2, col = "blue")
matlines(log(data.geo), type = "l", lty = 1, lwd = 2, col = "black")
  







