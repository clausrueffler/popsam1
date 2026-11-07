lambda.mean <- 1.01
epsilon <- 0.05
N0 <- 10
tmax <- 10


lambda.good <- lambda.mean + epsilon
lambda.bad <- lambda.mean - epsilon

time.series <- c(N0)
time.series

env.stoch <- function(lambda.mean, epsilon, N0, tmax){
  lambda.good <- lambda.mean + epsilon
  lambda.bad <- lambda.mean - epsilon
  time.series <- c(N0)
  for (t in 1:tmax) {
    lambda <- sample(c(lambda.good, lambda.bad), 1, replace = TRUE, c(0.5, 0.5))
    time.series[t + 1] <- time.series[t]*lambda
  }
  time.series
}

data <- env.stoch(1.01, 0.05, 10, 200)
plot(data, type = "l", log = "y")

plot(log(data + 1), type = "l", log = "y")

sim <- 500
tmax <- 200
results <- matrix(nrow = tmax +1, ncol = sim)

for (i in 1:sim) {
  results[ , i] <- env.stoch(1.01, 0.05, 10, tmax)
}

matplot(log(results +1), type = "l")

hist(results[tmax + 1, ], breaks = seq(0, 500, 20))

hist(log(results[tmax + 1, ]+1), breaks = seq(0, 7, 0.5))

max(results[tmax + 1, ])
max(log(results[tmax + 1, ]+1))
