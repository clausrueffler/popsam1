lambda.mean <- 1.01
deviation <- 0.1
lambda1 <- (lambda.mean + deviation)
lambda2 <- (lambda.mean - deviation)
possible.outcomes <- c(lambda1, lambda2)
probabilities <- c(0.5, 0.5)
N0 <- 10 # initial population size
tmax <- 200 # length of the time series
sim <- 40 # number of simulation runs

environmental.stochasticity <- function(N0, tmax, possible.outcomes, probabilities) {
  time.series <- c(N0)
  for (t in 1:tmax) {
    lambda <- sample(possible.outcomes, 1, probabilities, replace = TRUE)
    time.series[t+1] <- time.series[t]*lambda
  }
  return(time.series)
}

# function with extinction threshold
environmental.stochasticity.ext <- function(N0, tmax, possible.outcomes, probabilities) {
  time.series <- c(N0)
  for (t in 1:tmax) {
    lambda <- sample(possible.outcomes, 1, probabilities, replace = TRUE)
    time.series[t+1] <- ifelse(time.series[t] * lambda < 1, 0, time.series[t] * lambda) # sets population size to 0 in case N drops below 1
  }
  return(time.series)
}

time.series.arith <- c(N0)
for (t in 1:tmax) {
  time.series.arith[t+1] <- time.series.arith[t]*lambda.mean
}

lambda.geo <- (lambda.mean^2-deviation^2)^(1/2)
time.series.geo <- c(N0)
for (t in 1:tmax) {
  time.series.geo[t+1] <- time.series.geo[t]*lambda.geo
}

# without extinction threshold
results <- matrix(nrow = tmax + 1, ncol = sim)
for (i in 1:sim) {
  results[, i] <- environmental.stochasticity(N0, tmax, possible.outcomes, probabilities) # produces a time series and stores it in the i-th column of the matrix results
}

# with extinction threshold
results <- matrix(nrow = tmax + 1, ncol = sim)
for (i in 1:sim) {
  results[, i] <- environmental.stochasticity.ext(N0, tmax, possible.outcomes, probabilities) # produces a time series and stores it in the i-th column of the matrix results
}


### plots
cl <- rainbow(sim)
## y-axis on normal scale
matplot(results, type="l", lty=1, ylim=c(0, max(results)), col=cl, xlab="time", ylab="population size")
row.means <- rowMeans(results)
lines(row.means, type="l", lwd = 2, col="Black")
lines(time.series.arith, type="l", lwd = 2, col="4")
lines(time.series.geo, type="l", lwd = 2, col="Black")

## on log scale
matplot(log(results+1), lty = 1, type = "l", ylim = c(0, max(log(results))), col = cl, xlab = "time", ylab = "ln(population size)")
row.means <- rowMeans(log(results + 1))
lines(row.means, type="l", lwd = 2, col="Black")
lines(log(time.series.arith+1), type="l", lwd = 2, col="4")
lines(log(time.series.geo+1), type="l", lwd = 2, col="Black")
