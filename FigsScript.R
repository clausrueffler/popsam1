lambda.mean <- 1.01
deviation <- 0.2
lambda1 <- (lambda.mean + deviation)
lambda2 <- (lambda.mean - deviation)
lambda.geo <- sqrt(lambda1*lambda2)
possible.outcomes <- c(lambda1, lambda2)
probabilities <- c(0.5, 0.5)
N0 <- 10 # initial population size
tmax <- 200 # length of the time series

environmental.stochasticity <- function(N0, tmax, possible.outcomes, probabilities) {
  time.series <- c()
  time.series[1] <- N0
  for (t in 1:tmax) {
    lambda <- sample(possible.outcomes, 1, replace = TRUE, probabilities)
    time.series[t+1] <- time.series[t] * lambda
  }
  return(time.series)
}

sim <- 40

results <- matrix(nrow = tmax + 1, ncol = sim)

for (i in 1:sim) {
  results[, i] <- environmental.stochasticity(N0, tmax, possible.outcomes, probabilities)
}

time.series.geom <- c(N0)
for (t in 1:tmax) {
  time.series.geom[t+1] <- time.series.geom[t]*lambda.geo
}

cl <- rainbow(sim)
log.range <- range(
  log(results),
  log(time.series.geom),
  finite = TRUE
)

plot(log(results[, 1]), type="l", ylim=log.range, col=cl[1], xlab="time", ylab="ln(population size)")
for (i in 2:sim) {
  lines(log(results[, i]), type="l", col=cl[i])
}

lines(log(time.series.geom), type="l", col="4")

log.row.means <- rowMeans(log(results))
lines(log.row.means, type="l", col="Black")




log.range <- range(
  log(results),
  log(time.series.geom),
  finite = TRUE
)
log.range  





time.series.expected <- c()
time.series.expected[1] <- N0
for (t in 1:tmax) {
  time.series.expected[t+1] <- time.series.expected[t]*lambda.mean
}

lines(log(time.series.expected + 1), type="l", col="Black")

# plot on normal scale
plot(results[, 1], type="l", ylim=c(0, max(time.series.geom, results)), col=cl[1], xlab="time", ylab="population size")
for (i in 2:sim) {
  lines(results[, i], type="l", col=cl[i])
}

lines(time.series.geom, type="l", col="4")

log.row.means <- rowMeans(results)
lines(log.row.means, type="l", col="Black")

exp.mean.lambda <- sqrt(lambda1*lambda2) # exponential of the formula in the above equation

time.series.expected <- c()
time.series.expected[1] <- N0
for (t in 1:tmax) {
  time.series.expected[t+1] <- time.series.expected[t]*exp.mean.lambda
}

lines(time.series.expected, type="l", col="Black")