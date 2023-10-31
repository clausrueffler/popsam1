N0 <- 10
tmax <- 500
lambda_mean <- 1.01
deviation <- 0.15
possible_outcomes <- c(lambda_mean - deviation, lambda_mean + deviation)
probabilities <- c(0.5, 0.5)

environmental_stochasticity <- function(N0, tmax, possible_outcomes, probabilities) {# function with four arguments
  growth_factors <- sample(possible_outcomes, tmax, probabilities, replace = TRUE)
  time_series <- c()
  time_series[1] <- N0
  for (t in 1:tmax) {
    time_series[t+1] <- time_series[t]*growth_factors[t]
  }
  return(time_series)
}

sim <- 10 # number of simulation runs

results <- matrix(nrow = tmax + 1, ncol = sim) # creates an empty matrix with n+1 rows and sim columns in which we will store the time series

for (i in 1:sim) {# executes the function sim times
  results[, i] <- environmental_stochasticity(N0, tmax, possible_outcomes, probabilities) # produces a time series and stores it in the jth column of the matrix results
}

# row means of results gives the average density for each time point
row_means <- rowMeans(results)
# expected growth rate (based on Eq.(1.5b) in Lande et al. (2003))
exp_mean_r <- exp(log(lambda_mean) + 1/2*log(1 - deviation^2 / lambda_mean^2))
# time series based on the mean value of lambda
time_series_geom <- c()
time_series_geom[1] <- N0
for (t in 1:tmax) {
  time_series_geom[t+1] <- time_series_geom[t]*lambda_mean
}
# time series based on the mean value of lambda
time_series_expected <- c()
time_series_expected[1] <- N0
for (t in 1:tmax) {
  time_series_expected[t+1] <- time_series_expected[t]*exp_mean_r
}

cl <- rainbow(sim) # set number of colors equal to number of simulations

plot(0:tmax, results[, 1]+1, type="l", log="y", ylim=c(1, max(results)), col=cl[1], xlab="time", ylab="population size") # plot the result from the first simulation
for (i in 2:sim) {# add the results from all other simulations to the same graph
  lines(0:tmax, results[, i]+1, type="l", col=cl[i])
}
lines(0:tmax, time_series_geom, type="l", col="4")
lines(0:tmax, time_series_expected, type="l", col="Gray")
#lines(0:tmax, row_means, type="l", col="Black")

plot(0:tmax, results[, 1], type="l",  ylim=c(0, max(results)), col=cl[1], xlab="time", ylab="population size") # plot the result from the first simulation
for (i in 2:sim) {# add the results from all other simulations to the same graph
  lines(0:tmax, results[, i], type="l", col=cl[i])
}
lines(0:tmax, time_series_geom, type="l", col="4")
lines(0:tmax, time_series_expected, type="l", col="Gray")
#lines(0:tmax, row_means, type="l", col="Black")


mean_r <- exp(log(lambda_mean) + 1/2*log(1 - deviation^2 / lambda_mean^2))

