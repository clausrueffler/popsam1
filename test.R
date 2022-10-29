environmental_stochasticity <- function(N0, n, min, max) {# function with four arguments
  growth_factors <- runif(n, min, max)
  time_series <- c()
  time_series[1] <- N0
  for (t in 1:n) {
    time_series[t+1] <- if (time_series[t]*growth_factors[t] < 1) 0 else time_series[t]*growth_factors[t]
  }
  return(time_series)
}

n <- 500 # lenght of time series
min <- 0.9 # lowest possible value for growth factor
max <- 1.1 # largest possible value of growth factor
N0 <- 10 # initial population size
sim <- 20 # number of simulation runs

results <- matrix(nrow = n + 1, ncol = sim) # creates an empty matrix with n+1 rows and sim columns in which we will store the time series

j <- 1 # intialize a counter
for (i in 1:sim) {# executes the function sim times
  results[, j] <- environmental_stochasticity(N0, n, min, max) # produces a time series and stores it in the jth column of the matrix results
  j = j + 1 # updates the counter by increasing it by 1
}

cl <- rainbow(sim) # set number of colors equal to number of simulations

maxN <- numeric(sim) # here we determines how long the y-axis has to be
for (i in 1:sim) {# we do this by determining the largest population size that is reached in each of the simulations
  maxN[i] <- max(results[, i])
}

plot(0:n, results[, 1]+1, type="l", log="y", ylim=c(1, max(maxN)), col=cl[1], xlab="time", ylab="population size") # plot the result from the first simulation
for (i in 2:sim) {# add the results from all other simulations to the same graph
  lines(0:n, results[, i]+1, type="l", col=cl[i])
}