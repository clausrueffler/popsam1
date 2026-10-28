
time_series <- function(N0, Nmax, d, b){
  N <- N0
  t <- 0
  output <- data.frame(t, N)
  while (N < Nmax & N > 0) {# 1
    dt <- rexp(1, rate = d * N + b * N) # 2
    event <- sample(c(-1, 1), 1, prob = c(d, b)) # 3
    t <- t + dt
    N <- N + event
    output <- rbind(output, c(t, N)) # 4
  }
  return(output)
}

N0 <- 3
Nmax <- 20
d <- 0.5
b <- 0.6
sim <- 10 # this sets the number of simulations

results <- list() # using the list variable allows us to save results of all iterations of the function time_series, that is, we can save several data frames in it
for (i in 1:sim) {# to create sim different simulations, we embed the function time.series into a for-loop
  results[[i]] <- time_series(N0, Nmax, d, b) # adds the result of a simulation to the results, note that we have to use [[]] to access list entries
}

maxt <- c() # creates a vector in which we store the length of the different time series
for (i in 1:sim) {
  maxt[i] <- max(results[[i]][1]) # determines the length of the ith time series and stores the result in the object maxt
}
tmax <- max(maxt) # the length of the longest time series is assigned to the parameter tmax

colors <- rainbow(length(results)) # sets number of colors equal to number sim
plot(results[[1]], type="s", ylim=c(0, Nmax), xlim=c(0, tmax), col=colors[1], xlab="time", ylab="population size") # plots the first simulation
for (i in 2:sim) {# this for-loop adds all further simulations to the same graph
  lines(results[[i]], type="s", col=colors[i])
}

# Let us add a line showing the deterministic dynamics, that is, in the absence of demographic stochasticity.
curve(N0*exp(x*(b - d)), from = 0, to = tmax, add=TRUE)