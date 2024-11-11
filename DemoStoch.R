d <- 0.2
b <- 0.3
N <- 10
rexp(n = 1, rate = d * N + b * N)

time_series <- function(N0, Nmax, d, b){
  N <- N0 # initial value for population size
  t <- 0 # initial value for time
  output <- data.frame(t, N)
  while (N < Nmax & N > 0) {# loop runs until N=0 or N=Nmax
    dt <- rexp(1, rate = d * N + b * N) # time to next event drawn from exponential distribution
    event <- sample(c(-1, 1), 1, prob = c(d * N, b * N)) # determines whether next event is birth or death
    t <- t + dt # updates time
    N <- N + event # updates population size
    output <- rbind(output, c(t, N)) # updates output by adding a new row
  }
  return(output) # instruct the function to return the output data
}

demographic.stoch <- time_series(3, 200, 0.2, 0.3)
plot(demographic.stoch, type="l", xlab="time", ylab="population size")

plot(demographic.stoch, log="y", type="l", xlab="time", ylab="ln(population size)")

N0 <- 3
Nmax <- 50
d <- 0.2
b <- 0.3
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
plot(results[[1]], type="l", log="y", ylim=c(1, Nmax), xlim=c(0, tmax), col=colors[1], xlab="time", ylab="ln(population size)") # plots the first simulation
for (i in 2:sim) {# this for-loop adds all further simulations to the same graph
  lines(results[[i]], type="l", col=colors[i])
}

# Let us add a line showing the deterministic dynamics, that is, in the absence of demographic stochasticity.
curve(N0*exp(x*(b - d)), from = 0, to = tmax, add=TRUE)


# Adrienne's results
A1 <- matrix(1:25, nrow = 5, ncol = 5, dimnames = list(c("1","3","5","10","50"), c("0.9", "1", "1.1", "1.2", "1.5")))
A1[,1] <- c("1","1","1","1","1")
A1[,2] <- c("1","1","1","0.9", "0.8")
A1[,3] <- c("1", "0.8", "0.9", "0.3","0")
A1[,4] <- c("0.9","0.5","0.6", "0.2", "0")
A1[,5] <- c("0.9","0.1","0.6","0","0")
A1

# Lisa's results
diff.variables <- matrix( data =c(1,1,1,1,1,1,0.9,0.9,0.9,1,1,0.8,0.7,0.3,0,1,0.6,0.2,0.1,0,0.9,0.5,0.2,0,0  ), nrow = 5, ncol = 5)
diff.variables
