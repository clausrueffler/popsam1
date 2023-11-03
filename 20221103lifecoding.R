# Method 1: more elegant?
stochastic.time.series <- function(N, Nmax, b, d, Dt){
  time.series <- c(N)
  while (N > 0 & N < Nmax) {
    event <- sample(c(1, -1, 0), 1, c(b*N*Dt, d*N*Dt, 1-N*Dt*(b + d)), replace = TRUE)
    N <- N + event
    time.series <- append(time.series, N)
  }
  return(time.series)
}
result <- stochastic.time.series(10, 20, 1.0, 1.1, 0.001)
plot(result, type = "l", xlab = "time", ylab = "population size")

# Method 2: more intuitive?
stochastic.time.series <- function(N0, Nmax, b, d, Dt){
  n <- c(N0)
  t <- 1
  while (n[t] > 0 & n[t] < Nmax) {
    event <- sample(c(1, -1, 0), 1, c(b*n[t]*Dt, d*n[t]*Dt, 1-n[t]*Dt*(b + d)), replace = TRUE)
    n[t+1] <- n[t] + event
    t <- t + 1
  }
  return(n)
}
result <- stochastic.time.series(10, 20, 1.0, 1.1, 0.01)
plot(result, type = "l", xlab = "time", ylab = "population size")


# drawing waiting times from exponential distribution

rexp(5, rate = 2)






