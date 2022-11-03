Nmax <- 20
N <- 10
time.series <- c(N)

b <- 1.1
d <- 1
Dt <- 0.001

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

plot(result)


rexp(1, rate = 2)
