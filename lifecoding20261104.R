mean(rbinom(1090, 2, 0.85))/2

rpois(10, 2)

runif(10, 1, 2)

rnorm(10, 2, 5)

rep(1/6, 6)

b <- 0.6
d <- 0.5
events <-c(-1, 1)
probabilities <- c(d/(d + b), b/(d + b))

hist(rexp(1000, b), 20)

sample(events, 10, replace = TRUE, probabilities)

sum(sample(1:6, 2, replace = TRUE, rep(1/6, 6)))




probabilities <- c(d/(d + b), b/(d + b))

Nmax <- 100
b <- 0.6
d <- 0.5
N0 <- 3

dem.stoch <- function(b, d, N0, Nmax){
  probabilities <- c(d/(d + b), b/(d + b))
  t <- 0
  N <- N0
  output <- data.frame(t, N)

  while (N > 0 & N < Nmax) {
    dt <- rexp(1, N*(b + d))
    t <- t + dt
    event <- sample(c(-1, 1), 1, replace = TRUE, probabilities)
    N <- N + event
    output <- rbind(output, c(t, N))
  }
  output
}

data <- dem.stoch(b = 0.6, d = 0.5, N0 = 3, Nmax = 100)

plot(data, type = "s", log = "y")

sim <- 10
data <- list()

for (i in 1:sim) {
  data[[i]] <- dem.stoch(b = 0.6, d = 0.5, N0 = 3, Nmax = 100)
}

maxtime <- c()
for (i in 1:sim) {
  maxtime[i] <- max(data[[i]][1])
  }
maxmaxtime <- max(maxtime)


plot(data[[1]], log = "y", type = "s", xlim = c(0, maxmaxtime), ylim = c(1, 100))
for (i in 2:10) {
  lines(data[[i]], type = "s") 
}
