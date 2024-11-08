######### brute force algorithm
b <- 0.55
d <- 0.5
n0 <- 10
dt <- 0.005
nmax <- 20

pop <- c(n0)
i <- 1
while(pop[i] > 0 & pop[i] < nmax){
  event <- sample(c(1, -1, 0), 1, c(pop[i]*dt*b, pop[i]*dt*d, 1- pop[i]*dt*(b + d)), replace = TRUE)
  pop[i + 1] <- pop[i] + event
  i <- i + 1
}

plot(pop, type = "l")
length(pop)


#### Gillespie algorithm
rexp(1, 20*(b + d))

t <- 0
n <- n0
b <- 0.55
d <- 0.5
n0 <- 10
nmax <- 200
output <- data.frame(t, n)

while(n > 0 & n < nmax){
  twait <- rexp(1, n*(b + d))
  event <- sample(c(1, -1), 1, c(b/(b + d), d/(b + d)), replace = TRUE)
  t <- t + twait
  n <- n + event
  output <- rbind(output, c(t, n))
}

plot(output, log = "y", type = "l")




