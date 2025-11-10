## plotting the solution of logistic growth

log.growth <- function(r, a, n0, t){
  (r/a)/(1 - (1 - (r/(a*n0)))*exp(-r*t))
}

log.growth(0.1, 0.001, 150, 10)

curve(log.growth(0.1, 0.001, 10, x), from = 0, to = 200)
curve(log.growth(0.1, 0.001, 150, x), from = 0, to = 100, ylim = c(0, 150))

# model with Allee effect
Allee.dynamics <- function(c, b, a, d, n){
  c*n*n*(b - a*n) - d*n
}

eq.2 <- function(c, b, a, d){
  (b*c - sqrt(b^2 * c^2 - 4*a*c*d))/(2*a*c)
}
eq.3 <- function(c, b, a, d){
  (b*c + sqrt(b^2 * c^2 - 4*a*c*d))/(2*a*c)
}

b <- 0.3
eq.2(0.1, b, 0.001, 1)
eq.3(0.1, b, 0.001, 1)

curve(Allee.dynamics(0.1, b, 0.001, 1, x), from = 0, to = 300)
abline(h = 0)

curve(eq.3(0.1, x, 0.001, 1), from = 0.2, to = 1, xlim = c(0, 1), ylim = c(0, 1000))
curve(eq.2(0.1, x, 0.001, 1), from = 0.2, to = 1, add = TRUE)
abline(h = 0)

###########
library(deSolve)
n0 <- c(n = 1)
parameters <- c(c= 0.1, b = 0.5, a = 0.001, d = 1)
time.steps <- seq(0, 20, 0.5)

Allee.growth <- function(time.steps, n0, parameters) {
  with(as.list(c(n0, parameters)), {
     dn.dt <- c*n*n*(b - a*n) - d*n
     return(list(dn.dt))
     })
}

time.series <- ode(y = n0, times = time.steps, func = Allee.growth, parms = parameters)
plot(time.steps, time.series[, 2], type = "l", lty = 1:2, col=1, ylab = "Population Density N", xlab="Time")
