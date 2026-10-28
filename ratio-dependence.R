library(deSolve)

predprey.ratio <- function(t, y, p) {
  R <- y[1]
  C <- y[2]
  with(as.list(p), {
    dR.dt <- r * R * (1 - R / k) - a * C * (R/C)/(1 + a * h * (R/C))
    dC.dt <- e * a * (R/C) * C/(1 + a * h * (R/C)) - d * C
    list(c(dR.dt, dC.dt))
  })
}

r <- 2
e <- 0.5
a <- 0.5
d <- 0.35
k <- 10
h <- 0.35

R0 <- 2
C0 <- 2

params.RosMac1 <- c(a = a, r = r, d = d, e = e, k = k, h = h)
MaxTime <- 150 # you can increase MaxTime if you think the dynamics have not yet reached their "endpoint"
Time <- seq(0, MaxTime, by = 0.5)
RosMac.out <- ode(c(R0, C0), Time, predprey.ratio, params.RosMac1)

matplot(Time, (RosMac.out[, 2:3]), type = "l", lty = 1:2, col=1, ylab = "Population Size", xlab="Time")
legend("top", c(expression("resource"), expression("consumer")), lty = 1:2, bty = "n")

plot(RosMac.out[, 2], RosMac.out[, 3], type = "l",  lty = 1, col = 1, ylab = "consumer", xlab = "resource")
abline(v = d / (e * (a - d*h)), lty = 2, col = "blue") # consumer isocline, a vertical line
# to produce the graph for the resource isocline requires a bit more work since the now becomes a "curve" instead of a straight line
R <- 0:k
Riso <- expression(r / (a * k) * (k -R) * (1 + a * h * R))
RisoStable <- eval(Riso)
lines(R, RisoStable, type = "l", lty =2, ylab = "consumer", xlab = "resource", col = "red", ylim = c(0, 10)) # resource isocline