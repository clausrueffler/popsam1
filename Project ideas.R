### Structured population with demographic stochasticity
# two life stages
# {{0, f2}, {p1, p2}}
pop.next <- function(n, f2, s1, s2){
  a12.n2 <- sum(rpois(1, n[2]*f2))
  a21.n1 <- sum(sample(c(1, 0), n[1], c(s1, 1-s1), replace = TRUE))
  a22.n2 <- sum(sample(c(1, 0), n[2], c(s2, 1-s2), replace = TRUE))
  n <- c(a12.n2, a21.n1 + a22.n2)
  return(n)
}

# as a for-loop

time.series.for <- function(n0, f2, s1, s2, tmax){
  juv <- n0[1]
  ad <- n0[2]
  n.series <- data.frame(juv, ad)
  for(i in 1:tmax){
    n.series <- rbind(n.series, pop.next(as.numeric(n.series[i, ]), f2, s1, s2))
  }
  return(n.series)
}

output <- time.series.for(n = c(2, 3), f2 = 3, s1 = 0.1, s2 = 0.7, tmax = 500)
matplot(output+1, type = "l", col =c(2, 4), lty = 1, log = "y")
legend(x = "topleft", legend = c("juv", "ad"), col =c(2, 4), lty = 1)

# as a while-loop

time.series <- function(n0, f2, s1, s2, nmax){
  juv <- n0[1]
  ad <- n0[2]
  n.series <- data.frame(juv, ad)
  i <- 1
  while(sum(as.numeric(n.series[i, ])) & sum(as.numeric(n.series[i, ])) < nmax){
    n.series <- rbind(n.series, pop.next(as.numeric(n.series[i, ]), f2, s1, s2))
    i <- i + 1
  }
  return(n.series)
}

output <- time.series(n = c(2, 3), f2 = 3, s1 = 0.1, s2 = 0.7, nmax = 500)
matplot(output+1, type = "l", col =c(2, 4), lty = 1, log = "y")
legend(x = "topleft", legend = c("juv", "ad"), col =c(2, 4), lty = 1)



       