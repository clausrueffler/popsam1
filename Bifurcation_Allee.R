### Differential equation for model with sexual reproduction giving rise to Allee effect

Allee <- function(N, a, b, c, d){
  N*(c*N*b - c*a*N^2 - d)
}

# plot of the curve
curve(Allee(x, a=0.1, b=1, c=0.2, d=0.3), from=0, to=9, xlab = "population size, N", ylab = "dN/dt")
abline(h=0)
abline(v=0)

# This ODE is a polynomial of degree three. It has therefore potentially three equilibria
# (N-values where the rate of change equals zero).
# For the above parameter values this is indeed the case as can be seen by the graph.

# N=0 is the trivial equilibrium.
# The other two equilibrium values can be found by solving the expression in brackets in the ODE for N=0.
# 0=c*N*b - c*a*N^2 - d
# The results is given by the following two functions:

n2equil <- function(a, b, c, d){
  (b*c - sqrt(b^2 * c^2 - 4 * d * c* a))/(2 * a * c)
}

n3equil <- function(a, b, c, d){
  (b*c + sqrt(b^2 * c^2 - 4 * d * c* a))/(2 * a * c)
}

# These equilibria exist if the expression under the square-root is positive
# (otherwise the equilibria are complex numbers and not biologically relevant).

# How does the number and stability of equilibria depend on the parameters?
# Let us investigate this for the parameter b, the birth rate in the absence of competition.
# To do this, we plot all equilibria as function of N.

curve(n2equil(a=0.1, x, c=0.2, d=0.3), n = 20000, from = 0.0, to = 3, lty = 2, ylim = c(0, 30), xlab = "Intrinsic birth rate, b", ylab = "Equilibrium population size, N")
curve(n3equil(a=0.1, x, c=0.2, d=0.3), n = 20000, from = 0.0, to = 3, add=TRUE)
curve(0*x, from = 0, to= 3, add=TRUE)

# Note that the parameter n in the plotting command is NOT the population size but the "resolution" of the plot. It tells R at
# how many different values to evaluate the function in order to draw the graph.

# In this plot, the two solid lines show stable equilibria and the hatched line shows the unstable (= repelling) equilibrium
    
# Next comes a bifurcation diagram where we vary a, that is, the sensitivity to crowding.
curve(n2equil(x, b=1, c=0.2, d=0.3), n = 20000, from = 0.0, to = 0.3, lty = 2, ylim = c(0, 50), xlab = "Sensitivity to crowding", ylab = "Equilibrium population size, N")
curve(n3equil(x, b=1, c=0.2, d=0.3), n = 20000, from = 0.0, to = 0.3, add=TRUE)
curve(0*x, from = 0, to= 3, add=TRUE)
