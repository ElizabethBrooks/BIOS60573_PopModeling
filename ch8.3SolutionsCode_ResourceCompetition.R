# 8.3 (b) solution code
#Install necessary packages
#install.packages("deSolve")
#install.packages("primer")
#Load necessary libraries
library(deSolve)
library(primer)
#Set up input matrix
p <- c(N = 1, as = 0.0, af = 0.01, b = 0.0, qs = 0.075, qf = 0.005, 
  hs = 0, hf = 0.2, ls = 0.05, lf = 0.05, rs = 0.5, rf = 0.5, W = 0)
t <- 1:200
Initial <- c(F = 10, S = 10)
#Run the solver and plot the results
S.out1 <- ode(Initial, t, scheffer, p)
matplot(t, S.out1[, -1], type = "l")
legend("right", c("F", "S"), lty = 1:2, bty = "n")
#Increase nitrogen and plot the results
p["N"] <- 4
S.out2 <- ode(Initial, t, scheffer, p)
matplot(t, S.out2[, -1], type = "l")
#Vary N, increasing it slowly
N.s <- seq(0.5, 4, by = 0.1)
t <- 1:1000
S.s <- t(sapply(N.s, function(x) {
  p["N"] <- x
  ode(Initial, t, scheffer, p)[length(t), 2:3]
}))
#Plot the asymptotic abundances vs. the nitrogen levels
matplot(N.s, S.s, type = "l")
legend("right", c("F", "S"), lty = 1:2, bty = "n")
#Start at high floating plant abundances, low submerged plant abundances,
# and then plot to see what happens at different nitrogen levels
Initial.Eutrophic <- c(F = 600, S = 10)
S.s.E <- t(sapply(N.s, function(x) {
  p["N"] <- x
  ode(Initial.Eutrophic, c(1, 1000), scheffer, p)[2, 2:3]
}))
#Plot the asymptotic abundances vs the nitrogen levels
matplot(N.s, S.s.E, type = "l")

