# 8.3 (b) Notes code
#Install necessary packages
#install.packages("deSolve")
#install.packages("primer")
#Load necessary libraries
library(deSolve)
library(primer)
#Set up input matrix
p <- c(N = 1, as = 0.01, af = 0.01, b = 0.02, qs = 0.075, qf = 0.005, 
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
#Plot the low abundance state for the floating plants
plot(N.s[1:23], S.s[1:23, 1], type = "l", lwd = 2, xlim = c(0, 4),
  ylim = c(0, 900), main = "Floating Plants",
  ylab = expression("Biomass (g m"^-2 * ")"),
  xlab = "Nitrogen Supply Rate")
lines(N.s[-(1:5)], S.s.E[-(1:5), 1], lwd = 2)
#Reinforce the concepts of multiple basins and hysteresis, by showing where the attractors are
arrows(3, 10, 3, 620, length = 0.1)
arrows(3, 820, 3, 720, length = 0.1)
arrows(0.5, 620, 0.5, 50, length = 0.1)
#Next use arrows to indicate the alternate basins of attraction at intermediate nitrogen supply rates
arrows(2.5, -10, 2.5, 60, length = 0.1)
arrows(2.5, 200, 2.5, 100, length = 0.1)
text(2.5, 100, "Coexisting\nwith S", adj = c(1.1, 0))
#Alternatively, if submerged plants were at low abundance
arrows(2, 480, 2, 580, length = 0.1)
arrows(2, 750, 2, 650, length = 0.1)
text(2, 700, "Monoculture", adj = c(1.1, 0))
#Repeat the exercise with the submerged plants
#First plot the high abundance state, and then add the low abundance state
plot(N.s[1:23], S.s[1:23, 2], type = "l", lwd = 2, xlim = c(0, 4),
  ylim = c(0, 900), main = "Submerged Plants",
  ylab = expression("Biomass (g m"^-2 * ")"),
  xlab = "Nitrogen Supply Rate")
lines(N.s[-(1:5)], S.s.E[-(1:5), 2], lwd = 2)
#Highlight the global attractors
arrows(0.7, 30, 0.7, 830, length = 0.1)
arrows(3.8, 830, 3.8, 30, length = 0.1)
#Highlight the local, alternate stable equilibria
arrows(2.3, 650, 2.3, 750, length = 0.1)
arrows(2.3, 900, 2.3, 800, length = 0.1)
text(2.4, 900, "Coexisting\nwith F", adj = c(0, 1))
# or they are excluded entirely, due to light competition
arrows(2, 130, 2, 30, length = 0.1)
text(2, 140, "Excluded\nDue to Light Comp.", adj = c(0.5,-0.3))