#Author: Elizabeth Brooks
#Date: 21 January 2020
#CH 1 Problem Solutions

#Problem 1.1
# First we select the number of observed R (t = 9);
# this will require that we use ten years of lily data.
t <- 9
LL9 <- c(150,100,125,200,225,150,100,175,100,150)
#Problem 1.1, Parts b and c
# Next we calculate λ for each generation, from t to t + 1,
# and calculate the arithmetic and geometric means.
LLgr <- LL9[2:(t + 1)]/LL9[1:t]
lam.A <- sum(LLgr)/t
lam.G <- prod(LLgr)^(1/t)
plot(0:8, LLgr, ylab = "Lily Population Rates")
#Problem 1.1, Part a
#Graph of population size versus time
N0 <- LL9[1]
plot(0:t, LL9, ylab = "Lily Projected Population Size")
lines(0:t, N0 * lam.A^(0:t), lty = 2)
lines(0:t, N0 * lam.G^(0:t), lty = 1)
legend(0,220,c("Arithmetic Avg", "Geometric Avg"),
       title = "Projections Based On:",lty = 2:1,bty = "n",xjust = 0)
#Problem 1.1, Part d*
#Function to simulate population dynamics with random R, initial pop size,
# years to simulate, and number of simulations to run
PopSim <- function(Rs,Ni,time = 15,sims = 10) {
  sim.RM = matrix(sample(Rs,size = sims * years,replace = TRUE),
                  nrow = years, ncol = sims)
  output <- numeric(years + 1)
  output[1] <- Ni
  outmat <- sapply(1:sims, function(i) {
    for (t in 1:years) output[t + 1] <- round(output[t] * sim.RM[t, i], 0)
    output
  })
  return(outmat)
}
#Plotted use case for Problem 1.1, Part d*
#Note we use log scale to help us see the small populations
years <- 15
runs <- 10
#All but first, and last elements
Rr <- LL9[-1]/LL9[-length(LL9)]
Mr <- PopSim(Rr,N0,years,runs)
#Graph of projected population sizes to 2010
matplot(0:years,Mr,type = "l",log = "y",ylab = "Lily Descrete Growth Dynamics")
#Plot a histogram of the last simulated populations
hist(Mr[-length(Mr)])
summary(Mr[-length(Mr)])

#Problem 1.3, Part a
H0 <- 630000000
He <- 63000000000
years = 2003-1700
growth <- He/H0
rate <- log(growth)/years
#Problem 1.3, Part b  
#Graph of the model of human population size population
# size from 1700 to 2020
years = 2020-1700
runs = 1
n <- 0:years
#Calculate (1 + rd/n)^n for ever larger values of n
# in order to approximate the limit of the given equation
# for continuous exponential growth
H1 <- H0 * (1+rate/n)^n
plot(0:years, H1, ylab = "Human Estimated Population Size")
#Project the population sizes from 1700 to 2020 using
# the Nt = N0*e^(r*t) cotinuous exponential growth function
Ht <- H0 * exp(rate*n)
plot(0:years, Ht, ylab = "Human Projected Population Size")
#Plot the ratio and add some fancy math text to the plot
plot(n,H1/H0,type = "l",ylab = "Human Growth Ratio Dynamics")
text(50, 0.98, "For n = 100,")
text(50, 0.96, bquote((1 + frac("r"["d"], "n"))^"n" == .(round(H1[101]/H0, 3))))
#Simulate descrete growth dynamics and plot for comparison
Hr <- PopSim(rate,H0,years,runs)
matplot(0:years,Hr,type = "l",ylab = "Human Descrete Growth Dynamics")
#Problem 1.3, Part c
#Function for determining the time it takes for
# a population to reach m times its initial size,
# with a default of 2 for doublign times
m.times <- function(m=2,r) {
  log(m)/log(r)
}
#Function for determining the rates for the times
m.rates <- function(m,Ni,Ne) {
  r <- Ne/Ni
  log(m)/log(r)
}
#Use m.time to generate doubling times and add graph
# points indicating the population doublings from 1700 onward
m.times(2,growth)
m.rates(2,H0,He)
