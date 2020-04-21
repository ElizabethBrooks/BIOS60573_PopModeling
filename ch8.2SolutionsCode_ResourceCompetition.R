#Install necessary packages
#install.packages("deSolve")
#install.packages("primer")
#Load necessary libraries
library(deSolve)
library(primer)

#Create a function for multi-species Lotka–Volterra competition
lvcompg <- function(t, n, parms) {
  r <- parms[[1]]; a <- parms[[2]]
  dns.dt <- r * n * (1 - (a%*%n))
  return( list(c(dns.dt)) ) 
}

#Create a vector of rs, and a matrix of αs
r <- c(r1=1, r2=1, r3=1)
a <- matrix(c(a11=.001, a12=.002, a13=.002,
              a21=.002, a22=.00101, a23=.002,
              a31=.002, a32=.002, a33=.00102), nrow=3, ncol=3)
parms <- list(r,a)
parms

#This will create a 3 × 24 matrix, where we have one row for each
# species, and each column is one of the initial sets of population sizes
t=seq(0,40, by=.1)
ni <- 200; std=10
N0 <- sapply(1:30, function(i) rnorm(3, mean=ni,sd=std) )

#Replace the first set of initial abundances to see what would happen
# if they start out at precisely the same initial abundances
N0[,1] <- ni

#Plot the result, we also record
# which species has the greatest initial abundance
par(mar=c(2,2,1,1))
layout(matrix(1:30, ncol=5))
for(i in 1:30) {lvout <- ode(N0[,i], t, lvcompg, parms)
matplot(t, lvout[,2:4], type="l", lwd=1.5, col=1) 
if(all(N0[,i]==200)) {
  text(3, 500, "Equal", srt=90) }else {
    text(3, 500, paste("Sp.", which.max(N0[,i])), srt=90)}
lastN <- lvout[nrow(lvout),2:4]
text(3, max(lastN), paste("Sp.", which.max(lastN)), 
     adj=c(0,1))
}
