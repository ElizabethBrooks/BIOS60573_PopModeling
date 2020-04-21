#9.5 solution code
#Install necessary packages
#install.packages("deSolve")
#install.packages("primer")
#Load necessary libraries
library(deSolve)
library(primer)
#################################################################################
#First test
#First we create a variable environment with white noise
years <- 100
t <- 1:years
variability = 4
env <- rnorm(years, m=0, sd=variability)
plot(t, env, type='l')

#Set species fitness values
w.rare <- 1
w.comm <- 1

#The niches overlap, and we will call the overlap rho, ρ
#Let overlap, ρ, be equal to the standard deviation of 
# our environmental variabiity
rho <- sd(env)

#Create a histogram of environmental va riability, niche overlap (ρ),
# and the resulting buffered population growth rates
hist.env <- hist(env,col='lightgray',main="Histogram of Environment")
abline(v=c(c(-rho, rho)/2), lty=3)
arrows(x0=-rho/2, y0=mean(hist.env[["counts"]]), 
       x1=rho/2, y1=mean(hist.env[["counts"]]), code=3, length=.1)
text(0, mean(hist.env[["counts"]]), quote(italic(rho)), adj=c(1.5,0), cex=1.5)
text(min(hist.env[["breaks"]]),mean(hist.env[["counts"]]),
     "Common sp.\ngrows best", adj=c(0,0))
text(max(hist.env[["breaks"]]),mean(hist.env[["counts"]]),
     "Rare sp.\ngrows best", adj=c(1,0))

#Quantitfy reproduction as a function of the environmenta.rare <-  (env+rho/2)*w.rare
a.comm <- -(env-rho/2)*w.comm

#Quantitfy reproduction as a function of the environment
a.rare <- (env + rho/2) * w.rare
a.comm <- -(env - rho/2) * w.comm

#To model this buffering effect, we will simply prevent the
# reproductive rates from falling below zero
Es <- matrix(NA, nrow=years, ncol=2)
Es[,1] <- ifelse(a.rare > 0, a.rare, 0)
Es[,2] <- ifelse(a.comm > 0, a.comm, 0)
matplot(t, Es, type='l', col=1)
matplot(env,Es, col=1)

#Let us create a variable for community-wide mortality, δ,
# as if a disturbance kills a constant fraction of the community
d <- 0.1

#Let the rare species experience greater per capita effects of competition
alpha <- c(2*1e-5,1e-5)

#Finally, we simulate these dynamics
#Initiallize matricies
Ns <- matrix(NA, nrow=years+1, ncol=2)
Cs <- matrix(NA, nrow=years, ncol=2)
Rs <- matrix(NA, nrow=years, ncol=2)

#Next we initialize our populations at t 0 .
Ns[1,] <- c(1e3,1e5)

#Finally, we run the simulation
for(i in 1:years) Ns[i+1,] <- { 
  juveniles <- sum( exp(Es[i,]) * Ns[i,] )
  Cs[i,]  <-  alpha*juveniles
  #Cs[i,] <- log(juveniles/sum(d*Ns[i,]))
  Rs[i,] <- exp(Es[i,]-Cs[i,])
  (1-d) * Ns[i,] + Rs[i,]*Ns[i,]
}

#Plot the simulation results
matplot(c(0,t), Ns, type='b', log='y')
#################################################################################
#Second Test
#First we create a variable environment with white noise
years <- 100
t <- 1:years
variability = 1
env <- rnorm(years, m=0, sd=variability)
plot(t, env, type='l')

#Set species fitness values
w.rare <- 1
w.comm <- 1

#The niches overlap, and we will call the overlap rho, ρ
#Let overlap, ρ, be equal to the standard deviation of 
# our environmental variabiity
rho <- sd(env)

#Create a histogram of environmental va riability, niche overlap (ρ),
# and the resulting buffered population growth rates
hist.env <- hist(env,col='lightgray',main="Histogram of Environment")
abline(v=c(c(-rho, rho)/2), lty=3)
arrows(x0=-rho/2, y0=mean(hist.env[["counts"]]), 
       x1=rho/2, y1=mean(hist.env[["counts"]]), code=3, length=.1)
text(0, mean(hist.env[["counts"]]), quote(italic(rho)), adj=c(1.5,0), cex=1.5)
text(min(hist.env[["breaks"]]),mean(hist.env[["counts"]]),
     "Common sp.\ngrows best", adj=c(0,0))
text(max(hist.env[["breaks"]]),mean(hist.env[["counts"]]),
     "Rare sp.\ngrows best", adj=c(1,0))

#Quantitfy reproduction as a function of the environmenta.rare <-  (env+rho/2)*w.rare
a.comm <- -(env-rho/2)*w.comm

#Quantitfy reproduction as a function of the environment
a.rare <- (env + rho/2) * w.rare
a.comm <- -(env - rho/2) * w.comm

#To model this buffering effect, we will simply prevent the
# reproductive rates from falling below zero
Es <- matrix(NA, nrow=years, ncol=2)
Es[,1] <- ifelse(a.rare > 0, a.rare, 0)
Es[,2] <- ifelse(a.comm > 0, a.comm, 0)
matplot(t, Es, type='l', col=1)
matplot(env,Es, col=1)

#Let us create a variable for community-wide mortality, δ,
# as if a disturbance kills a constant fraction of the community
d <- 0.1

#Let the rare species experience greater per capita effects of competition
alpha <- c(2*1e-5,1e-5)

#Finally, we simulate these dynamics
#Initiallize matricies
Ns <- matrix(NA, nrow=years+1, ncol=2)
Cs <- matrix(NA, nrow=years, ncol=2)
Rs <- matrix(NA, nrow=years, ncol=2)

#Next we initialize our populations at t 0 .
Ns[1,] <- c(1e3,1e5)

#Finally, we run the simulation
for(i in 1:years) Ns[i+1,] <- { 
  juveniles <- sum( exp(Es[i,]) * Ns[i,] )
  Cs[i,]  <-  alpha*juveniles
  #Cs[i,] <- log(juveniles/sum(d*Ns[i,]))
  Rs[i,] <- exp(Es[i,]-Cs[i,])
  (1-d) * Ns[i,] + Rs[i,]*Ns[i,]
}

#Plot the simulation results
matplot(c(0,t), Ns, type='b', log='y')
#################################################################################
#Third Test
#First we create a variable environment with white noise
years <- 100
t <- 1:years
variability = 1
env <- rnorm(years, m=0, sd=variability)
plot(t, env, type='l')

#Set species fitness values
w.rare <- 0.5
w.comm <- 1

#The niches overlap, and we will call the overlap rho, ρ
#Let overlap, ρ, be equal to the standard deviation of 
# our environmental variabiity
rho <- sd(env)

#Create a histogram of environmental va riability, niche overlap (ρ),
# and the resulting buffered population growth rates
hist.env <- hist(env,col='lightgray',main="Histogram of Environment")
abline(v=c(c(-rho, rho)/2), lty=3)
arrows(x0=-rho/2, y0=mean(hist.env[["counts"]]), 
       x1=rho/2, y1=mean(hist.env[["counts"]]), code=3, length=.1)
text(0, mean(hist.env[["counts"]]), quote(italic(rho)), adj=c(1.5,0), cex=1.5)
text(min(hist.env[["breaks"]]),mean(hist.env[["counts"]]),
     "Common sp.\ngrows best", adj=c(0,0))
text(max(hist.env[["breaks"]]),mean(hist.env[["counts"]]),
     "Rare sp.\ngrows best", adj=c(1,0))

#Quantitfy reproduction as a function of the environmenta.rare <-  (env+rho/2)*w.rare
a.comm <- -(env-rho/2)*w.comm

#Quantitfy reproduction as a function of the environment
a.rare <- (env + rho/2) * w.rare
a.comm <- -(env - rho/2) * w.comm

#To model this buffering effect, we will simply prevent the
# reproductive rates from falling below zero
Es <- matrix(NA, nrow=years, ncol=2)
Es[,1] <- ifelse(a.rare > 0, a.rare, 0)
Es[,2] <- ifelse(a.comm > 0, a.comm, 0)
matplot(t, Es, type='l', col=1)
matplot(env,Es, col=1)

#Let us create a variable for community-wide mortality, δ,
# as if a disturbance kills a constant fraction of the community
d <- 0.1

#Let the rare species experience greater per capita effects of competition
alpha <- c(2*1e-5,1e-5)

#Finally, we simulate these dynamics
#Initiallize matricies
Ns <- matrix(NA, nrow=years+1, ncol=2)
Cs <- matrix(NA, nrow=years, ncol=2)
Rs <- matrix(NA, nrow=years, ncol=2)

#Next we initialize our populations at t 0 .
Ns[1,] <- c(1e3,1e5)

#Finally, we run the simulation
for(i in 1:years) Ns[i+1,] <- { 
  juveniles <- sum( exp(Es[i,]) * Ns[i,] )
  Cs[i,]  <-  alpha*juveniles
  #Cs[i,] <- log(juveniles/sum(d*Ns[i,]))
  Rs[i,] <- exp(Es[i,]-Cs[i,])
  (1-d) * Ns[i,] + Rs[i,]*Ns[i,]
}

#Plot the simulation results
matplot(c(0,t), Ns, type='b', log='y')
#################################################################################
#Fourth Test
#First we create a variable environment with white noise
years <- 100
t <- 1:years
variability = 4
env <- rnorm(years, m=0, sd=variability)
plot(t, env, type='l')

#Set species fitness values
w.rare <- 0.5
w.comm <- 1

#The niches overlap, and we will call the overlap rho, ρ
#Let overlap, ρ, be equal to the standard deviation of 
# our environmental variabiity
rho <- sd(env)

#Create a histogram of environmental va riability, niche overlap (ρ),
# and the resulting buffered population growth rates
hist.env <- hist(env,col='lightgray',main="Histogram of Environment")
abline(v=c(c(-rho, rho)/2), lty=3)
arrows(x0=-rho/2, y0=mean(hist.env[["counts"]]), 
       x1=rho/2, y1=mean(hist.env[["counts"]]), code=3, length=.1)
text(0, mean(hist.env[["counts"]]), quote(italic(rho)), adj=c(1.5,0), cex=1.5)
text(min(hist.env[["breaks"]]),mean(hist.env[["counts"]]),
     "Common sp.\ngrows best", adj=c(0,0))
text(max(hist.env[["breaks"]]),mean(hist.env[["counts"]]),
     "Rare sp.\ngrows best", adj=c(1,0))

#Quantitfy reproduction as a function of the environmenta.rare <-  (env+rho/2)*w.rare
a.comm <- -(env-rho/2)*w.comm

#Quantitfy reproduction as a function of the environment
a.rare <- (env + rho/2) * w.rare
a.comm <- -(env - rho/2) * w.comm

#To model this buffering effect, we will simply prevent the
# reproductive rates from falling below zero
Es <- matrix(NA, nrow=years, ncol=2)
Es[,1] <- ifelse(a.rare > 0, a.rare, 0)
Es[,2] <- ifelse(a.comm > 0, a.comm, 0)
matplot(t, Es, type='l', col=1)
matplot(env,Es, col=1)

#Let us create a variable for community-wide mortality, δ,
# as if a disturbance kills a constant fraction of the community
d <- 0.1

#Let the rare species experience greater per capita effects of competition
alpha <- c(2*1e-5,1e-5)

#Finally, we simulate these dynamics
#Initiallize matricies
Ns <- matrix(NA, nrow=years+1, ncol=2)
Cs <- matrix(NA, nrow=years, ncol=2)
Rs <- matrix(NA, nrow=years, ncol=2)

#Next we initialize our populations at t 0 .
Ns[1,] <- c(1e3,1e5)

#Finally, we run the simulation
for(i in 1:years) Ns[i+1,] <- { 
  juveniles <- sum( exp(Es[i,]) * Ns[i,] )
  Cs[i,]  <-  alpha*juveniles
  #Cs[i,] <- log(juveniles/sum(d*Ns[i,]))
  Rs[i,] <- exp(Es[i,]-Cs[i,])
  (1-d) * Ns[i,] + Rs[i,]*Ns[i,]
}

#Plot the simulation results
matplot(c(0,t), Ns, type='b', log='y')