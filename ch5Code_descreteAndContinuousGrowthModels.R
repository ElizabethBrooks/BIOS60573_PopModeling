##Problems##
#5.1 Basics
#Let α_11 = α_22 = 0.1, α_12 = 0.05, α_21 = 0.01
#Set competition values
aList1 <- c(0.1,0.5)
aList2 <- c(0.01,0.1)
#Project matrix
popAlphas <- matrix(c(aList1,aList2),nr = 2,byrow = TRUE)
#Determine (N_1)^∗, (N_2)^∗
#Create equations or expressions for the equilibria, (N_1)^∗ and (N_2)^∗
N1Star <- expression((a22-a12)/(a22*a11-a12*a21))
N2Star <- expression((a11-a21)/(a22*a11-a12*a21))
#Create the α and evaluate our expressions
a11 <- popAlphas[1,1]
a22 <- popAlphas[2,2]
a12 <- popAlphas[1,2]
a21 <- popAlphas[2,1]
N1 <- eval(N1Star)
N2 <- eval(N2Star)
N1
N2
K1 <- 1/a11
K2 <- 1/a22
K1
K2
#Determine K_1, K_2
#Model of discrete logistic competition
#Calculate N_t+1, given N_t, r_d and a matrix of competition coefficients α
dlvcomp2 <- function(N, alpha, rd = c(1, 1)) {
	N1.t1 <- N[1]+rd[1]*N[1]*(1-alpha[1,1]*N[1]-alpha[1,2]*N[2])
	N2.t1 <- N[2]+rd[2]*N[2]*(1-alpha[2,1]*N[1]-alpha[2,2]*N[2])
	c(N1.t1, N2.t1)
}
#Plot discrete logistic competition dynamics
#Set the number of time steps
t <- 20
#Create a matrix to hold the results using the initial population sizes
N <- matrix(NA, nrow = t + 1, ncol = 2)
N[1, ] <- c(N1, N2)
for (i in 1:t) N[i + 1, ] <- dlvcomp2(N[i, ], popAlphas)
#Plot the population trends and theoretical K in the absence of a second population
matplot(0:t, N, type = "l", col = 1, ylim = c(0, 110))
abline(h = 1/popAlphas[1, 1], lty = 3)
text(0, 1/popAlphas[1, 1], "K", adj = c(0, 0))
#Adda a legend for the population trend lines
legend("right", c(expression("Sp.1 " * (alpha[21] == popAlphas[2,1])),
	expression("Sp.2 " * (alpha[12] == popAlphas[1,2]))), lty = 1:2,
	bty = "n")
#Draw the zero net growth isoclines
#Create an expression to plot the N_1 isocline as a function of possible values of N_2
N1iso <- expression(1/popAlphas[2,2]-(popAlphas[2,1]/popAlphas[2,2])*N2)
#Now specify N_2, and then evaluate and plot N_1
N2 <- 0:200
plot(N2, eval(N1iso), type = "l", ylim = c(0, 50), xlim = c(0,
200), ylab = expression("N"[1]))
#Add arrows to indicate what happens if N_2 is above or below the value on the isocline
arrows(x0 = 90, y0 = 150, x1 = 90, y1 = 80, length = 0.1)
arrows(x0 = 75, y0 = 0, x1 = 75, y1 = 50, length = 0.1)
#Create an expression to plot the N_2 isocline as a function of possible values of N_1
N2iso <- expression(1/popAlphas[2,2]-(popAlphas[2,1]/popAlphas[2,2])*N1)
#Now specify N_1, and then evaluate and plot N_2
N1 <- 0:200
plot(eval(N2iso), N1, type = "l", ylim = c(0, 50), xlim = c(0,
200), ylab = expression("N"[2]))
#Add arrows to indicate what happens if N_2 is above or below the value on the isocline
arrows(x0 = 90, y0 = 150, x1 = 90, y1 = 80, length = 0.1)
arrows(x0 = 75, y0 = 0, x1 = 75, y1 = 50, length = 0.1)

#5.4 Total community size
#Assume for convenience that α_11 = α_22 and α_12 = α_21, and let N_T = N_1 + N_2
#Set competition values
aList1 <- c(0.1,0.1)
aList2 <- c(0.1,0.1)
#Project matrix
popAlphas <- matrix(c(aList1,aList2),nr = 2,byrow = TRUE)
#Write (N_T)^∗ as a function of α_11, α_22, α_12, α_21
#N1Star <- expression((a22-a12)/(a22*a11-a12*a21))
#N2Star <- expression((a11-a21)/(a22*a11-a12*a21))
#NStar <- expression(((a22-a12)/(a22*a11-a12*a21))+((a11-a21)/(a22*a11-a12*a21)))
#Convert to time based function
NStar <- expression((a22+a11-a12-a21)/((a11*a22)-(a12*a21)))
#Create the α and evaluate our expressions
a11 <- popAlphas[1,1]
a22 <- popAlphas[2,2]
a12 <- popAlphas[1,2]
a21 <- popAlphas[2,1]
#N1New <- eval(N1Star)
#N2New <- eval(N2Star)
N <- eval(NStar)
#N1New
#N2New
N
#Plot discrete logistic competition dynamics
#Set the number of time steps
t <- 20
#Create a matrix to hold the results using the initial population sizes
N <- matrix(NA, nrow = t + 1, ncol = 2)
N[1, ] <- c(N1, N2)
for (i in 1:t) N[i + 1, ] <- dlvcomp2(N[i, ], popAlphas)
#Plot the population trends and theoretical K in the absence of a second population
matplot(0:t, N, type = "l", col = 1, ylim = c(0, 110))
abline(h = 1/popAlphas[1, 1], lty = 3)
text(0, 1/popAlphas[1, 1], "K", adj = c(0, 0))
#Adda a legend for the population trend lines
legend("right", c(expression("Sp.1 " * (alpha[21] == popAlphas[2,1])),
	expression("Sp.2 " * (alpha[12] == popAlphas[1,2]))), lty = 1:2,
	bty = "n")

##Notes##
#Model of discrete logistic competition
#Calculate N_t+1, given N_t, r_d and a matrix of competition coefficients α
dlvcomp2 <- function(N, alpha, rd = c(1, 1)) {
	N1.t1 <- N[1]+rd[1]*N[1]*(1-alpha[1,1]*N[1]-alpha[1,2]*N[2])
	N2.t1 <- N[2]+rd[2]*N[2]*(1-alpha[2,1]*N[1]-alpha[2,2]*N[2])
	c(N1.t1, N2.t1)
}

#Plot discrete logistic competition dynamics
#First specify the matrix of α’s, the initial paopulation sizes, and the number of time steps
alphs <- matrix(c(0.01, 0.005, 0.008, 0.01), ncol = 2, byrow = TRUE)
t <- 20
#Create a matrix to hold the results using the initial population sizes
N <- matrix(NA, nrow = t + 1, ncol = 2)
N[1, ] <- c(10, 10)
for (i in 1:t) N[i + 1, ] <- dlvcomp2(N[i, ], alphs)
#Plot the population trends and theoretical K in the absence of a second population
matplot(0:t, N, type = "l", col = 1, ylim = c(0, 110))
abline(h = 1/alphs[1, 1], lty = 3)
text(0, 1/alphs[1, 1], "K", adj = c(0, 0))
#Adda a legend for the population trend lines
legend("right", c(expression("Sp.1 " * (alpha[21] == 0.008)),
	expression("Sp.2 " * (alpha[12] == 0.005))), lty = 1:2,
	bty = "n")

#Continuous logistic competition, 2-species Lotka-Volterra competition
lvcomp2 <- function(t, n, parms) {
	with(as.list(parms), {
		dn1dt <- r1*n[1]*(1-a11*n[1]-a12*n[2])
		dn2dt <- r2*n[2]*(1-a22*n[2]-a21*n[1])
		list(c(dn1dt, dn2dt))
	})
}
#Numerically integrate the dynamics, using the deSolve package
library(deSolve)
parms <- c(r1 = 1, r2 = 0.1, a11 = 0.2, a21 = 0.1, a22 = 0.02,
a12 = 0.01)
initialN <- c(2, 1)
out <- ode(y = initialN, times = 1:100, func = lvcomp2, parms = parms)
#Plot the results
matplot(out[, 1], out[, -1], type = "l")

#Graphing an Isocline (zero net growth)
#First define a new matrix of competition coefficients, where α_11 = α_22 > α_12 = α_21
a <- matrix(c(0.01, 0.005, 0.005, 0.01), ncol = 2, byrow = TRUE)
#Create an expression to plot the N_2 isocline as a function of possible values of N_1
N2iso <- expression(1/a[2,2]-(a[2,1]/a[2,2])*N1)
#Now specify N_1, and then evaluate and plot N_2
N1 <- 0:200
plot(N1, eval(N2iso), type = "l", ylim = c(0, 200), xlim = c(0,
200), ylab = expression("N"[2]))
#Add arrows to indicate what happens if N_2 is above or below the value on the isocline
arrows(x0 = 90, y0 = 150, x1 = 90, y1 = 80, length = 0.1)
arrows(x0 = 75, y0 = 0, x1 = 75, y1 = 50, length = 0.1)

#Finding equilibria
#Create equations or expressions for the equilibria, (N_1)^∗ and (N_2)^∗
N1Star <- expression((a22-a12)/(a22*a11-a12*a21))
N2Star <- expression((a11-a21)/(a22*a11-a12*a21))
#Create the α and evaluate our expressions
a11 <- a22 <- 0.01
a12 <- 0.001; a21 <- 0.001
N1 <- eval(N1Star); N2 <- eval(N2Star)
N1

#Finding partial differential equations and the Jacobian matrix
#Create equations or expressions for the for the growth rates, Ṅ_1 and Ṅ_2
#First, define expressions for the growth rates
dN1dt <- expression(r1*N1-r1*a11*N1^2-r1*a12*N1*N2)
dN2dt <- expression(r2*N2-r2*a22*N2^2-r2*a21*N1*N2)
#Next, we use each expression for Ṅ to get each the partial derivatives
ddN1dN1 <- D(dN1dt, "N1")
ddN1dN1
r1 - r1*a11*(2*N1)-r1*a12*N2
#Find the remaining PDEs
ddN1dN2 <- D(dN1dt, "N2")
ddN2dN1 <- D(dN2dt, "N1")
ddN2dN2 <- D(dN2dt, "N2")
#Lastly, put these together to create the Jacobian matrix, which is itself an 
# expression that may be evaluated again and again
J <- expression(matrix(c(eval(ddN1dN1), eval(ddN1dN2), eval(ddN2dN1),
eval(ddN2dN2)), nrow = 2, byrow = TRUE))

#Evaluating the Jacobian matrix
#Assuming that above we selected particular α, used these to determine (N_1)^∗ and (N_2)^∗,
# found the PDEs and created an expression for the Jacobian matrix, and labeled
# everything appropriately, we can then evaluate the Jacobian at an equilibrium
#For α_ii = 0.01 and α_ij = 0.001 we find
r1 <- r2 <- 1
J1 <- eval(J)
J1
#Note that all of these PDEs are negative for this equilibrium
#This indicates a stable equilibrium, because it means that each population’s growth
# rate slows in response to an increase in any other

#Eigenanalysis of the Jacobian matrix
#Now that we have evaluated the Jacobian matrix, we simply perform eigenanalysis on
#  the matrix (α_11 = α_22 = 0.01, α_12 = α_21 = 0.001, r = 1)
eigStable <- eigen(J1)
eigStable[["values"]]
#The dominant eigenvalue is negative (the larger of the two: λ_1 = -0.818)
# indicating a globally stable equilibrium
#Both eigenvalues are real, not complex, indicating that there would be no oscillations

#Eigenanalysis of the Jacobian where β_ij , β_ji > 1
# (the inter- and intraspecific effects of each species)
#Here we create values for α that create an unstable equilbrium
a11 <- a22 <- 0.01
a12 <- a21 <- 0.011
N1 <- eval(N1Star); N2 <- eval(N2Star)
eigen(eval(J))$values
#The dominant eigenvalue is now positive, while the other is negative,
# indicating a saddle

#Eigenanalysis of the Jacobian where β_ij = β_ji = 1
#Here we create values for α that create a neutral equilbrium
a11 <- a21 <- 0.01
a22 <- a12 <- 0.015
#Here (N)^∗ is determined differently because the usual expression fails
# when the denominator equals 0
N1 <- N2 <- 1/(a11 + a22)
eigen(eval(J))[["values"]]
#The dominant eigenvalue is now zero, indicating a neutral equilibrium
#The neutral nature of this equilibrium results in more than one equilibrium
#Here is a different one, also on the isocline
N1 <- 1/(a11)
N2 <- 0
eigen(eval(J))[["values"]]
#Again λ_1 = 0 (the perturbation growth rate) so this equilibrium is also neutral

