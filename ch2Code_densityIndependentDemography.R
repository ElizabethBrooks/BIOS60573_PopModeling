#Problem 2.1
#Part b: start with N = (0 10 10 10 10 10) and graph population dynamics
# for all stages for 10 years
#First, create a population projection matrix, and a vector of 
# stage class abundances for year zero
s1 <- c(0,0,0,0,0,1.642)
s2 <- c(0.098,0,0,0,0,0.437)
s3 <- c(0,0.342,0.591,0.050,0.095,0)
s4 <- c(0,0.026,0.295,0.774,0.177,0.194)
s5 <- c(0,0,0,0.145,0.596,0.362)
s6 <- c(0,0,0,0.016,0.277,0.489)
#Project matrix
A <- matrix(c(s1,s2,s3,s4,s5,s6),nr = 6,byrow = TRUE)
#Observed stage class abundances
N0 <- matrix(c(0,10,10,10,10,10),ncol = 1)
#Now, project the population over 10 years, using a for-loop
years <- 10
N.projections <- matrix(0,nrow = nrow(A),ncol = years+1)
N.projections[,1] <- N0
#Next, perform the iteration of matrix multiplication between
# the projection matrix and N0
for (i in 1:years) N.projections[,i+1] <- A %*% N.projections[,i]
#Finally, graph the results for each stage
matplot(0:years, t(N.projections), type = "l",lty = 1:6,col = 1,
        ylab = "Stage Abundance",xlab = "Year")
legend("topleft", legend = c("Domant","Seedling","Small","Medium",
                             "Large","Fertile"),lty = 1:6,col = 1,bty = "n")
#Part c: determine the stable stage distribution (SSD)
#Perform eigenanalysis on A to retrieve vectors and values,
# each of which provide a solution to Aw = λw 
eigs.A <- eigen(A)
eigs.A
#Next we explicitly find the index position of the largest 
# absolute value of the eigen-values
dom.pos <- which.max(eigs.A[["values"]])
#We extract w, keeping just the real part, and divide it by
# its sum to get the SSD
w <- Re(eigs.A[["vectors"]][,dom.pos])
ssd <- w/sum(w)
round(ssd,3)
#Part d (#1): determine λ.
#λ1, the aysmptotic finite rate of increase
L1 <- Re(eigs.A[["values"]][dom.pos])
L1
#Part d (#2): determine the elasticities.
#First, calculate reproductive value
M <- eigen(t(A))
v <- Re(M$vectors[,which.max(Re(M$values))])
RV <- v/v[1]
RV
#Next, calculate sensitivities
vw.s <- v %*% t(w)
(S <- vw.s/as.numeric(v %*% w))
max(S)
(sens <- which(S == max(S),arr.ind = TRUE))
A[sens]
#Finally, determine the elasticity of the projection matrix
elas <- (A/L1) * S
round(elas,3)
max(elas)
(sens <- which(elas == max(elas),arr.ind = TRUE))
elas[5,5]

#Problem 2.2
#Create a population projection matrix, and a vector of 
# stage class abundances for year zero
st1 <- c(0,0,0,0,127,4,80)
st2 <- c(0.6747,0.7370,0,0,0,0,0)
st3 <- c(0,0.0486,0.6610,0,0,0,0)
st4 <- c(0,0,0.0147,0.6907,0,0,0)
st5 <- c(0,0,0,0.0518,0,0,0)
st6 <- c(0,0,0,0,0.8091,0,0)
st7 <- c(0,0,0,0,0,0.8091,0.8089)
#Projection matrix
At <- matrix(c(st1,st2,st3,st4,st5,st6,st7),nr = 7,byrow = TRUE)
#Part b: determine the stable stage distribution
#Perform eigenanalysis on A to retrieve vectors and values,
# each of which provide a solution to Aw = λw 
eigs.At <- eigen(At)
eigs.At
#Next we explicitly find the index position of the largest 
# absolute value of the eigen-values
dom.post <- which.max(eigs.At[["values"]])
#We extract w, keeping just the real part, and divide it by
# its sum to get the SSD
wt <- Re(eigs.At[["vectors"]][,dom.post])
ssdt <- wt/sum(wt)
round(ssdt,3)
#Part c: determine λ
#λ1, the aysmptotic finite rate of increase
L1t <- Re(eigs.At[["values"]][dom.post])
L1t
#Part d: determine the elasticities
#First, calculate reproductive value
Mt <- eigen(t(At))
vt <- Re(Mt$vectors[,which.max(Re(Mt$values))])
RVt <- vt/vt[1]
RVt
#Next, calculate sensitivities
vw.st <- vt %*% t(wt)
(St <- vw.st/as.numeric(vt %*% wt))
max(St)
(senst <- which(St == max(St),arr.ind = TRUE))
At[senst]
#Finally, determine the elasticity of the projection matrix
elast <- (At/L1t) * St
round(elast,3)
max(elast)
(sens <- which(elast == max(elast),arr.ind = TRUE))
elast[4,4]
elast[3,3]
elast[2,2]
#Part e: predict long-term relative abundance for all stages
#Observed stage class abundances
Nt0 <- matrix(c(0,10,10,10,10,10,10),ncol = 1)
#First, project the population over 10 years, using a for-loop
yearst <- 10
Nt.projections <- matrix(0,nrow = nrow(At),ncol = yearst+1)
Nt.projections[,1] <- Nt0
#Now, perform the iteration of matrix multiplication between
# the projection matrix and N0
for (i in 1:yearst) Nt.projections[,i+1] <- At %*% Nt.projections[,i]
#Next, calculate Rt = Nt+1 /Nt for each year t
Nt.totals <- apply(Nt.projections, 2, sum)
#Next, get each Rt by dividing all the Nt+1 by each Nt
Rst <- Nt.totals[-1]/Nt.totals[-(yearst+1)]
#Finally, plot each R in each year t, rather than each year t+1
plot(0:(yearst-1), Rst,type = "b",xlab = "Year",ylab = "R")
#Part g: start with N = (0 10 10 10 10 10) and
# graph dynamics for all stages for 10 years
#New observed stage class abundances
Nn0 <- matrix(c(0,10,10,10,10,10,10),ncol = 1)
#Finally, graph the results for each stage
matplot(0:yearst, t(Nt.projections), type = "l",lty = 1:7,col = 1,
        ylab = "Stage Abundance",xlab = "Year")
legend("topleft", legend = c("S1","S2","S3","S4","S5",
                             "S6","S7"),lty = 1:6,col = 1,bty = "n")
  