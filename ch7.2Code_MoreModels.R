#Define a function for implementing Pimm and Lawton's methods,
# where the input args define intraspecific negative density dependance
pimmlawton <- function (j1, j2, j3, j4){
  #Define the food web matrix
  Aq = matrix(c(j1, -10, 0, 0,
                0.1, j2, -10, 0,
                0, 0.1, j3, -10,
                0, 0, 0.1, j4), nrow = 4, byrow = TRUE)
  #Find how many species, S , are in the web
  S <- nrow(Aq)
  #Next, we create a random realization of this matrix
  M <- Aq * runif(S^2)
  #Next we perform eigenanalysis on it, retaining the eigenvalues
  eM <- eigen(M)[["values"]]
  #Record the dominant eigenvalue
  deM <- max(Re(eM))
  sprintf("Dominant eigenvalue: %g", deM)
  #Determine the average intraspecific negative density dependence
  intraNDD <- sqrt(sum(diag(M)^2)/S)
  sprintf("Average intraspecific DD: %g", intraNDD)
  #Calculate the interacton strength
  diag(M) <- 0
  IS <- sqrt(sum(M^2)/(S * (S - 1)))
  sprintf("Interaction strength: %g", IS)
  #Find the return time for the dominant eigenvalue  
  RT.M <- -1/deM
  sprintf("Return time: %g", RT.M)
}

#Test a variety of intraspecific negative density dependance interactions
# with varying degrees average magnitudes of negative density dependence 
pimmlawton(-1,0,0,0)
pimmlawton(-1,-1,-1,-1)
pimmlawton(-1,0,0,-1)
pimmlawton(-10,0,0,0)
pimmlawton(-10,-10,-10,-10)
pimmlawton(-10,0,0,-10)

