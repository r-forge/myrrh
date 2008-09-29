#********************************************************************************
# Harmonic Nonnegative Matrix Aapproximation
#
MIR.Tools.HNNMA <- function(X, initialA, maxIterations, theta=1, epsilon=0.000001, binsPerOctave=36, silent=FALSE)
{
  squared_sum <- 0
  sums <- rep(0, iterations)

  # Matrix initialization
  A <- initialA
  basis_size <- ncol(A)
  for(atom in 1:dim(A)[2])   #... and normalization of every column
    A[,atom] <- A[,atom] / sum(A[,atom])
  S <- matrix(abs(rnorm(dim(X)[2]*basis_size)), ncol=dim(X)[2])

  objective <- sum((X - A%*%S)^2)
  objHistory <- c(objective)
  if(!silent)
    cat("Objective = ", objective, "\n")

  # Columnd and row matrices filled w/ ones
  onesMH <- matrix(rep(1, dim(X)[1]), ncol=1)  # too much memory wasted!
  onesMV <- matrix(rep(1, dim(X)[2]), nrow=1)

  # Shift matrices
  SM1 <- matrix(rep(0, dim(A)[1] ^ 2), ncol=dim(A)[1])  # down-shifting matrix
  SM2 <- matrix(rep(0, dim(A)[2] ^ 2), ncol=dim(A)[2])  # right-shifting matrix
  shift1 <- binsPerOctave / 12
  shift2 <- 1
  for(jj in 1:(nrow(SM1)-shift1))
    SM1[jj+shift1,jj] <- 1
  for(jj in 1:(nrow(SM2)-shift2))
    SM2[jj,jj+shift2] <- 1

  eta <- 1

  for(iteration in 1:iterations)
  {
    if(!silent)
      cat("Iteration ", iteration, " ")

    temp1 <- 2*A - 2 * t(SM1)%*%A%*%t(SM2) - 2 * SM1%*%A%*%SM2 + 2 * t(SM1)%*%SM1%*%A%*%SM2%*%t(SM2)
    temp1 <- ifelse(temp1 > 0, temp1, 0)
    A <- A * eta * ((X / (A %*% S + 1e-9)) %*% t(S)) / (onesMH %*% matrix(rowSums(S), nrow=1) + theta*temp1)
    A <- A / max(A)
    S <- S * (t(A) %*% (X / (A %*% S + 1e-9))) / (matrix(colSums(A), ncol=1) %*% onesMV)
    S <- S / max(S)

    # End condition check
    objective <- sum((X - A%*%S)^2)
    objHistory <- c(objHistory, objective)
    if(!silent)
      cat("\t objective = ", objective, "\n")
    if(((abs(objHistory[length(objHistory)-1] - objective) / objHistory[length(objHistory)-1]) < epsilon) && (iteration > 1))
      break
  }

  return(list(A=A,S=S))
}
