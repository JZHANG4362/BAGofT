# missing interaction data
dag1 <- function(n, rho, p, sig){
  mat1 <- matrix(rep(c(1:p), p), nrow = p, byrow = TRUE)
  mat2 <- matrix(rep(c(1:p), p), nrow = p, byrow = FALSE)
  covMat <- matrix(rho, nrow = p, ncol = p)^abs(mat1 - mat2)
  library(MASS)
  Xdat <- mvrnorm(n, rep(0, p), covMat)
  colnames(Xdat) <- paste("x", c(1:p), sep = "")
  betaVec <- rnorm(6)
  lindat <-  betaVec[1] * Xdat[,1] + betaVec[2] * Xdat[,2] + betaVec[3] * Xdat[,3] + 
    betaVec[4] * Xdat[,4] + betaVec[5] * Xdat[,5] + 
    sig * betaVec[6] *Xdat[,1] * Xdat[,2]
  pdat <- 1/(1 + exp(-lindat))
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat <- data.frame(y = ydat, Xdat)
  return(dat)
}


# missing quadratic effect data
dag2 <- function(n, rho, p, sig){
  mat1 <- matrix(rep(c(1:p), p), nrow = p, byrow = TRUE)
  mat2 <- matrix(rep(c(1:p), p), nrow = p, byrow = FALSE)
  covMat <- matrix(rho, nrow = p, ncol = p)^abs(mat1 - mat2)
  library(MASS)
  Xdat <- mvrnorm(n, rep(0, p), covMat)
  colnames(Xdat) <- paste("x", c(1:p), sep = "")
  betaVec <- rnorm(6)
  lindat <-  betaVec[1] * Xdat[,1] + betaVec[2] * Xdat[,2] + betaVec[3] * Xdat[,3] + 
    betaVec[4] * Xdat[,4] + betaVec[5] * Xdat[,5] + 
    sig * betaVec[6] *Xdat[,1]^2/2
  pdat <- 1/(1 + exp(-lindat))
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat <- data.frame(y = ydat, Xdat)
  return(dat)
}

##################################################################
# Generate data hdDat1_0.4_2
##################################################################



set.seed(20)
n <- 800
p <- 500
rho <- 0.4
nrep <- 100
sig <- 2

hdDat1_0.4_2 <- lapply(c(1:nrep), function(x){dag1(n = n, rho = rho, p = p, sig = sig)})

save(hdDat1_0.4_2, file = "hdDat1_0.4_2.rda")

##################################################################
# Generate data hdDat1_0.4_1
##################################################################
set.seed(20)
n <- 800
p <- 500
rho <- 0.4
nrep <- 100
sig <- 1

hdDat1_0.4_1 <- lapply(c(1:nrep), function(x){dag1(n = n, rho = rho, p = p, sig = sig)})

save(hdDat1_0.4_1, file = "hdDat1_0.4_1.rda")


##################################################################
# Generate data hdDat1_0.4_0
##################################################################
set.seed(20)
n <- 800
p <- 500
rho <- 0.4
nrep <- 100
sig <- 0

hdDat1_0.4_0 <- lapply(c(1:nrep), function(x){dag1(n = n, rho = rho, p = p, sig = sig)})

save(hdDat1_0.4_0, file = "hdDat1_0.4_0.rda")


##################################################################
# Generate data hdDat2_0.4_2
##################################################################



set.seed(20)
n <- 800
p <- 500
rho <- 0.4
nrep <- 100
sig <- 2

hdDat2_0.4_2 <- lapply(c(1:nrep), function(x){dag2(n = n, rho = rho, p = p, sig = sig)})

save(hdDat2_0.4_2, file = "hdDat2_0.4_2.rda")

##################################################################
# Generate data hdDat2_0.4_1
##################################################################
set.seed(20)
n <- 800
p <- 500
rho <- 0.4
nrep <- 100
sig <- 1

hdDat2_0.4_1 <- lapply(c(1:nrep), function(x){dag2(n = n, rho = rho, p = p, sig = sig)})

save(hdDat2_0.4_1, file = "hdDat2_0.4_1.rda")


##################################################################
# Generate data hdDat2_0.4_0
##################################################################
set.seed(20)
n <- 800
p <- 500
rho <- 0.4
nrep <- 100
sig <- 0

hdDat2_0.4_0 <- lapply(c(1:nrep), function(x){dag2(n = n, rho = rho, p = p, sig = sig)})

save(hdDat2_0.4_0, file = "hdDat2_0.4_0.rda")

