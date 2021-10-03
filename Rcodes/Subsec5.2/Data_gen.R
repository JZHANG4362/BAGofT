# nonparametric setting
dag1 <- function(n, p){
  Xdat <- matrix(runif((n*p), -5,5), nrow = n, ncol = p)
  colnames(Xdat) <- paste("x", c(1:p), sep = "")
  betaVec <- rnorm(6)
  lindat <-  3 * (Xdat[,1] < 2 & Xdat[,1] > -2) + -3 * (Xdat[,1] > 2 | Xdat[,1] < -2) + 
    0.5 * (Xdat[,2] + Xdat[, 3] + Xdat[,4] + Xdat[, 5])
  pdat <- 1/(1 + exp(-lindat))
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat <- data.frame(y = ydat, Xdat)
  return(dat)
}

# parametric setting
dag2 <- function(n, p){
  Xdat <- matrix(runif((n*p), -5,5), nrow = n, ncol = p)
  colnames(Xdat) <- paste("x", c(1:p), sep = "")
  betaVec <- rnorm(6)
  lindat <-  0.5 * Xdat[,1] + 0.3 * Xdat[,2] + 0.1 * Xdat[, 3] + 
    0.1 * Xdat[,4] + 0.1 * Xdat[, 5]
  pdat <- 1/(1 + exp(-lindat))
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat <- data.frame(y = ydat, Xdat)
  return(dat)
}


##################################################################
# Generate nonparametric data
##################################################################

set.seed(20)
n <- 500
p <- 1000
nrep <- 100

npDat <- lapply(c(1:nrep), function(x){dag1(n = n, p = p)})

save(npDat, file = "npDat.rda")

##################################################################
# Generate parametric data
##################################################################

set.seed(20)
n <- 500
p <- 1000
nrep <- 100

pmDat <- lapply(c(1:nrep), function(x){dag2(n = n, p = p)})

save(pmDat, file = "pmDat.rda")