
dag1 <- function(n){
  n <- 500
  x1dat <- 1.5 * rnorm(n)
  x2dat <- rnorm(n)
  x3dat <- rnorm(n)
  x4dat <- rnorm(n)
  lindat <-  sin(x1dat) + 1.8 * x2dat * x3dat + x4dat
  pdat <- 1/(1 + exp(-lindat))
  #pdat <- (lindat + 1)/2
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat <- data.frame(y = ydat, x1 = x1dat, x2 = x2dat, x3 = x3dat, x4 = x4dat)
}


#======================case1======================
set.seed(20)
n <- 500
nrep <- 100


#omega <- 0.8

testDat1 <- lapply(c(1:nrep), function(x){dag1(n = n)})

save(testDat1, file = "testDat1.rda")

