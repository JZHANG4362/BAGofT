# missing main effect data
dag1<- function(n, bet){
  x1dat <- runif(n, -3, 3)
  x2dat <- rnorm(n, 0, 1)
  x3dat <- rchisq(n, 4)
  lindat <- x1dat * bet[1] + x2dat * bet[2] + x3dat * bet[3]
  pdat <- 1/(1 + exp(-lindat))
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat <- data.frame(y = ydat, x1 = x1dat, x2 = x2dat,
                    x3 = x3dat)
  return(dat)
}



# missing interaction data
dag2 <- function(n, bet){
  x1dat <- runif(n, -3, 3)
  x2dat <- runif(n, -3, 3)
  x3dat <- x1dat * x2dat
  lindat <- x1dat * bet[1] +
    x2dat * bet[2] + x3dat * bet[3]
  pdat <- 1/(1 + exp(-lindat))
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat <- data.frame(y = ydat, x1 = x1dat, x2 = x2dat)
  return(dat)
}

# missing quadratic term data
dag3 <- function(n, bet){
  x1dat <- runif(n, -3, 3)
  x2dat <- rnorm(n, 0, 1)
  x3dat <- rchisq(n, 2)
  lindat <-  x1dat *  bet[1] +
    x2dat *  bet[2] + x3dat *  bet[3] + x1dat^2 *  bet[4]
  pdat <- 1/(1 + exp(-lindat) )
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat <- data.frame(y = ydat, x1 = x1dat, x2 = x2dat,
                    x3 = x3dat)
  return(dat)
}

# missing interaction
dag3 <- function(n, bet){
  x1dat <- runif(n, -3, 3)
  x2dat <- rnorm(n, 0, 1)
  x3dat <- rchisq(n, 2)
  lindat <-  x1dat *  bet[1] +
    x2dat *  bet[2] + x3dat *  bet[3] + x1dat^2 *  bet[4]
  pdat <- 1/(1 + exp(-lindat) )
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat <- data.frame(y = ydat, x1 = x1dat, x2 = x2dat,
                    x3 = x3dat)
  return(dat)
}
##################################################################
# Generate data 1_100
##################################################################



set.seed(20)
n <- 100
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep))

genDat1_100 <- lapply(c(1:nrep), function(x){dag1(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat1_100, file = "genDat1_100.rda")








##################################################################
# Generate data 1_200
##################################################################



set.seed(20)
n <- 200
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep))

genDat1_200 <- lapply(c(1:nrep), function(x){dag1(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat1_200, file = "genDat1_200.rda")



##################################################################
# Generate data 1_800
##################################################################



set.seed(20)
n <- 800
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep))

genDat1_800 <- lapply(c(1:nrep), function(x){dag1(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat1_800, file = "genDat1_800.rda")

##################################################################
# Generate data 2_100
##################################################################



set.seed(20)
n <- 100
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep))

genDat2_100 <- lapply(c(1:nrep), function(x){dag2(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat2_100, file = "genDat2_100.rda")


##################################################################
# Generate data 2_200
##################################################################



set.seed(20)
n <- 200
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep))

genDat2_200 <- lapply(c(1:nrep), function(x){dag2(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat2_200, file = "genDat2_200.rda")




##################################################################
# Generate data 2_800
##################################################################



set.seed(20)
n <- 800
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep))

genDat2_800 <- lapply(c(1:nrep), function(x){dag2(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat2_800, file = "genDat2_800.rda")

##################################################################
# Generate data 3_100
##################################################################



set.seed(20)
n <- 100
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep),
                      beta4 =  rnorm(nrep, mean = 1))

genDat3_100 <- lapply(c(1:nrep), function(x){dag3(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat3_100, file = "genDat3_100.rda")


##################################################################
# Generate data 3_200
##################################################################



set.seed(20)
n <- 200
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep),
                      beta4 =  rnorm(nrep, mean = 1))

genDat3_200 <- lapply(c(1:nrep), function(x){dag3(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat3_200, file = "genDat3_200.rda")



##################################################################
# Generate data 3_800
##################################################################



set.seed(20)
n <- 800
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep),
                      beta4 =  rnorm(nrep, mean = 1))

genDat3_800 <- lapply(c(1:nrep), function(x){dag3(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat3_800, file = "genDat3_800.rda")





# library("ResourceSelection")
# HLvec <- numeric(length(genDat3_800))
# for (j in c(1:length(genDat3_800))){
#   lfitAD3 <- glm(y~x1 + x2 + x3, family = binomial("logit"), 
#                  data = genDat3_800[[j]])
#   HLAD3 <- hoslem.test(genDat3_800[[j]]$y, fitted(lfitAD3), g=10)
#   HLvec[j] <- HLAD3$p.value
# }
# 
# 
# BAGofT(data = genDat3_800[[4]],  g = 5, nsplits = 100,
#        formula = y ~ x1 + x2  , adj = TRUE, parVar = c("x1", "x2", "x3"),
#        link = "logit", nsim = 10)

