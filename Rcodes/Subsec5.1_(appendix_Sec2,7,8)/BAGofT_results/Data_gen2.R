
# correspond to dag1, dag2 and dag3

# missing main effect data
dag4<- function(n, bet){
  x1dat <- runif(n, -3, 3)
  x2dat <- rnorm(n, 0, 1)
  x3dat <- rchisq(n, 4)
  lindat <- x1dat * bet[1] + x2dat * bet[2] + 0.5 * x3dat * bet[3]
  pdat <- 1/(1 + exp(-lindat))
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat <- data.frame(y = ydat, x1 = x1dat, x2 = x2dat,
                    x3 = x3dat)
  return(dat)
}



# missing interaction data
dag5 <- function(n, bet){
  x1dat <- runif(n, -3, 3)
  x2dat <- runif(n, -3, 3)
  x3dat <- x1dat * x2dat
  lindat <- x1dat * bet[1] +
    x2dat * bet[2] + 0.5 * x3dat * bet[3]
  pdat <- 1/(1 + exp(-lindat))
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat <- data.frame(y = ydat, x1 = x1dat, x2 = x2dat)
  return(dat)
}

# missing quadratic term data
dag6 <- function(n, bet){
  x1dat <- runif(n, -3, 3)
  x2dat <- rnorm(n, 0, 1)
  x3dat <- rchisq(n, 2)
  lindat <-  x1dat *  bet[1] +
    x2dat *  bet[2] + x3dat *  bet[3] + 0.5 * x1dat^2 *  bet[4]
  pdat <- 1/(1 + exp(-lindat) )
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat <- data.frame(y = ydat, x1 = x1dat, x2 = x2dat,
                    x3 = x3dat)
  return(dat)
}

##################################################################
# Generate data 4_100
##################################################################



set.seed(20)
n <- 100
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep))

genDat4_100 <- lapply(c(1:nrep), function(x){dag4(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat4_100, file = "genDat4_100.rda")


##################################################################
# Generate data 4_200
##################################################################



set.seed(20)
n <- 200
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep))

genDat4_200 <- lapply(c(1:nrep), function(x){dag4(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat4_200, file = "genDat4_200.rda")



##################################################################
# Generate data 4_800
##################################################################



set.seed(20)
n <- 800
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep))

genDat4_800 <- lapply(c(1:nrep), function(x){dag4(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat4_800, file = "genDat4_800.rda")

##################################################################
# Generate data 5_100
##################################################################



set.seed(20)
n <- 100
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep))

genDat5_100 <- lapply(c(1:nrep), function(x){dag5(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat5_100, file = "genDat5_100.rda")

##################################################################
# Generate data 5_200
##################################################################



set.seed(20)
n <- 200
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep))

genDat5_200 <- lapply(c(1:nrep), function(x){dag5(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat5_200, file = "genDat5_200.rda")




##################################################################
# Generate data 5_800
##################################################################



set.seed(20)
n <- 800
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep))

genDat5_800 <- lapply(c(1:nrep), function(x){dag5(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat5_800, file = "genDat5_800.rda")

##################################################################
# Generate data 6_100
##################################################################



set.seed(20)
n <- 100
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep),
                      beta4 =  rnorm(nrep, mean = 1))

genDat6_100 <- lapply(c(1:nrep), function(x){dag6(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat6_100, file = "genDat6_100.rda")



##################################################################
# Generate data 6_200
##################################################################



set.seed(20)
n <- 200
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep),
                      beta4 =  rnorm(nrep, mean = 1))

genDat6_200 <- lapply(c(1:nrep), function(x){dag6(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat6_200, file = "genDat6_200.rda")



##################################################################
# Generate data 6_800
##################################################################



set.seed(20)
n <- 800
nrep <- 100

betaDat <- data.frame(beta1 = rnorm(nrep),
                      beta2 = rnorm(nrep),  beta3 = rnorm(nrep),
                      beta4 =  rnorm(nrep, mean = 1))

genDat6_800 <- lapply(c(1:nrep), function(x){dag6(n = n, bet = as.numeric(betaDat[x, ]))})

save(genDat6_800, file = "genDat6_800.rda")


# 
# 
# 
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
# BAGofT(data = genDat6_800[[4]],  nsplits = 100,
#        formula = y ~ x1 + x2  , adj = TRUE, parVar = c("x1", "x2", "x3"),
#        link = "logit", nsim = 10)
# 
