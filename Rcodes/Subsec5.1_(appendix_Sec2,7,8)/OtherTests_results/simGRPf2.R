
library("ResourceSelection")
############################################################
# case5 100
############################################################
#=========================alternative========================
load(file = "genDat5_100.rda")

##########
# error 28
##########
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat5_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat5_100[[k]][,c("x1", "x2")])
    Ymat <- genDat5_100[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP5_100_1_fl.rda")

#====================null===========================
##########
# error 14，29，48，74，96
##########


set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat5_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- cbind(as.matrix(genDat5_100[[k]][,c("x1", "x2")]), as.matrix(genDat5_100[[k]]$x1*genDat5_100[[k]]$x2))
    colnames(Xmat) <- c("x1", "x2", "x3")
    Ymat <- genDat5_100[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP5_100_2_fl.rda")

############################################################
# case5 200
############################################################
#=========================alternative========================
load(file = "genDat5_200.rda")
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat5_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat5_200[[k]][,c("x1", "x2")])
    Ymat <- genDat5_200[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP5_200_1_fl.rda")

#====================null===========================
##########
# error 29
##########


set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat5_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- cbind(as.matrix(genDat5_200[[k]][,c("x1", "x2")]), as.matrix(genDat5_200[[k]]$x1*genDat5_200[[k]]$x2))
    colnames(Xmat) <- c("x1", "x2", "x3")
    Ymat <- genDat5_200[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP5_200_2_fl.rda")


############################################################
# case5 800
############################################################
#=========================alternative========================
load(file = "genDat5_800.rda")
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat5_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat5_800[[k]][,c("x1", "x2")])
    Ymat <- genDat5_800[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP5_800_1_fl.rda")

#====================null===========================
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat5_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- cbind(as.matrix(genDat5_800[[k]][,c("x1", "x2")]), as.matrix(genDat5_800[[k]]$x1*genDat5_800[[k]]$x2))
    colnames(Xmat) <- c("x1", "x2", "x3")
    Ymat <- genDat5_800[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP5_800_2_fl.rda")


############################################################
# case6 100
############################################################
#=========================alternative========================
load(file = "genDat6_100.rda")

##########
# error 5,7,14,20,22,31,32,36,48,50,56,77,80,91
##########
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat6_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat6_100[[k]][,c("x1", "x2", "x3")])
    Ymat <- genDat6_100[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP6_100_1_fl.rda")

#====================null===========================
##########
# error 5, 7, 14,19,20,22,27,31,33,34,35,36,38,48,49,50,54,56,58,59,61,62,63,68,72,77,79,80,81,82,91,99
##########


set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat6_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- cbind(as.matrix(genDat6_100[[k]][,c("x1", "x2", "x3")]), as.matrix(genDat6_100[[k]]$x1^2))
    colnames(Xmat) <- c("x1", "x2", "x3", "x4")
    Ymat <- genDat6_100[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP6_100_2_fl.rda")


############################################################
# case6 200
############################################################
#=========================alternative========================
load(file = "genDat6_200.rda")

##########
# error 5, 20,90
##########
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat6_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat6_200[[k]][,c("x1", "x2", "x3")])
    Ymat <- genDat6_200[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP6_200_1_fl.rda")

#====================null===========================
##########
# error 5, 20,22,31,36,50,81,90
##########


set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat6_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- cbind(as.matrix(genDat6_200[[k]][,c("x1", "x2", "x3")]), as.matrix(genDat6_200[[k]]$x1^2))
    colnames(Xmat) <- c("x1", "x2", "x3", "x4")
    Ymat <- genDat6_200[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP6_200_2_fl.rda")

############################################################
# case6 800
############################################################
#=========================alternative========================
load(file = "genDat6_800.rda")

##########
# error 22,31,74
##########
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat6_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat6_800[[k]][,c("x1", "x2", "x3")])
    Ymat <- genDat6_800[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP6_800_1_fl.rda")

#====================null===========================
##########
# error 5, 20, 22, 31, 36,74,
##########


set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat6_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- cbind(as.matrix(genDat6_800[[k]][,c("x1", "x2", "x3")]), as.matrix(genDat6_800[[k]]$x1^2))
    colnames(Xmat) <- c("x1", "x2", "x3", "x4")
    Ymat <- genDat6_800[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP6_800_2_fl.rda")




