
library("ResourceSelection")

############################################################
# case2 100
############################################################
#=========================alternative========================
load(file = "genDat2_100.rda")

##########
# error 66,91
##########
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat2_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat2_100[[k]][,c("x1", "x2")])
    Ymat <- genDat2_100[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP2_100_1_fl.rda")

#====================null===========================
##########
# error 7, 14, 27, 29,31,36,37,39,48,50,69,74,82,96
##########


set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat2_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- cbind(as.matrix(genDat2_100[[k]][,c("x1", "x2")]), as.matrix(genDat2_100[[k]]$x1*genDat2_100[[k]]$x2))
    colnames(Xmat) <- c("x1", "x2", "x3")
    Ymat <- genDat2_100[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP2_100_2_fl.rda")

############################################################
# case2 200
############################################################
#=========================alternative========================
load(file = "genDat2_200.rda")
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat2_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat2_200[[k]][,c("x1", "x2")])
    Ymat <- genDat2_200[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP2_200_1_fl.rda")

#====================null===========================
##########
# error 14, 36
##########


set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat2_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- cbind(as.matrix(genDat2_200[[k]][,c("x1", "x2")]), as.matrix(genDat2_200[[k]]$x1*genDat2_200[[k]]$x2))
    colnames(Xmat) <- c("x1", "x2", "x3")
    Ymat <- genDat2_200[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP2_200_2_fl.rda")


############################################################
# case2 800
############################################################
#=========================alternative========================
load(file = "genDat2_800.rda")
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat2_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat2_800[[k]][,c("x1", "x2")])
    Ymat <- genDat2_800[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP2_800_1_fl.rda")

#====================null===========================
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat2_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- cbind(as.matrix(genDat2_800[[k]][,c("x1", "x2")]), as.matrix(genDat2_800[[k]]$x1*genDat2_800[[k]]$x2))
    colnames(Xmat) <- c("x1", "x2", "x3")
    Ymat <- genDat2_800[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP2_800_2_fl.rda")


############################################################
# case3 100
############################################################
#=========================alternative========================
load(file = "genDat3_100.rda")

##########
# error 5,7,19,20,22,23,31,36,48,56,58,59,77,79,81,96
##########
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat3_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat3_100[[k]][,c("x1", "x2", "x3")])
    Ymat <- genDat3_100[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP3_100_1_fl.rda")

#====================null===========================
##########
# error 1,5,7,10,11,14,19,20,22,25,27,31,33,35,36,38,41,42,43,45,48,49,50,53,54,55,56,58,59,61,62,63,65,66,67,68,71,72,74,76,77,79,80,81,83,85,86,88,91,95,96,99
##########


set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat3_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- cbind(as.matrix(genDat3_100[[k]][,c("x1", "x2", "x3")]), as.matrix(genDat3_100[[k]]$x1^2))
    colnames(Xmat) <- c("x1", "x2", "x3", "x4")
    Ymat <- genDat3_100[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP3_100_2_fl.rda")


############################################################
# case3 200
############################################################
#=========================alternative========================
load(file = "genDat3_200.rda")

##########
# error 5,20,22,31,36,48,58,96
##########
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat3_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat3_200[[k]][,c("x1", "x2", "x3")])
    Ymat <- genDat3_200[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP3_200_1_fl.rda")

#====================null===========================
##########
# error 5,7,15,20,22,31,36,38,41,42,48,50,53,59,61,67,68,74,80,81,86,88,90,96,97
##########


set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat3_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- cbind(as.matrix(genDat3_200[[k]][,c("x1", "x2", "x3")]), as.matrix(genDat3_200[[k]]$x1^2))
    colnames(Xmat) <- c("x1", "x2", "x3", "x4")
    Ymat <- genDat3_200[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP3_200_2_fl.rda")

############################################################
# case3 800
############################################################
#=========================alternative========================
load(file = "genDat3_800.rda")

##########
# error 31, 74
##########
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat3_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat3_800[[k]][,c("x1", "x2", "x3")])
    Ymat <- genDat3_800[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP3_800_1_fl.rda")

#====================null===========================
##########
# error 5, 15, 19,20,22,3136,38,48,55,74,81,96
##########


set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat3_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- cbind(as.matrix(genDat3_800[[k]][,c("x1", "x2", "x3")]), as.matrix(genDat3_800[[k]]$x1^2))
    colnames(Xmat) <- c("x1", "x2", "x3", "x4")
    Ymat <- genDat3_800[[k]]$y
    result[[k]] <-GRPtest(X = Xmat, y = Ymat, fam = c("binomial") )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP3_800_2_fl.rda")




