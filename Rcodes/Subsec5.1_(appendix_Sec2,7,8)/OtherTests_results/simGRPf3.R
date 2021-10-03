
library("GRPtests")
############################################################
# case1 100
############################################################
#=========================alternative========================
load(file = "genDat1_100.rda")

set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat1_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat1_100[[k]][,c("x1", "x2", "x3")])
    Ymat <- genDat1_100[[k]]$y
    result[[k]] <-GRPgrouptest(X = Xmat, y = Ymat, fam = c("binomial"), G = 3 )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP1_100_1_fl.rda")


############################################################
# case1 200
############################################################
#=========================alternative========================
load(file = "genDat1_200.rda")

set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat1_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat1_200[[k]][,c("x1", "x2", "x3")])
    Ymat <- as.matrix(genDat1_200[[k]]$y)
    result[[k]] <-GRPgrouptest(X = Xmat, y = Ymat, fam = c("binomial"), G = 3 )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP1_200_1_fl.rda")

############################################################
# case1 800
############################################################
#=========================alternative========================
load(file = "genDat1_800.rda")
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat1_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat1_800[[k]][,c("x1", "x2", "x3")])
    Ymat <- genDat1_800[[k]]$y
    result[[k]] <-GRPgrouptest(X = Xmat, y = Ymat, fam = c("binomial"), G = 3 )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP1_800_1_fl.rda")

