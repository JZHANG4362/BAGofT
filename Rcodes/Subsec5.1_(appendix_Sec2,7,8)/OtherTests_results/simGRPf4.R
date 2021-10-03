
library("GRPtests")
############################################################
# case4 100
############################################################
#=========================alternative========================
load(file = "genDat4_100.rda")

set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat4_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat4_100[[k]][,c("x1", "x2", "x3")])
    Ymat <- genDat4_100[[k]]$y
    result[[k]] <-GRPgrouptest(X = Xmat, y = Ymat, fam = c("binomial"), G = 3 )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP4_100_1_fl.rda")




############################################################
# case4 200
############################################################
#=========================alternative========================
load(file = "genDat4_200.rda")
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat4_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat4_200[[k]][,c("x1", "x2", "x3")])
    Ymat <- genDat4_200[[k]]$y
    result[[k]] <-GRPgrouptest(X = Xmat, y = Ymat, fam = c("binomial"), G = 3 )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP4_200_1_fl.rda")



############################################################
# case4 800
############################################################
#=========================alternative========================
load(file = "genDat4_800.rda")
set.seed(20)
result <- list()
system.time( for(k in c(1:length(genDat4_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    Xmat <- as.matrix(genDat4_800[[k]][,c("x1", "x2", "x3")])
    Ymat <- genDat4_800[[k]]$y
    result[[k]] <-GRPgrouptest(X = Xmat, y = Ymat, fam = c("binomial"), G = 3 )
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]])) < 0.05)

save( result, 
      file = "simGRP4_800_1_fl.rda")


