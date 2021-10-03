
source(file = "parRF.R")
source(file = "testRF.R")
source(file = "BAGofT.R")
source(file = "dcPre.R")

load(file = "npDat.rda")

fm <- y ~ .
result <- list()

testFun <- function(ne){
  set.seed(20)
  system.time( for(k in c(1:length(npDat)) ){
    message(paste("replication: ",k))
    tryCatch({ result[[k]]  <- BAGofT(testModel = testRF(formula = y~.,maxnodes = 10), 
                                      parFun = parRF(preFun = dcPre()),
                                      data =  npDat[[k]], 
                                      ne = ne,
                                      nsplits = 20,
                                      nsim = 60)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  } )
  
  return(result)
}
ne <- 500/4
result1 <- testFun(ne)

save(result1, file = "RF1_2.rda")