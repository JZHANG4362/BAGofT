
source(file = "parRF.R")
source(file = "testGlmnet.R")
source(file = "BAGofT.R")
source(file = "dcPre.R")

load(file = "pmDat.rda")

fm <- y ~ .
result <- list()

testFun <- function(ne){
  set.seed(20)
  system.time( for(k in c(1:length(pmDat)) ){
    message(paste("replication: ",k))
    tryCatch({ result[[k]]  <- BAGofT(testModel = testGlmnet(formula = y~., alpha = 1), 
                                      parFun = parRF(preFun = dcPre()),
                                      data =  pmDat[[k]], 
                                      ne = ne,
                                      nsplits = 40,
                                      nsim = 100)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  } )
  
  return(result)
}
ne <- 500/4
result1 <- testFun(ne)

save(result1, file = "Glmnet2_2.rda")