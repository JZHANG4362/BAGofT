
source(file = "parRF.R")
source(file = "testGlmnet.R")
source(file = "BAGofT.R")
source(file = "dcPre.R")

load(file = "npDat.rda")

fm <- y ~ .
result <- list()

testFun <- function(ne){
  set.seed(20)
  system.time( for(k in c(1:length(npDat)) ){
    message(paste("replication: ",k))
    tryCatch({ result[[k]]  <- BAGofT(testModel = testGlmnet(formula = y~., alpha = 1), 
                                      parFun = parRF(preFun = dcPre()),
                                      data =  npDat[[k]], 
                                      ne = ne,
                                      nsplits = 40,
                                      nsim = 100)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  } )
  
  return(result)
}
ne <- 500/10
result1 <- testFun(ne)

save(result1, file = "Glmnet1_1.rda")