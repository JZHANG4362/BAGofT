
source(file = "parRF.R")
source(file = "testNeu3.R")
source(file = "BAGofT.R")
source(file = "dcPre.R")

library(keras)

load(file = "testDat1.rda")


fm <- y ~ .
result <- list()

testFun <- function(ne){
  set.seed(20)
  system.time( for(k in c(1:length(testDat1)) ){
    message(paste("replication: ",k))
    tryCatch({ result[[k]]  <- BAGofT(testModel = testNeu3(formula = y~., units1 = 80,units2 = 5, epochs = 10, 
                                                           batch_size = 5), 
                                      parFun = parRF(preFun = dcPre()),
                                      data =  testDat1[[k]], 
                                      ne = ne,
                                      nsplits = 40,
                                      nsim = 100)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
  } )
  
  return(result)
}

result1 <- testFun(250)

fname <- paste("Neu3.rda")
save(result1, file = fname)