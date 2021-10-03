# no finite sample correction


source(file = "BAGofT.R")
source(file = "testGlmBi.R")
source(file = "parRF.R")

load(file = "genDat5_200.rda")

fm <- y ~ x1 + x2
result <- list()
system.time( for(k in c(1:length(genDat5_200)) ){
  message(paste("replication: ",k))
  tryCatch({ result[[k]]  <- BAGofT(testModel = function(Train.data, Test.data){testGlmBi(formula = fm, link = "logit", Train.data, Test.data)}, 
                                    parFun = parRF,
                                    data = genDat5_200[[k]], 
                                    nsplits = 40,
                                    nsim = 100)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )


save( result, 
      file = "sim5_200_1_fl.rda")

