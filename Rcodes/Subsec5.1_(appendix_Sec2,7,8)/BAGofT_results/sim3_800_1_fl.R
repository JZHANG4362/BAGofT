# no finite sample correction


source(file = "BAGofT.R")
source(file = "testGlmBi.R")
source(file = "parRF.R")

load(file = "genDat3_800.rda")

fm <- y ~ x1 + x2 + x3
result <- list()
system.time( for(k in c(1:length(genDat3_800)) ){
  message(paste("replication: ",k))
  tryCatch({ result[[k]]  <- BAGofT(testModel = function(Train.data, Test.data){testGlmBi(formula = fm, link = "logit", Train.data, Test.data)}, 
                                    parFun = parRF,
                                    data = genDat3_800[[k]], 
                                    nsplits = 40,
                                    nsim = 100)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )


save( result, 
      file = "sim3_800_1_fl.rda")




