# no finite sample correction
# correctly specified


source(file = "BAGofT.R")
source(file = "testGlmBi.R")
source(file = "parRF.R")

load(file = "genDat2_800.rda")

fm <- y ~ x1 + x2 + x1:x2
result <- list()
system.time( for(k in c(1:length(genDat2_800)) ){
  message(paste("replication: ",k))
  tryCatch({ result[[k]]  <- BAGofT(testModel = function(Train.data, Test.data){testGlmBi(formula = fm, link = "logit", Train.data, Test.data)}, 
                                    parFun = parRF,
                                    data = genDat2_800[[k]], 
                                    nsplits = 40,
                                    nsim = 100)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )


save( result, 
      file = "sim2_800_2_fl.rda")

