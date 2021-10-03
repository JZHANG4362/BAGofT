
source(file = "BAGofT.R")
source(file = "testGlmBi.R")
source(file = "parRF.R")
source(file = "dcPre.R")

load(file = "hdDat1_0.4_1.rda")

fm <- y ~ x1 + x2 + x3 + x4 + x5
result <- list()
system.time( for(k in c(1:length(hdDat1_0.4_1)) ){
  message(paste("replication: ",k))
  tryCatch({ result[[k]]  <- BAGofT(testModel = testGlmBi(formula = fm, link = "logit"), 
                                    parFun = parRF(preFun = dcPre()),
                                    data = hdDat1_0.4_1[[k]], 
                                    nsplits = 30, nsim = 50)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )


save( result, 
      file = "simHD_BT_1_0.4_1.rda")


