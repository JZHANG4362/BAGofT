
source(file = "BAGofT.R")
source(file = "testGlmnet.R")
source(file = "parRF.R")
source(file = "dcPre.R")

load(file = "hdDat1_0.4_2.rda")

fm <- y ~ x1 + x2 + x3 + x4 + x5
result <- list()
system.time( for(k in c(1:length(hdDat1_0.4_2)) ){
  message(paste("replication: ",k))
  tryCatch({ result[[k]]  <- BAGofT(testModel = testGlmnet(formula = fm, alpha = 1), 
                                    parFun = parRF(preFun = dcPre()),
                                    data = hdDat1_0.4_2[[k]], 
                                    nsplits = 30, nsim = 50)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )


save( result, 
      file = "simHD_BT_1_0.4_2_lasso.rda")


