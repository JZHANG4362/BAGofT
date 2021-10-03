
source(file = "BAGofT.R")
source(file = "testGlmBi.R")
source(file = "parRF.R")
source(file = "dcPre.R")

load(file = "hdDat1_0.4_2.rda")

fm <- y ~ x1 + x2 + x3 + x4 + x5
result <- list()
system.time( for(k in c(1:length(hdDat1_0.4_2)) ){
  message(paste("replication: ",k))
  tryCatch({ result[[k]]  <- BAGofT(testModel = testGlmBi(formula = fm, link = "logit"), 
                                    parFun = parRF(preFun = dcPre(npreSel = 500)),
                                    data = hdDat1_0.4_2[[k]], 
                                    nsplits = 1, nsim = 0)}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )


save( result, 
      file = "simHD_BT_3_0.4_2.rda")


