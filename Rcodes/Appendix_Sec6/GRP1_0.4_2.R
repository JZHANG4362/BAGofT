load(file = "hdDat1_0.4_2.rda")

fm <- y ~ x1 + x2 + x3 + x4 + x5
library(GRPtests)

result <- list()
system.time( for(k in c(1:length(hdDat1_0.4_2)) ){
  message(paste("replication: ",k))
  tryCatch({ result[[k]]  <- GRPtest( X = as.matrix(hdDat1_0.4_2[[k]][,-1]), y = hdDat1_0.4_2[[k]]$y, fam = c("binomial"), penalize = TRUE)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )


save( result, 
      file = "GRP_1_0.4_2_lasso.rda")