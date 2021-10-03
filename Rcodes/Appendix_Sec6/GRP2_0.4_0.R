load(file = "hdDat2_0.4_0.rda")

fm <- y ~ x1 + x2 + x3 + x4 + x5
library(GRPtests)

result <- list()
system.time( for(k in c(1:length(hdDat2_0.4_0)) ){
  message(paste("replication: ",k))
  tryCatch({ result[[k]]  <- GRPtest( X = as.matrix(hdDat2_0.4_0[[k]][,-1]), y = hdDat2_0.4_0[[k]]$y, fam = c("binomial"), penalize = TRUE)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )


save( result, 
      file = "GRP_2_0.4_0_lasso.rda")