
alpha <- 0.05
######################################################
# rejection count under the alternative for BAGofT
######################################################

for (set in c(1:6)){
  rejM <- numeric(3)
  ErrorCount <- numeric(3)
  for (i in c(1:3)){
    nsize <-  c(100, 200, 800)[i]
    eval(parse(text = paste0("load(file = 'sim",set,"_",nsize,"_1_fl.rda')")) )
    pval <- unlist(lapply(c(1:100), function(x)result[[x]]$p.value))
    ErrorCount[i] <- sum(is.na(pval))
    pval2 <- pval[!is.na(pval)]
    rejM[i] <- mean(pval2 < alpha)
  }
  pRes <- list(rejM = rejM, ec = ErrorCount)
  eval(parse(text = paste0("save(pRes, ", 
                           "file = 'BAGofT", set, "rej.rda')")) )
}

