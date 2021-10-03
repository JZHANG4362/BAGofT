
alpha <- 0.05
######################################################
# rejection count under the alternative for HL
######################################################

for (set in c(1:6)){
  rejM <- numeric(3)
  ErrorCount <- numeric(3)
  for (i in c(1:3)){
    nsize <-  c(100, 200, 800)[i]
    eval(parse(text = paste0("load(file = 'simHL",set,"_",nsize,"_1_fl.rda')")) )
    pval <- unlist(lapply(c(1:100), function(x)result[[x]]$p.value))
    ErrorCount[i] <- sum(is.na(pval))
    pval2 <- pval[!is.na(pval)]
    rejM[i] <- mean(pval2 < alpha)
  }
  pRes <- list(rejM = rejM, ec = ErrorCount)
  eval(parse(text = paste0("save(pRes, ", 
                           "file = 'HL", set, "rej.rda')")) )
}

######################################################
# rejection count under the alternative for CH
######################################################

for (set in c(1:6)){
  rejM <- numeric(3)
  ErrorCount <- numeric(3)
  for (i in c(1:3)){
    nsize <-  c(100, 200, 800)[i]
    eval(parse(text = paste0("load(file = 'simCH",set,"_",nsize,"_1_fl.rda')")) )
    pval <- unlist(lapply(c(1:100), function(x)result[[x]][5]))
    ErrorCount[i] <- sum(is.na(pval))
    pval2 <- pval[!is.na(pval)]
    rejM[i] <- mean(pval2 < alpha)
  }
  pRes <- list(rejM = rejM, ec = ErrorCount)
  eval(parse(text = paste0("save(pRes, ", 
                           "file = 'CH", set, "rej.rda')")) )
}



######################################################
# rejection count under the alternative for GRP
######################################################

#=================error list for GRP==================

ErrorList_GRP <- list()
list_temp <- list()
# list_temp[[1]] <- c(5, 8, 14, 20, 22, 23, 24, 27, 31, 36, 50, 52, 61, 74, 80, 83, 95, 96)
# list_temp[[2]] <- c(22, 52, 80, 81, 83)
ErrorList_GRP[[1]] <- list_temp
list_temp <- list()
list_temp[[1]] <- c(3, 29)
ErrorList_GRP[[2]] <- list_temp
list_temp <- list()
list_temp[[1]] <- c(5, 7, 19, 20, 22, 31, 33, 36, 48, 50, 56, 58, 71, 79, 80, 81, 93, 96)
list_temp[[2]] <- c(5, 22, 36, 68, 81, 96)
list_temp[[3]] <- c(31, 74)
ErrorList_GRP[[3]] <- list_temp
list_temp <- list()
#list_temp[[1]] <- c(22, 81)
ErrorList_GRP[[4]] <- list_temp
list_temp <- list()
list_temp[[1]] <- 29
ErrorList_GRP[[5]] <- list_temp
list_temp <- list()
list_temp[[1]] <- c(5, 7, 20, 22, 31, 36, 37, 48, 50, 58, 80, 81, 82, 91, 96)
list_temp[[2]] <- c(5, 22, 36, 80, 90, 96)
list_temp[[3]] <- c(5, 22)
ErrorList_GRP[[6]] <- list_temp

readError_GRP<- list()
list_temp2 <- list()
# list_temp2[[1]] <- TRUE
# list_temp2[[2]] <- TRUE
list_temp2[[1]] <- FALSE
list_temp2[[2]] <- FALSE
list_temp2[[3]] <- FALSE
readError_GRP[[1]] <- list_temp2
list_temp2 <- list()
list_temp2[[1]] <- TRUE
list_temp2[[2]] <- FALSE
list_temp2[[3]] <- FALSE
readError_GRP[[2]] <- list_temp2
list_temp2 <- list()
list_temp2[[1]] <- TRUE
list_temp2[[2]] <- TRUE
list_temp2[[3]] <- TRUE
readError_GRP[[3]] <- list_temp2
list_temp2 <- list()
# list_temp2[[1]] <- TRUE
list_temp2[[1]] <- FALSE
list_temp2[[2]] <- FALSE
list_temp2[[3]] <- FALSE
readError_GRP[[4]] <- list_temp2
list_temp2 <- list()
list_temp2[[1]] <- TRUE
list_temp2[[2]] <- FALSE
list_temp2[[3]] <- FALSE
readError_GRP[[5]] <- list_temp2
list_temp2 <- list()
list_temp2[[1]] <- TRUE
list_temp2[[2]] <- TRUE
list_temp2[[3]] <- TRUE
readError_GRP[[6]] <- list_temp2

for (set in c(1:6)){
  rejM <- numeric(3)
  ErrorCount <- numeric(3)
  for (i in c(1:3)){
    nsize <-  c(100, 200, 800)[i]
    eval(parse(text = paste0("load(file = 'simGRP",set,"_",nsize,"_1_fl.rda')")) )
    pval <- unlist(lapply(c(1:100), function(x)result[[x]]))
    if (readError_GRP[[set]][[i]]){
      Errorlog <- ErrorList_GRP[[set]][[i]]
      # remove pvalues with error output in the log
      pval_rm <- pval[-Errorlog]
      # count the total number of pvalues dropped
      ErrorCount[i] <- length(Errorlog) + sum(is.na(pval_rm))
    }else{
      pval_rm <- pval
      # count the total number of pvalues dropped
      ErrorCount[i] <- sum(is.na(pval_rm))
    }
    
    pval2 <- pval_rm[!is.na(pval_rm)]
    rejM[i] <- mean(pval2 < alpha)
  }
  pRes <- list(rejM = rejM, ec = ErrorCount)
  eval(parse(text = paste0("save(pRes, ", 
                           "file = 'GRP", set, "rej.rda')")) )
}




