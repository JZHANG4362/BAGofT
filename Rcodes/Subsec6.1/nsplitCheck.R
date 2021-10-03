nsplitCheck <- function(testModel, parFun, 
                         data,  nsplits = 20, 
                        nrep = 6,
                        ne = floor(5*nrow(data)^(1/2)),
                        plotRes = TRUE){
  checkRes <- matrix(nrow = nrep, ncol = nsplits)
  for (i in c(1:nrep)){
    message(paste("rep: ", i))
    pvres <- unlist(lapply(c(1:nsplits), function(x)BAGofT_multi(testModel = testModel, 
                                                        parFun =  parFun,
                                                        data  = data,  nsplits =  nsplits, ne = floor(5*nrow( dattest)^(1/2)))$spliDat[[x]]$p.value))
    checkRes[i, ] <- cumsum(pvres) / seq_along(pvres)
  }
  
  
  if (plotRes){
    if (nrep >1){
      pvStd <- apply(checkRes, 2, sd)
      plot(pvStd, xlab = "number of splittings", ylab = "standard deviation of the test statistic")
      return(invisible( list(checkRes = checkRes, pvStd = pvStd)))
      }else if(nrep==1){
      plot(as.numeric(checkRes), xlab = "number of splittings", ylab = "value of the test statistic")
        return(invisible( list(checkRes = checkRes)))
    }

    }
  
}