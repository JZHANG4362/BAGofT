# preselection by random forest

rfPre <- function( npreSel = 5,
                   pre.mtry = NULL,
                   pre.ntree = 4,
                   pre.maxnodes = 5){

  return(function(datRf, parVar){
    # count the number of variables to partition
    nParV <- if(!identical(parVar, ".")){length(parVar)}else{ncol(datRf) - 1}
    if (nParV > npreSel){
      if (is.null(pre.mtry)){
        pre.mtry <- if(!identical(parVar, ".")){floor(length(parVar)/2 )}else{floor( (ncol(datRf) - 1)/2) }
      }
      preSelected <- TRUE
      # fit a random forest to preselect partition variables
      formula_rf_temp <- stats :: as.formula(paste("res", " ~ ", paste(parVar,  collapse = " + "), sep = ""))
      resRf_temp <- randomForest :: randomForest(formula_rf_temp, data = datRf, ntree = pre.ntree, mtry = pre.mtry,  maxnodes = pre.maxnodes, importance=TRUE)
      
      # select Kmax partition variables with the largest variable importance
      parVarNew <- rownames(resRf_temp$importance)[order(-resRf_temp$importance[,1])[c(1: npreSel)]]
      return(list(preSelected = preSelected, parVarNew = parVarNew, VI = resRf_temp$importance))
      
    }else{
      preSelected = FALSE
      parVarNew <- parVar
      return(list(preSelected = preSelected, parVarNew = parVarNew))
      
    }
    
  })
}