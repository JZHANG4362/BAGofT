testXGboostAVG <- function(formula, params = list(), 
                           early_stopping_rounds = NULL, 
                           objective = "reg:logistic",
                           verbose = 0,  
                           nrounds = 25, 
                           nRep = 20){
  # return a function of train data and test data
  return(function(Train.data, Test.data){
    # obtain the response name
    Rsp <- as.character(formula)[2]
    # regressor data 
    XmatT <- model.matrix(formula,  Train.data)[,-1] 
    XmatE <- model.matrix(formula,  Test.data)[,-1] 
    
    predTMat <- matrix(nrow = nrow(XmatT), ncol = nRep)
    predEMat <- matrix(nrow = nrow(XmatE), ncol = nRep)
    
    for (b in c(1:nRep)){
      # fit xgboost
      xgModT <- xgboost :: xgboost(data = XmatT, label = Train.data[, Rsp], params = params, nrounds = nrounds, verbose = verbose, objective = objective, early_stopping_rounds = early_stopping_rounds)
      #predict on the test set
      predEMat[,b] <- stats :: predict(xgModT, XmatE)
      
      #predict on the training set
      predTMat[,b] <- stats :: predict(xgModT, XmatT)
      
    }
    predE <- rowMeans(predEMat)
    predT <- rowMeans(predTMat)
    
    # calculate the Pearson residual
    res <-   (Train.data[, Rsp] -  predT)/sqrt(predT * (1 - predT ))
    
    return(list(predT = predT, predE = predE, res = res, Rsp = Rsp))
  })
}
