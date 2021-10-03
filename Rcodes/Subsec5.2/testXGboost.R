testXGboost <- function(formula, params = list(), nrounds = 25){
  # return a function of train data and test data
  return(function(Train.data, Test.data){
    # obtain the response name
    Rsp <- as.character(formula)[2]
    # regressor data 
    XmatT <- model.matrix(formula,  Train.data)[,-1] 
    XmatE <- model.matrix(formula,  Test.data)[,-1] 
    # fit xgboost
    xgModT <- xgboost :: xgboost(data = XmatT, label = Train.data[, Rsp], params = params, nrounds = nrounds, objective = "binary:logistic")
    
    #predict on the test set
    predE <- stats :: predict(xgModT, XmatE)
    
    #predict on the training set
    predT <- stats :: predict(xgModT, XmatT)
    
    # calculate the Pearson residual
    res <-   (Train.data[, Rsp] -  predT)/sqrt(predT * (1 - predT ))
    
    return(list(predT = predT, predE = predE, res = res, Rsp = Rsp))
  })
}
