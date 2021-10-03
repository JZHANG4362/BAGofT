testXGboost <- function(formula, params = list(), early_stopping_rounds = NULL, 
                        objective = "reg:logistic",verbose = 0,  nrounds = 25){
  # return a function of train data and test data
  return(function(Train.data, Test.data){
    # obtain the response name
    Rsp <- as.character(formula)[2]
    # regressor data 
    XmatT <- model.matrix(formula,  Train.data)[,-1] 
    XmatE <- model.matrix(formula,  Test.data)[,-1] 
    Train.data <- Train.data
    # response data
    yT <- Train.data[, Rsp]
    # training set
    df.train <- XmatT
    train.y <- yT
    dtrain <- xgboost :: xgb.DMatrix(data=df.train, label=train.y)

    # create a watchlist to track training and validation performance
    watchlist <- list(train=dtrain)
    # fit xgboost
    xgModT <- xgboost :: xgb.train(params = params,
                                   data = dtrain, 
                                 nrounds = nrounds, 
                                 objective = objective, 
                                 early_stopping_rounds = early_stopping_rounds,
                                 watchlist = watchlist)
    
    #predict on the test set
    predE <- stats :: predict(xgModT, XmatE)
    
    #predict on the training set
    predT <- stats :: predict(xgModT, XmatT)
    
    # calculate the Pearson residual
    res <-   (Train.data[, Rsp] -  predT)/sqrt(predT * (1 - predT ))
    
    return(list(predT = predT, predE = predE, res = res, Rsp = Rsp))
  })
}
