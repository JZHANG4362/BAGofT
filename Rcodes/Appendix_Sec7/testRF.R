testRF <- function(formula,
                   ntree = 500,
                   mtry = NULL,
                   maxnodes = NULL){
  # return a function of train data and test data
  return(function(Train.data, Test.data){
    if (is.null(mtry)){
      mtry <- max(floor( length( dim(model.matrix(formula ,Train.data))[2]-1)/3),1)
    }
    
    # obtain the response name
    Rsp <- as.character(formula)[2]
    
    RspDat <- Train.data[,Rsp]
    
    Train.data[,Rsp] <- as.factor(Train.data[,Rsp])
    Test.data[,Rsp] <- as.factor(Test.data[,Rsp])
    
    resRf <- randomForest :: randomForest(formula, data = Train.data, ntree = ntree,  maxnodes = maxnodes, mtry = mtry, importance=FALSE)
    # obtain random forest prediction on the training set
    predT <-  stats :: predict(resRf, newdata = Train.data, type =  "prob")[,2]
    # obtain random forest prediction on the test set
    predE <-  stats :: predict(resRf, newdata = Test.data, type =  "prob")[,2]
    
    # calculate the Pearson residual
    # res <-   (RspDat -  predT)/sqrt(predT * (1 - predT ))
    res <-   (RspDat -  predT)
    
    
    return(list(predT = predT, predE = predE, res = res, Rsp = Rsp))
  })
}