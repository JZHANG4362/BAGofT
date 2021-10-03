testNeuAVG <- function(formula, units, Train.data, Test.data, epochs, batch_size, verbose = 0, nRep){
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
      # fit neural network with 1 hidden layer
      network <- keras :: keras_model_sequential() %>%
        layer_dense(units = units, activation = "relu", input_shape = c(ncol(XmatT))) %>%
        layer_dense(units = 1, activation = 'sigmoid')
      
      network %>% keras :: compile(
        # optimizer = "rmsprop",
        optimizer = optimizer_sgd( lr=0.001, momentum=0.9),
        loss = 'binary_crossentropy',
        metrics = c("accuracy")
      )
      
      network %>% keras :: fit(XmatT, Train.data[, Rsp],  epochs = epochs, batch_size = batch_size, verbose = verbose)
      
      #predict on the test set
      predEMat[,b] <- network %>% keras :: predict_proba(XmatE)
      
      #predict on the training set
      predTMat[,b] <- network %>% keras :: predict_proba(XmatT)
    }
    predE <- rowMeans(predEMat)
    predT <- rowMeans(predTMat)
    
    # calculate the Pearson residual
    #res <-   (Train.data[, Rsp] -  predT)/sqrt(predT * (1 - predT ))
    res <-   (Train.data[, Rsp] -  predT)
    
    return(list(predT = predT, predE = predE, res = res, Rsp = Rsp))
  })
 
}