testNeu3 <- function(formula, units1, units2, Train.data, Test.data, epochs, batch_size, verbose = 0){
  # return a function of train data and test data
  return(function(Train.data, Test.data){
    require(dplyr)
    # obtain the response name
    Rsp <- as.character(formula)[2]
    
    # regressor data 
    XmatT <- model.matrix(formula,  Train.data)[,-1] 
    XmatE <- model.matrix(formula,  Test.data)[,-1] 
    
    # fit neural network with 1 hidden layer
    network <- keras :: keras_model_sequential() %>%
      layer_dense(units = units1, activation = "relu", 
                  input_shape = c(ncol(XmatT))) %>%
      layer_dense(units = units2, activation = "relu", 
                  input_shape = units1) %>%
      layer_dense(units = 1, activation = 'sigmoid')
    
    network %>% keras :: compile(
      optimizer = "rmsprop",
      loss = 'binary_crossentropy',
      metrics = c("accuracy")
    )
    
    network %>% keras :: fit(XmatT, Train.data[, Rsp],  epochs = epochs, batch_size = batch_size, verbose = verbose)
    
    #predict on the test set
    predE <- network %>% keras :: predict_proba(XmatE)
    
    #predict on the training set
    predT <- network %>% keras :: predict_proba(XmatT)
    
    # calculate the Pearson residual
    #res <-   (Train.data[, Rsp] -  predT)/sqrt(predT * (1 - predT ))
    res <-   (Train.data[, Rsp] -  predT)
    
    return(list(predT = predT, predE = predE, res = res, Rsp = Rsp))
  })
 
}