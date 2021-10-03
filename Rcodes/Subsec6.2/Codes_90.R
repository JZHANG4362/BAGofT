

library(keras)
fashion_mnist <- dataset_fashion_mnist()

c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$tes

TrainX = as.matrix(array_reshape(train_images, c(60000, 28*28))/255)
TrainY = as.matrix(train_labels, nrow = length(train_labels), ncol = 1)

ind1 <- which(train_labels == 1)[c(1:500)]
ind3 <- which(train_labels == 3)[c(1:500)]

TrainX17 <- TrainX[c(ind1, ind3), ]
TrainY17 <- as.numeric(TrainY[c(ind1, ind3)] == 1)

mnist17 <- data.frame(TrainX17)
mnist17$y <- TrainY17

source(file = "parRF.R")
source(file = "testNeu.R")
source(file = "BAGofT.R")
source(file = "dcPre.R")

ne <- nrow(mnist17)*10/100

testRes1 <- BAGofT(testModel = testNeu(formula = y~., units = 1, epochs = 10, 
                                                                       batch_size = 5), 

       parFun = parRF(preFun = dcPre(npreSel = 5)),
       data =  mnist17, 
       ne = ne,
       nsplits = 20,
       nsim = 100)

save( testRes1, 
      file = "fashion1_0.rda")
