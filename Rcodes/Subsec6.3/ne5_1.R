###################################################################
# dataset preparation
###################################################################

# read predictors data
trainX <- read.csv(file = 'trainX.csv', header = FALSE)
testX <- read.csv(file = 'testX.csv', header = FALSE)

# assign predictor names
trainDat <- as.data.frame(as.matrix(trainX))
names(trainDat) <- paste0("x", c(1:1000))
testDat <- as.data.frame(as.matrix(testX))
names(testDat) <- paste0("x", c(1:1000))

# read response data
trainY <- as.numeric(read.csv(file = 'trainY.csv', header = FALSE)$V1)
testY <- as.numeric(read.csv(file = 'testY.csv', header = FALSE)$V1)

# add response data to the dataset
trainDat$y <- trainY
testDat$y <- testY

# change row names before appending the datasets
row.names(testDat) <- c(426:628)

# append the datasets
datset <- rbind(trainDat, testDat)

# load packages for the method to assess and BAGofT
library(keras)
source(file = "parRF.R")
source(file = "testNeuAVG.R")
source(file = "BAGofT.R")
source(file = "dcPre.R")

# set validation set size
nesize <- nrow(datset) * 25/100

# apply the BAGofT
system.time(testRes1 <- BAGofT(testModel = testNeuAVG(formula = y~., units = 1, epochs = 10, 
                                                                       batch_size = 6, nRep = 20), 

       parFun = parRF(preFun = dcPre(npreSel = 5)),
       data =  datset , 
       ne = nesize,
       nsplits = 20,
       nsim = 60))

# save the result
save( testRes1, 
      file = "ne5_1.rda")


