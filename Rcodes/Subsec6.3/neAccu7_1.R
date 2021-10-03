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


###################################################################
# Fit the model to assess
###################################################################
library(keras)
library(pROC)
source(file = "parRF.R")
source(file = "testNeuAVG.R")
source(file = "BAGofT.R")
source(file = "dcPre.R")

# set validation set size
nesize <- nrow(datset) * 25/100
ne <- nesize

# set parameters for the method to assess 
testModel = testNeuAVG(formula = y~., units = 7, epochs = 10, 
                       batch_size = 6, nRep = 20)

# number of rows in the dataset
nr <- nrow(datset)
# obtain the training set size
nt <- nr - ne

# set the number of replications
nrep <- 100
# set vectors to store accuracy and AUC
accuVec <- numeric(nrep)
AUCVec <- numeric(nrep)

# compute the results from multiple replications
for (i in c(1:nrep)){
  message(paste("rep: ", i))
  # randomly generate the indices for training set observations
  trainIn <- sample(c(1 : nr), nt)
  
  #split the data
  datT <- datset[trainIn, ]
  datE <- datset[-trainIn, ]
  # fit the model to test by training data
  testMod <- testModel(Train.data = datT, Test.data = datE)
  
  # store accuracy
  accuVec[i] <- mean(datE$y == (testMod$predE > 0.5))
  AUCVec[i] <- roc(datE$y, testMod$predE, quiet = TRUE)$auc[1]
}

# mean accuracy
accuracy <- mean(accuVec)
# standard error of the mean accuracy
accuSE <- sd(accuVec)/sqrt(nrep)
# mean AUC
AUC <- mean(AUCVec)
# standard error of the mean AUC
AUCSE <- sd(AUCVec)/sqrt(nrep)

# store the results
results <- list(accuVec = accuVec,
                AUCVec = AUCVec,
                accuracy = accuracy,
                accuSE= accuSE,
                AUC = AUC,
                AUCSE = AUCSE)
save(results, 
      file = "neAccu7_1.rda")


