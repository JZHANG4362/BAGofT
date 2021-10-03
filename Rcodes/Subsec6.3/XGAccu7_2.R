setwd("/home/zhan4362/Project/HL/HLsimulation92")
#setwd("/Users/mikezhang/Documents/学习18fall/RA/HL/Rcodes152_covid")

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
source(file = "testXGboostAVG.R")
library(pROC)

# set validation set size
nesize <- nrow(datset) * 50/100
ne <- nesize
set.seed(22)
# set parameters for the method to assess 
# nrep: number of averaged xgboosts with the same parametrization but different random seeds
# other parameters are from the function xgboost
testModel = testXGboostAVG(formula = y~., params = list(colsample_bytree =  0.1,
                                                     subsample = 0.6, 
                                                     eta = 0.04, 
                                                     max_depth =7), 
                                          nrounds = 500,
                                          nRep = 20)

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
  
  # print accuracy
  print(mean(datE$y == (testMod$predE > 0.5)))
  # store accuracy
  accuVec[i] <- mean(datE$y == (testMod$predE > 0.5))
  AUCVec[i] <- roc(datE$y, testMod$predE, quiet = TRUE)$auc[1]
}

# mean accuracy
accuracy <- mean(accuVec)
# params = list(colsample_bytree =  0.5,
#               subsample = 1, 
#               eta = 0.001, 
#               max_depth =7), 
# early_stopping_rounds = 100, nrounds = 500,
# objective = "binary:logistic",
# validationRatio = 0.2
# seed 23
# 0.64
# seed 22
#0.6266667

# standard error of the mean accuracy
accuSE <- sd(accuVec)/sqrt(nesize)
# mean AUC
AUC <- mean(AUCVec)
# standard error of the mean AUC
AUCSE <- sd(AUCVec)/sqrt(nesize)

# store the results
results <- list(accuVec = accuVec,
                AUCVec = AUCVec,
                accuracy = accuracy,
                accuSE= accuSE,
                AUC = AUC,
                AUCSE = AUCSE)
save(results, 
      file = "XGAccu7_2.rda")


