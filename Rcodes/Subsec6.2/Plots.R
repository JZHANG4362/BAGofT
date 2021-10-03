

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

nameList <-c("T-shirt/top", "Trouser", "Pullover", "Dress", "Coat", "Sandal", "Shirt", "Sneaker", " Bag", "Ankle boot")
plotFun <- function(gp){
  matTmp <- TrainX[ which(train_labels == gp), ]
  # number of plots
  np <- 9
  par(mfrow = c(3,3))
  par(mar=c(1.5,1,1,1))
  for (k in c(1:np)){
    #Creating a empty matrix to use
    datMat1 <- matrix(nrow = 28, ncol = 28)
    j <- 1
    for(i in 28:1){
      
      datMat1[,i] <- as.numeric(matTmp[k, ])[j:(j+27)]
      
      j <- j+28
      
    }
    image(1:28, 1:28, datMat1, col=gray((0:255)/255),
          xaxt='n', xlab = "", ylab = "")
  }
  title(sub=paste("Group ", gp, " ", nameList[(gp + 1)]), line = -1, outer = TRUE)  
  par(mfrow = c(1,1))
  par(mar=c(5.1, 4.1, 4.1, 2.1))
}

plotFun(9)



# number of plots
np <- 9
par(mfrow = c(3,3))
par(mar=c(2,1,1,1))
for (k in c(1:np)){
  #Creating a empty matrix to use
  datMat1 <- matrix(nrow = 28, ncol = 28)
  j <- 1
  for(i in 28:1){
    
    datMat1[,i] <- as.numeric(TrainX17[k, ])[j:(j+27)]
    
    j <- j+28
    
  }
  image(1:28, 1:28, datMat1, col=gray((0:255)/255),
        xaxt='n', xlab = "", ylab = "")
}
par(mfrow = c(1,1))
par(mar=c(5.1, 4.1, 4.1, 2.1))

# number of plots
np <- 9
par(mfrow = c(3,3))
par(mar=c(2,1,1,1))
for (k in c(1:np)){
  #Creating a empty matrix to use
  datMat2 <- matrix(nrow = 28, ncol = 28)
  j <- 1
  for(i in 28:1){
    
    datMat2[,i] <- as.numeric(TrainX17[(500 + k), ])[j:(j+27)]
    
    j <- j+28
    
  }
  image(1:28, 1:28, datMat2, col=gray((0:255)/255),
        xaxt='n', xlab = "", ylab = "")
}
par(mfrow = c(1,1))
par(mar=c(5.1, 4.1, 4.1, 2.1))


#load(file = "fashion1_1.rda")
load(file = "fashion1_0.rda")
source(file = "VarImp.R")

# variable importance after preselection
vimp1 <- VarImp(testRes1)$Var.imp
# calcualte the variable importance
mat_temp <-  VarImp(testRes1)$preVar.imp
mat_temp[c(1:nrow(mat_temp)),] <- c(0,0)
for (r in c(1 : nrow(vimp1))){
  mat_temp[which(rownames(mat_temp) == rownames(vimp1)[r]), ] <- vimp1[r]
}
impMat <- matrix(nrow = 28, ncol = 28)
j <- 1
for(i in 28:1){
  
  impMat[,i] <-mat_temp[,1][j:(j+27)]
  
  j <- j+28
  
}
image(1:28, 1:28, impMat, col=gray((0:255)/255),
      xaxt='n', axes = FALSE, xlab="", ylab="")


# # variable importance before preselection
# vimp2 <- VarImp(testRes1)$preVar.imp
# impMat <- matrix(nrow = 28, ncol = 28)
# j <- 1
# for(i in 28:1){
#   
#   impMat[,i] <- vimp2[,1][j:(j+27)]
#   
#   j <- j+28
#   
# }
# image(1:28, 1:28, impMat, col=gray((0:255)/255),
#       xaxt='n', xlab = "", ylab = "")

