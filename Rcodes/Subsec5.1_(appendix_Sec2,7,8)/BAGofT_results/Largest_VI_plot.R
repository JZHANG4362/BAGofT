
source("VarImp.R")

load(file = "sim2_100_1_fl.rda")
 maxRes <- sapply(c(1:length(result)), function(x) which.max(VarImp(result[[x]])$Var.imp) )
 plotDat <- data.frame(covariates = maxRes)
 g <- ggplot(plotDat, aes(covariates ))
 # Number of cars in each class:
 g + geom_bar()       

#================setting 1=================
gglist1 <- list()
rdaList <- c("sim1_100_1_fl.rda",
             "sim1_200_1_fl.rda",
             "sim1_800_1_fl.rda",
             "sim4_100_1_fl.rda",
             "sim4_200_1_fl.rda",
             "sim4_800_1_fl.rda")
sttList <- rep(1,6)
gammaList <- c(rep(1, 3), rep(0.5, 3))
nsizeList <- c(rep(c(100,200,800), 2))

for (j in seq_along(rdaList)){
  message(j)
  load(file = rdaList[j])
  
  stt <- sttList[j]
  gamma <- gammaList[j]
  nsize <- nsizeList[j]
  
  library(ggplot2)
 
  maxRes <- sapply(c(1:length(result)), function(x) which.max(VarImp(result[[x]])$Var.imp) )
  maxResf <- as.factor(maxRes)
  levels(maxResf) <- c("x1", "x2", "x3")
  plotDat <- data.frame(covariates = maxResf)
  p <- ggplot(plotDat, aes(covariates )) + geom_bar() +
    ylim(0,100) +
    labs(title=bquote("Setting"~.(stt)~ gamma~"="~.
                      (gamma)), subtitle=
           bquote(paste("n="~.(nsize))), x = "Covariates", y = "Counts" )
  gglist1[[j]] <- p
}

library(gridExtra)
grid.arrange(gglist1[[1]], gglist1[[2]], gglist1[[3]], 
             gglist1[[4]], gglist1[[5]], gglist1[[6]],
             ncol=3, nrow = 2)


#================setting 4=================
gglist2 <- list()
rdaList <- c("sim3_100_1_fl.rda",
             "sim3_200_1_fl.rda",
             "sim3_800_1_fl.rda",
             "sim6_100_1_fl.rda",
             "sim6_200_1_fl.rda",
             "sim6_800_1_fl.rda")
sttList <- rep(3,6)
gammaList <- c(rep(1, 3), rep(0.5, 3))
nsizeList <- c(rep(c(100,200,800), 2))

for (j in seq_along(rdaList)){
  message(j)
  load(file = rdaList[j])
  
  stt <- sttList[j]
  gamma <- gammaList[j]
  nsize <- nsizeList[j]
  
  library(ggplot2)
  
  maxRes <- sapply(c(1:length(result)), function(x) which.max(VarImp(result[[x]])$Var.imp) )
  maxResf <- as.factor(maxRes)
  levels(maxResf) <- c("x1", "x2", "x3")
  plotDat <- data.frame(covariates = maxResf)
  p <- ggplot(plotDat, aes(covariates )) + geom_bar() +
    ylim(0,100) +
    labs(title=bquote("Setting"~.(stt)~ gamma~"="~.
                      (gamma)), subtitle=
           bquote(paste("n="~.(nsize))), x = "Covariates", y = "Counts" )
  gglist2[[j]] <- p
}



library(gridExtra)
grid.arrange(gglist2[[1]], gglist2[[2]], gglist2[[3]], 
             gglist2[[4]], gglist2[[5]], gglist2[[6]],
             ncol=3, nrow = 2)
