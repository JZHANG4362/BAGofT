
source(file = "VarImp.R")
#==============Model A=============
gglist <- list()
rdaList <- c("sim1_100_1_fl.rda",
             "sim1_200_1_fl.rda",
             "sim1_800_1_fl.rda",
             "sim2_100_1_fl.rda",
             "sim2_200_1_fl.rda",
             "sim2_800_1_fl.rda",
             "sim3_100_1_fl.rda",
             "sim3_200_1_fl.rda",
             "sim3_800_1_fl.rda",
             "sim4_100_1_fl.rda",
             "sim4_200_1_fl.rda",
             "sim4_800_1_fl.rda",
             "sim5_100_1_fl.rda",
             "sim5_200_1_fl.rda",
             "sim5_800_1_fl.rda",
             "sim6_100_1_fl.rda",
             "sim6_200_1_fl.rda",
             "sim6_800_1_fl.rda")
sttList <- c(rep(c(1:3), each = 3), rep(c(1:3), each = 3))
gammaList <- c(rep(1, 9), rep(0.5, 9))
nsizeList <- c(rep(c(100,200,800), 6))

for (j in seq_along(rdaList)){
  message(j)
  load(file = rdaList[j])
  
  stt <- sttList[j]
  gamma <- gammaList[j]
  nsize <- nsizeList[j]
  
  library(ggplot2)
  p <- ggplot() +
    ylim(0, 1) +
    labs(title=bquote("Setting"~.(stt)~ gamma~"="~.
                      (gamma)), subtitle=
           bquote(paste("n="~.(nsize))), x = "Number of splits", y = "Statistic value" )
  for (i in c(1:100)){
    plist <- unlist(lapply(c(1:40), function(x) result[[i]]$singleSplit.results[[x]]$p.value))
    cumMean <- cumsum(plist) / seq_along(plist) 
    plotDat <- data.frame(cumMean = cumMean, nSplit = c(1:40))
   p  <- p + geom_line(data = plotDat, aes(x = nSplit, y = cumMean)) 
  }
  gglist[[j]] <- p
}

library(gridExtra)
grid.arrange(gglist[[1]], gglist[[2]], gglist[[3]], 
             gglist[[10]], gglist[[11]], gglist[[12]],
             ncol=3, nrow = 2)

grid.arrange(gglist[[4]], gglist[[5]], gglist[[6]], 
             gglist[[13]], gglist[[14]], gglist[[15]],
             ncol=3, nrow = 2)

grid.arrange(gglist[[7]], gglist[[8]], gglist[[9]], 
             gglist[[16]], gglist[[17]], gglist[[18]],
             ncol=3, nrow = 2)

#==============Model B=============
gglist <- list()
rdaList <- c("sim1_100_2_fl.rda",
             "sim1_200_2_fl.rda",
             "sim1_800_2_fl.rda",
             "sim2_100_2_fl.rda",
             "sim2_200_2_fl.rda",
             "sim2_800_2_fl.rda",
             "sim3_100_2_fl.rda",
             "sim3_200_2_fl.rda",
             "sim3_800_2_fl.rda",
             "sim4_100_2_fl.rda",
             "sim4_200_2_fl.rda",
             "sim4_800_2_fl.rda",
             "sim5_100_2_fl.rda",
             "sim5_200_2_fl.rda",
             "sim5_800_2_fl.rda",
             "sim6_100_2_fl.rda",
             "sim6_200_2_fl.rda",
             "sim6_800_2_fl.rda")
sttList <- c(rep(c(1:3), each = 3), rep(c(1:3), each = 3))
gammaList <- c(rep(1, 9), rep(0.5, 9))
nsizeList <- c(rep(c(100,200,800), 6))

for (j in seq_along(rdaList)){
  message(j)
  load(file = rdaList[j])
  
  stt <- sttList[j]
  gamma <- gammaList[j]
  nsize <- nsizeList[j]
  
  library(ggplot2)
  p <- ggplot() +
    ylim(0, 1) +
    labs(title=bquote("Setting"~.(stt)~ gamma~"="~.
                      (gamma)), subtitle=
           bquote(paste("n="~.(nsize))), x = "Number of splits", y = "Statistic value" )
  for (i in c(1:100)){
    plist <- unlist(lapply(c(1:40), function(x) result[[i]]$singleSplit.results[[x]]$p.value))
    cumMean <- cumsum(plist) / seq_along(plist) 
    plotDat <- data.frame(cumMean = cumMean, nSplit = c(1:40))
    p  <- p + geom_line(data = plotDat, aes(x = nSplit, y = cumMean)) 
  }
  gglist[[j]] <- p
}

library(gridExtra)
grid.arrange(gglist[[1]], gglist[[2]], gglist[[3]], 
             gglist[[10]], gglist[[11]], gglist[[12]],
             ncol=3, nrow = 2)

grid.arrange(gglist[[4]], gglist[[5]], gglist[[6]], 
             gglist[[13]], gglist[[14]], gglist[[15]],
             ncol=3, nrow = 2)

grid.arrange(gglist[[7]], gglist[[8]], gglist[[9]], 
             gglist[[16]], gglist[[17]], gglist[[18]],
             ncol=3, nrow = 2)
