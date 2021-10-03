
##########################################################
# lasso logistic
# read Glmnet1_1.rda - Glmnet2_3.rda to  pVac1_1 - pVac2_3
##########################################################

sig <- 0.05
nrep <- 100
plotDat1 <- data.frame(pvalues = c(pVac1_1, pVac1_2, pVac1_3), trainsize = factor(rep(c(1:3), each =100)) )
levels(plotDat1$trainsize) <- c("90%", "75%", "50%")
library(ggplot2)

pbox1 <- ggplot(plotDat1, aes(x=trainsize, y=pvalues) ) + 
  geom_boxplot() + 
  geom_hline(yintercept=sig, linetype="dashed", color = "red") +
  scale_y_continuous(breaks=c(seq(0,1,0.25)), limits = c(0, 1)) +
  labs(title = "Lasso logistic regression", subtitle="Setting 1") +
  xlab("Training set percentage") + 
  ylab("P-values")
plot(pbox1)

plotDat2 <- data.frame(pvalues = c(pVac2_1, pVac2_2, pVac2_3), trainsize = factor(rep(c(1:3), each = 100)) )
levels(plotDat2$trainsize) <- c("90%", "75%", "50%")
library(ggplot2)

pbox2 <- ggplot(plotDat2, aes(x=trainsize, y=pvalues) ) + 
  geom_boxplot() + 
  geom_hline(yintercept=sig, linetype="dashed", color = "red") +
  scale_y_continuous(breaks=c(seq(0,1,0.25)), limits = c(0, 1)) +
  labs(title = "", subtitle="Setting 2") +
  xlab("Training set percentage") + 
  ylab("P-values")
plot(pbox2)
library(gridExtra)
pdf(file = "SummaryPlot3.pdf", width=7, height=3.5)
grid.arrange(pbox1, pbox2,
             nrow = 1, ncol = 2)
dev.off()

##########################################################
# random forest 
# read RF1_1.rda - RF2_3.rda to  pRFVac1_1 - pRFVac2_3
##########################################################


sig <- 0.05

plotRFDat1 <- data.frame(pvalues = c(pRFVac1_1, pRFVac1_2, pRFVac1_3), trainsize = factor(rep(c(1:3), each =7*13)) )
levels(plotRFDat1$trainsize) <- c("90%", "75%", "50%")
library(ggplot2)

pboxRF1 <- ggplot(plotRFDat1, aes(x=trainsize, y=pvalues) ) + 
  geom_boxplot() + 
  geom_hline(yintercept=sig, linetype="dashed", color = "red") +
  scale_y_continuous(breaks=c(seq(0,1,0.25)), limits = c(0, 1) ) +
  labs(title = "Random forest", subtitle="Setting 1") +
  xlab("Training set percentage") + 
  ylab("P-values")
plot(pboxRF1)

plotRFDat2 <- data.frame(pvalues = c(pRFVac2_1, pRFVac2_2, pRFVac2_3), trainsize = factor(rep(c(1:3), each = 7*13)) )
levels(plotRFDat2$trainsize) <- c("90%", "75%", "50%")
library(ggplot2)

pboxRF2 <- ggplot(plotRFDat2, aes(x=trainsize, y=pvalues) ) + 
  geom_boxplot() + 
  geom_hline(yintercept=sig, linetype="dashed", color = "red") +
  scale_y_continuous(breaks=c(seq(0,1,0.25)), limits = c(0, 1)  ) +
  labs(title = "", subtitle="Setting 2") +
  xlab("Training set percentage") + 
  ylab("P-values")
plot(pboxRF2)
library(gridExtra)
pdf(file = "SummaryPlot3RF.pdf", width=7, height=3.5)
grid.arrange(pboxRF1, pboxRF2,
             nrow = 1, ncol = 2)
dev.off()
##########################################################
# xgboost
# read XG1_1.rda - XG2_3.rda to  pXGVac1_1 - pXGVac2_3
##########################################################


sig <- 0.05

plotXGDat1 <- data.frame(pvalues = c(pXGVac1_1, pXGVac1_2, pXGVac1_3), trainsize = factor(rep(c(1:3), each =100)) )
levels(plotXGDat1$trainsize) <- c("90%", "75%", "50%")
library(ggplot2)

pboxXG1 <- ggplot(plotXGDat1, aes(x=trainsize, y=pvalues) ) + 
  geom_boxplot() + 
  geom_hline(yintercept=sig, linetype="dashed", color = "red") +
  scale_y_continuous(breaks=c(seq(0,1,0.25)), limits = c(0, 1)) +
  labs(title = "XGboost", subtitle="Setting 1") +
  xlab("Training set percentage") + 
  ylab("P-values")
plot(pboxXG1)

plotXGDat2 <- data.frame(pvalues = c(pXGVac2_1, pXGVac2_2, pXGVac2_3), trainsize = factor(rep(c(1:3), each = 100)) )
levels(plotXGDat2$trainsize) <- c("90%", "75%", "50%")
library(ggplot2)

pboxXG2 <- ggplot(plotXGDat2, aes(x=trainsize, y=pvalues) ) + 
  geom_boxplot() + 
  geom_hline(yintercept=sig, linetype="dashed", color = "red") +
  scale_y_continuous(breaks=c(seq(0,1,0.25)), limits = c(0, 1)) +
  labs(title = "", subtitle="Setting 2") +
  xlab("Training set percentage") + 
  ylab("P-values")
plot(pboxXG2)
library(gridExtra)
pdf(file = "SummaryPlot3XG.pdf", width=7, height=3.5)
grid.arrange(pboxXG1, pboxXG2,
             nrow = 1, ncol = 2)
dev.off()



# library(gridExtra)
# pdf(file = "SummaryPlot4.pdf", width=7, height=8.5)
# grid.arrange(pbox1, pbox2,
#              pboxRF1, pboxRF2,
#              pboxXG1, pboxXG2,
#              nrow = 3, ncol = 2)
# dev.off()

library(gridExtra)
pdf(file = "SummaryPlot4.pdf", width=10, height=4)
grid.arrange(pbox1, pboxRF1, pboxXG1,
             pbox2, pboxRF2, pboxXG2,
             nrow = 2, ncol = 3)
dev.off()

