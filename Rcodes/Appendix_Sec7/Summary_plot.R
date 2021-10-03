
sig <- 0.05

#################################################################
# Neural network
#################################################################

# 90%
pNeuVac1 <- c()
RDANeulist1 <- paste("Neu1_", c(1:20), ".rda", sep = "")
for(i in c(1:20)){
  load(file = RDANeulist1[i])
  # append new values to the pvalue vector.
  startID <- (i-1)*5 + 1
  endID <- i*5
  pNeuVac1 <- c(pNeuVac1, sapply(c(startID:endID), function(x)result1[[x]]$p.value))
}

# 75%
pNeuVac2 <- c()
RDANeulist2 <- paste("Neu2_", c(1:20), ".rda", sep = "")
for(i in c(1:20)){
  load(file = RDANeulist2[i])
  # append new values to the pvalue vector.
  startID <- (i-1)*5 + 1
  endID <- i*5
  pNeuVac2 <- c(pNeuVac2, sapply(c(startID:endID), function(x)result1[[x]]$p.value))
}

# 50%
pNeuVac3 <- c()
RDANeulist3 <- paste("Neu3_", c(1:20), ".rda", sep = "")
for(i in c(1:20)){
  load(file = RDANeulist3[i])
  # append new values to the pvalue vector.
  startID <- (i-1)*5 + 1
  endID <- i*5
  pNeuVac3 <- c(pNeuVac3, sapply(c(startID:endID), function(x)result1[[x]]$p.value))
}

ntrainDat <- as.factor(c(1:3))
levels(ntrainDat) <- c("90%", "75%", "50%")
nrep <- 100
library(ggplot2)            
pNeu1 <- ggplot() 
for (i in c(1:nrep)){
  plotDat_temp <- data.frame(pvalues = c(pNeuVac1[i], pNeuVac2[i], pNeuVac3[i]), ntrain = ntrainDat)
  pNeu1  <- pNeu1 + geom_line(data = plotDat_temp, aes(x = ntrain, y = pvalues, group = 1)) 
  
}
pNeu1 <- pNeu1 +labs(title="Neural Network Line Chart",
                     x ="Training set percentage", y = "P-values") + 
  geom_hline(yintercept=sig, linetype="dashed", color = "red") +
  scale_y_continuous(breaks=c(sig, seq(0,1,0.25))  )
plot(pNeu1)


plotDat <- data.frame(pvalues = c(pNeuVac1, pNeuVac2, pNeuVac3), trainsize = factor(rep(c(1:3), each = length(result1))) )
levels(plotDat$trainsize) <- c("90%", "75%", "50%")
library(ggplot2)

pNeu2 <- ggplot(plotDat, aes(x=trainsize, y=pvalues) ) + 
  geom_boxplot() + 
  geom_hline(yintercept=sig, linetype="dashed", color = "red") +
  scale_y_continuous(breaks=c(sig, seq(0,1,0.25)), limits = c(0, 1)  ) +
  labs(title="Neural Network Box Plots") +
  xlab("Training set percentage") + 
  ylab("P-values")
plot(pNeu2)


#################################################################
# Random Forest
#################################################################
# 90%
load(file = "RF1.rda")
pRFVac1 <- sapply(c(1:100), function(x)result1[[x]]$p.value)

# 50%
load(file = "RF2.rda")
pRFVac2 <- sapply(c(1:100), function(x)result1[[x]]$p.value)

# 25%
load(file = "RF3.rda")
pRFVac3 <- sapply(c(1:100), function(x)result1[[x]]$p.value)


ntrainDat <- as.factor(c(1:3))
levels(ntrainDat) <- c("90%", "75%", "50%")
nrep <- 100
library(ggplot2)            
pRF1 <- ggplot() 
for (i in c(1:nrep)){
  plotDat_temp <- data.frame(pvalues = c(pRFVac1[i], pRFVac2[i], pRFVac3[i]), ntrain = ntrainDat)
  pRF1  <- pRF1 + geom_line(data = plotDat_temp, aes(x = ntrain, y = pvalues, group = 1)) 
  
}
pRF1 <- pRF1 +labs(title="Random Forest Line Chart",
                     x ="Training set percentage", y = "P-values") + 
  geom_hline(yintercept=sig, linetype="dashed", color = "red") +
  scale_y_continuous(breaks=c(sig, seq(0,1,0.25))  )
plot(pRF1)


plotDat <- data.frame(pvalues = c(pRFVac1, pRFVac2, pRFVac3), trainsize = factor(rep(c(1:3), each = length(result1))) )
levels(plotDat$trainsize) <- c("90%", "75%", "50%")
library(ggplot2)

pRF2 <- ggplot(plotDat, aes(x=trainsize, y=pvalues) ) + 
  geom_boxplot() + 
  geom_hline(yintercept=sig, linetype="dashed", color = "red") +
  scale_y_continuous(breaks=c(sig, seq(0,1,0.25)), limits = c(0, 1)  ) +
  labs(title="Random Forest Box Plots") +
  xlab("Training set percentage") + 
  ylab("P-values")
plot(pRF2)

#################################################################
# Logistic regression
#################################################################
# 90%
load(file = "GlmBi1.rda")
pGlmBiVac1 <- sapply(c(1:100), function(x)result1[[x]]$p.value)

# 50%
load(file = "GlmBi2.rda")
pGlmBiVac2 <- sapply(c(1:100), function(x)result1[[x]]$p.value)

# 25%
load(file = "GlmBi3.rda")
pGlmBiVac3 <- sapply(c(1:100), function(x)result1[[x]]$p.value)

ntrainDat <- as.factor(c(1:3))
levels(ntrainDat) <- c("90%", "75%", "50%")
nrep <- 100
library(ggplot2)            
pGlmBi1 <- ggplot() 
for (i in c(1:nrep)){
  plotDat_temp <- data.frame(pvalues = c(pGlmBiVac1[i], pGlmBiVac2[i], pGlmBiVac3[i]), ntrain = ntrainDat)
  pGlmBi1  <- pGlmBi1 + geom_line(data = plotDat_temp, aes(x = ntrain, y = pvalues, group = 1)) 
  
}
pGlmBi1 <- pGlmBi1 +labs(title="Logistic Regression Line Chart",
                         x ="Training set percentage", y = "P-values") + 
  geom_hline(yintercept=sig, linetype="dashed", color = "red") +
  scale_y_continuous(breaks=c(sig, seq(0,1,0.25))  )+
  ylim(0,1)
plot(pGlmBi1)


plotDat <- data.frame(pvalues = c(pGlmBiVac1, pGlmBiVac2, pGlmBiVac3), trainsize = factor(rep(c(1:3), each = length(result1))) )
levels(plotDat$trainsize) <- c("90%", "75%", "50%")
library(ggplot2)

pGlmBi2 <- ggplot(plotDat, aes(x=trainsize, y=pvalues) ) + 
  geom_boxplot() + 
  geom_hline(yintercept=sig, linetype="dashed", color = "red") +
  scale_y_continuous(breaks=c(sig, seq(0,1,0.25)), limits = c(0, 1)  ) +
  labs(title="Logistic Regression Box Plots") +
  xlab("Training set percentage") + 
  ylab("P-values") 
plot(pGlmBi2)
#################################################################

# line charts and box plots
library(gridExtra)
pdf(file = "SummaryPlot.pdf", width=7, height=8)
grid.arrange(pNeu1,pNeu2,
             pRF1, pRF2,
             pGlmBi1, pGlmBi2,
             nrow = 3, ncol = 2)
dev.off()

# box plots only
pdf(file = "SummaryPlot2.pdf", width=9, height=3)
grid.arrange(pNeu2, pRF2,pGlmBi2,
             nrow = 1, ncol = 3)
dev.off()


print(c(
sum(pNeuVac1 > sig & pNeuVac2 > sig & pNeuVac3 > sig),
sum(pNeuVac1 > sig & pNeuVac2 > sig & pNeuVac3 <= sig),
sum(pNeuVac1 > sig & pNeuVac2 <= sig & pNeuVac3 <= sig),
sum(pNeuVac1 <= sig & pNeuVac2 <= sig & pNeuVac3 <= sig) ) )



print(c(
sum(pRFVac1 > sig & pRFVac2 > sig & pRFVac3 > sig),
sum(pRFVac1 > sig & pRFVac2 > sig & pRFVac3 <= sig),
sum(pRFVac1 > sig & pRFVac2 <= sig & pRFVac3 <= sig),
sum(pRFVac1 <= sig & pRFVac2 <= sig & pRFVac3 <= sig) ) )




print(c(
sum(pGlmBiVac1 > sig & pGlmBiVac2 > sig & pGlmBiVac3 > sig),
sum(pGlmBiVac1 > sig & pGlmBiVac2 > sig & pGlmBiVac3 <= sig),
sum(pGlmBiVac1 > sig & pGlmBiVac2 <= sig & pGlmBiVac3 <= sig),
sum(pGlmBiVac1 <= sig & pGlmBiVac2 <= sig & pGlmBiVac3 <= sig) ) )

