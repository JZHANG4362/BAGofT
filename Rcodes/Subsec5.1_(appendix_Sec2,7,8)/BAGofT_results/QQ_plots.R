# remove cases with gamma = 0.5

setList <- c(1, 2, 3, 1, 2, 3)


gglist <- list()
j <- 1
datalist <- list()
library(ggplot2)
for (set in c(1:6)){
  for (nsize in c(100, 200, 800)){
    if (set <= 3){
      gamma = 1
    }else{
      gamma = 0.5
    }
    message(paste(set,nsize))
    stt <- setList[set]
    eval(parse(text = paste0("load(file = 'sim",set,"_",nsize,"_2_fl.rda')")) )
    pvalue <- unlist(lapply(c(1:length(result)), function(x)result[[x]]$p.value))
    datalist[[j]] <- pvalue
    j <- j + 1
    
  }
}

gglist[[3]]
datalist[[2]]

stt <- 1
gamma <- 1
nsize <- 100
rej <- mean(datalist[[1]] < 0.05)
p1100 <- ggplot()+
  geom_qq(aes(sample = datalist[[1]]), distribution = stats::qunif) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", size = 1, alpha = 0.5) +
  labs(title=bquote("Setting 1, n="~.(nsize)) )

stt <- 1
gamma <- 1
nsize <- 200
rej <- mean(datalist[[2]] < 0.05)
p1200 <- ggplot()+
  geom_qq(aes(sample = datalist[[2]]), distribution = stats::qunif) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", size = 1, alpha = 0.5) +
  labs(title=bquote("Setting 1, n="~.(nsize)) )

stt <- 1
gamma <- 1
nsize <- 800
rej <- mean(datalist[[3]] < 0.05)
p1800 <- ggplot()+
  geom_qq(aes(sample = datalist[[3]]), distribution = stats::qunif) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", size = 1, alpha = 0.5) +
  labs(title=bquote("Setting 1, n="~.(nsize)) )


stt <- 2
gamma <- 1
nsize <- 100
rej <- mean(datalist[[4]] < 0.05)
p2100 <- ggplot()+
  geom_qq(aes(sample = datalist[[4]]), distribution = stats::qunif) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", size = 1, alpha = 0.5) +
  labs(title=bquote("Setting 2, n="~.(nsize)) )

stt <- 2
gamma <- 1
nsize <- 200
rej <- mean(datalist[[5]] < 0.05)
p2200 <- ggplot()+
  geom_qq(aes(sample = datalist[[5]]), distribution = stats::qunif) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", size = 1, alpha = 0.5) +
  labs(title=bquote("Setting 2, n="~.(nsize)) )

stt <- 2
gamma <- 1
nsize <- 800
rej <- mean(datalist[[6]] < 0.05)
p2800 <- ggplot()+
  geom_qq(aes(sample = datalist[[6]]), distribution = stats::qunif) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", size = 1, alpha = 0.5) +
  labs(title=bquote("Setting 2, n="~.(nsize)) )


stt <- 3
gamma <- 1
nsize <- 100
rej <- mean(datalist[[7]] < 0.05)
p3100 <- ggplot()+
  geom_qq(aes(sample = datalist[[7]]), distribution = stats::qunif) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", size = 1, alpha = 0.5) +
  labs(title=bquote("Setting 3, n="~.(nsize)) )

stt <- 3
gamma <- 1
nsize <- 200
rej <- mean(datalist[[8]] < 0.05)
p3200 <- ggplot()+
  geom_qq(aes(sample = datalist[[8]]), distribution = stats::qunif) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", size = 1, alpha = 0.5) +
  labs(title=bquote("Setting 3, n="~.(nsize)) )

stt <- 3
gamma <- 1
nsize <- 800
rej <- mean(datalist[[9]] < 0.05)
p3800 <- ggplot()+
  geom_qq(aes(sample = datalist[[9]]), distribution = stats::qunif) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", size = 1, alpha = 0.5) +
  labs(title=bquote("Setting 3, n="~.(nsize)) )




library(gridExtra)
pdf(file = "qqplot2_1.pdf", width=8, height=1.5)
grid.arrange(p1100, p1200, p1800,
             ncol=3, nrow = 1)
dev.off()

library(gridExtra)
pdf(file = "qqplot2_others.pdf", width=8, height=3)
grid.arrange(p2100, p2200, p2800, 
             p3100, p3200, p3800, 
             ncol=3, nrow = 2)
dev.off()

