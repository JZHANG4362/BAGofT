
# load test functions
source(file = "BAGofT.R")
source(file = "testGlmBi.R")
source(file = "parRF.R")
source(file = "dcPre.R")
source(file = "VarImp.R")

#load the gset data
load("gset.rda")

#get the names of diagnosis types
diag <- levels(gset$characteristics_ch1)

#cases with diagnosis AD
ADind <- which(gset$characteristics_ch1 == diag[1])
#cases with normal cognitive function
NCind <- which(gset$characteristics_ch1 == diag[4])




#read the csv about which rnas are selected for each type
rnaSel <- read.csv("42003_2019_324_MOESM4_ESM.csv", header = TRUE, skip = 1)

#Mirnas selected by z-score for AD
ADrnaSel <- rnaSel$miRNA[which(rnaSel$disease == "AD")]



#rna data
rdat <- exprs(gset)
rnaNames <- rownames(rdat)


ADrnaDat <- which(rnaNames %in% ADrnaSel)

#principle component analysis
pcgAD <- prcomp(t(rdat[ADrnaDat, c(ADind, NCind)] )) 

#data for AD
ADdat <- data.frame(pcgAD$x[,c(1:20)])
names(ADdat) <- paste("PC", c(1 : 20), sep = "")
ADdat$y <- as.numeric(gset$characteristics_ch1[c(ADind, NCind)] == "diagnosis: AD")


fmList <- c("y ~ PC1", 
            "y ~ PC1 + PC2",
            "y ~ PC1 + PC2 + PC3",
            "y ~ PC1 + PC2 + PC3 + PC4",
            "y ~ PC1 + PC2 + PC3 + PC4 + PC5",
            "y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6",
            "y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7",
            "y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8",
            "y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9",
            "y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10")


##############################################################
#BaGofT test 
##############################################################


# model 1
fm <- y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 

set.seed(20)
BAG_result1 <- BAGofT(testModel = testGlmBi(formula = fm, link = "logit"), 
                      parFun = parRF(preFun = dcPre()),
                      data = ADdat, 
                      nsplits = 20,
                      nsim = 100)

# p-value:  0 Averaged statistic value:  0.2319568

vp1 <- VarImp(BAG_result1)

sort(vp1$Var.imp[,1])
sort(vp1$preVar.imp[,1])


# model 2
fm <- y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC9

set.seed(20)
BAG_result2 <- BAGofT(testModel = testGlmBi(formula = fm, link = "logit"), 
                      parFun = parRF(preFun = dcPre()),
                      data = ADdat, 
                      nsplits = 20,
                      nsim = 100)
# p-value:  0.21 Averaged statistic value:  0.4161016
vp2 <- VarImp(BAG_result2)

sort(vp2$Var.imp[,1])
sort(vp2$preVar.imp[,1])





##############################################################
#BAGofT without extra variables
##############################################################
# model 1
fm <- y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 

set.seed(20)
BAG_result1_1 <- BAGofT(testModel = testGlmBi(formula = fm, link = "logit"), 
                        parFun = parRF(preFun = dcPre(), parVar = paste("PC", c(1:8), sep = "")),
                        data = ADdat, 
                        nsplits = 20,
                        nsim = 100)
# p-value:  0 Averaged statistic value:  0.2427868
# model 2
fm <- y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC9

set.seed(20)
BAG_result2_1 <- BAGofT(testModel = testGlmBi(formula = fm, link = "logit"), 
                        parFun = parRF(preFun = dcPre(), parVar = paste("PC", c(c(1:6), 9), sep = "")),
                        data = ADdat, 
                        nsplits = 20,
                        nsim = 100)
# p-value:  0.06 Averaged statistic value:  0.381274

##############################################################
#HL test 
##############################################################
# model 1
library("ResourceSelection")
lfitAD1 <- glm(fm <- y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 
               , family = binomial("logit"), 
               data = ADdat)
HLAD1 <- hoslem.test(ADdat$y, fitted(lfitAD1), g=10)
# 8.170635 
(HLADstat1 <- HLAD1$statistic)
# 0.4169821
(HLADpval1 <- HLAD1$p.value)

# model 2
library("ResourceSelection")
lfitAD2 <- glm( y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC9, family = binomial("logit"), 
               data = ADdat)
HLAD2 <- hoslem.test(ADdat$y, fitted(lfitAD2), g=10)

# 11.60569 
(HLADstat2 <- HLAD2$statistic)
# 0.1696828
(HLADpval2 <- HLAD2$p.value)

##############################################################
#CH test 
##############################################################
library("rms")

# model 1
Cfit1 <- lrm(y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 
             , x=TRUE, y=TRUE, ADdat)
print(resid(Cfit1, "gof")[5])
# 0.2640515 

# model 2
Cfit2 <- lrm(y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC9
             , x=TRUE, y=TRUE, ADdat)
print(resid(Cfit2, "gof")[5])
# 0.2322957 



##############################################################
#GRP test 
##############################################################
library(GRPtests)
# model 1
set.seed(20)
Xmat <- as.matrix(ADdat[,paste0("PC", c(1:7))])
GRP1 <- GRPtest(X = Xmat, y = ADdat$y, fam = c("binomial") )
# 0.1667222

# model 2
set.seed(20)
Xmat <- as.matrix(ADdat[,paste0("PC", c(1:7, 9))])
GRP2 <- GRPtest(X = Xmat, y = ADdat$y, fam = c("binomial") )
# 0.2346907

##############################################################
#GRP group test considering all 20 principal components 
##############################################################
library(GRPtests)
# model 1
set.seed(20)
Xmat <- as.matrix(ADdat[,paste0("PC", c(1:20))])
GRP1_1 <- GRPgrouptest(X = Xmat, y = ADdat$y, fam = c("binomial"),
                       G = c(8:20))
# 0.1308691

# model 2
set.seed(20)
Xmat <- as.matrix(ADdat[,paste0("PC", c(1:20))])
GRP2_1 <- GRPgrouptest(X = Xmat, y = ADdat$y, fam = c("binomial"),
                       G = c(8,10:20))
# 0.1818182





