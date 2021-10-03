
library("rms")
############################################################
# case1 100
############################################################
#=========================alternative========================
load(file = "genDat1_100.rda")

result <- list()
system.time( for(k in c(1:length(genDat1_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2, x=TRUE, y=TRUE, genDat1_100[[k]])
    result[[k]] <- resid(glmFit, 'gof')
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )
  
#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH1_100_1_fl.rda")

##########
# error 22
##########
#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat1_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x3, x=TRUE, y=TRUE, genDat1_100[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH1_100_2_fl.rda")


############################################################
# case1 200
############################################################
#=========================alternative========================
load(file = "genDat1_200.rda")

result <- list()
system.time( for(k in c(1:length(genDat1_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2, x=TRUE, y=TRUE, genDat1_200[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH1_200_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat1_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x3, x=TRUE, y=TRUE, genDat1_200[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH1_200_2_fl.rda")

############################################################
# case1 800
############################################################
#=========================alternative========================
load(file = "genDat1_800.rda")

result <- list()
system.time( for(k in c(1:length(genDat1_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2, x=TRUE, y=TRUE, genDat1_800[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH1_800_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat1_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x3, x=TRUE, y=TRUE, genDat1_800[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH1_800_2_fl.rda")


############################################################
# case2 100
############################################################
#=========================alternative========================
load(file = "genDat2_100.rda")

result <- list()
system.time( for(k in c(1:length(genDat2_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2, x=TRUE, y=TRUE, genDat2_100[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH2_100_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat2_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x1:x2, x=TRUE, y=TRUE, genDat2_100[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH2_100_2_fl.rda")

############################################################
# case2 200
############################################################
#=========================alternative========================
load(file = "genDat2_200.rda")

result <- list()
system.time( for(k in c(1:length(genDat2_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2, x=TRUE, y=TRUE, genDat2_200[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH2_200_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat2_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x1:x2, x=TRUE, y=TRUE, genDat2_200[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH2_200_2_fl.rda")

############################################################
# case2 800
############################################################
#=========================alternative========================
load(file = "genDat2_800.rda")

result <- list()
system.time( for(k in c(1:length(genDat2_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2, x=TRUE, y=TRUE, genDat2_800[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH2_800_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat2_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x1:x2, x=TRUE, y=TRUE, genDat2_800[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH2_800_2_fl.rda")

############################################################
# case3 100
############################################################
#=========================alternative========================
load(file = "genDat3_100.rda")

result <- list()
system.time( for(k in c(1:length(genDat3_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x3, x=TRUE, y=TRUE, genDat3_100[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH3_100_1_fl.rda")
##########
# error 5, 7, 31, 58
##########
#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat3_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    x1s <- genDat3_100[[k]]$x1^2
    glmFit <- lrm(y ~ x1 + x2 + x3 + x1s , x=TRUE, y=TRUE, genDat3_100[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH3_100_2_fl.rda")


############################################################
# case3 200
############################################################
#=========================alternative========================
load(file = "genDat3_200.rda")

result <- list()
system.time( for(k in c(1:length(genDat3_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x3, x=TRUE, y=TRUE, genDat3_200[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH3_200_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat3_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    x1s <- genDat3_200[[k]]$x1^2
    glmFit <- lrm(y ~ x1 + x2 + x3 + x1s , x=TRUE, y=TRUE, genDat3_200[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH3_200_2_fl.rda")

############################################################
# case3 800
############################################################
#=========================alternative========================
load(file = "genDat3_800.rda")

result <- list()
system.time( for(k in c(1:length(genDat3_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x3, x=TRUE, y=TRUE, genDat3_800[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH3_800_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat3_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    x1s <- genDat3_800[[k]]$x1^2
    glmFit <- lrm(y ~ x1 + x2 + x3 + x1s , x=TRUE, y=TRUE, genDat3_800[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH3_800_2_fl.rda")