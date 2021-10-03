
library("rms")
############################################################
# case4 100
############################################################
#=========================alternative========================
load(file = "genDat4_100.rda")

result <- list()
system.time( for(k in c(1:length(genDat4_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2, x=TRUE, y=TRUE, genDat4_100[[k]])
    result[[k]] <- resid(glmFit, 'gof')
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )
  
#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH4_100_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat4_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x3, x=TRUE, y=TRUE, genDat4_100[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH4_100_2_fl.rda")


############################################################
# case4 200
############################################################
#=========================alternative========================
load(file = "genDat4_200.rda")

result <- list()
system.time( for(k in c(1:length(genDat4_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2, x=TRUE, y=TRUE, genDat4_200[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH4_200_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat4_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x3, x=TRUE, y=TRUE, genDat4_200[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH4_200_2_fl.rda")

############################################################
# case4 800
############################################################
#=========================alternative========================
load(file = "genDat4_800.rda")

result <- list()
system.time( for(k in c(1:length(genDat4_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2, x=TRUE, y=TRUE, genDat4_800[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH4_800_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat4_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x3, x=TRUE, y=TRUE, genDat4_800[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH4_800_2_fl.rda")


############################################################
# case5 100
############################################################
#=========================alternative========================
load(file = "genDat5_100.rda")

result <- list()
system.time( for(k in c(1:length(genDat5_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2, x=TRUE, y=TRUE, genDat5_100[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH5_100_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat5_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x1:x2, x=TRUE, y=TRUE, genDat5_100[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH5_100_2_fl.rda")

############################################################
# case5 200
############################################################
#=========================alternative========================
load(file = "genDat5_200.rda")

result <- list()
system.time( for(k in c(1:length(genDat5_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2, x=TRUE, y=TRUE, genDat5_200[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH5_200_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat5_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x1:x2, x=TRUE, y=TRUE, genDat5_200[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH5_200_2_fl.rda")

############################################################
# case5 800
############################################################
#=========================alternative========================
load(file = "genDat5_800.rda")

result <- list()
system.time( for(k in c(1:length(genDat5_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2, x=TRUE, y=TRUE, genDat5_800[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH5_800_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat5_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x1:x2, x=TRUE, y=TRUE, genDat5_800[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH5_800_2_fl.rda")

############################################################
# case6 100
############################################################
#=========================alternative========================
load(file = "genDat6_100.rda")


##########
# error 7
##########
result <- list()
system.time( for(k in c(1:length(genDat6_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x3, x=TRUE, y=TRUE, genDat6_100[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH6_100_1_fl.rda")
##########
# error 7
##########
#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat6_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    x1s <- genDat6_100[[k]]$x1^2
    glmFit <- lrm(y ~ x1 + x2 + x3 + x1s , x=TRUE, y=TRUE, genDat6_100[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH6_100_2_fl.rda")


############################################################
# case6 200
############################################################
#=========================alternative========================
load(file = "genDat6_200.rda")

result <- list()
system.time( for(k in c(1:length(genDat6_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x3, x=TRUE, y=TRUE, genDat6_200[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH6_200_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat6_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    x1s <- genDat6_200[[k]]$x1^2
    glmFit <- lrm(y ~ x1 + x2 + x3 + x1s , x=TRUE, y=TRUE, genDat6_200[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH6_200_2_fl.rda")

############################################################
# case6 800
############################################################
#=========================alternative========================
load(file = "genDat6_800.rda")

result <- list()
system.time( for(k in c(1:length(genDat6_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- lrm(y ~ x1 + x2 + x3, x=TRUE, y=TRUE, genDat6_800[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH6_800_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat6_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    x1s <- genDat6_800[[k]]$x1^2
    glmFit <- lrm(y ~ x1 + x2 + x3 + x1s , x=TRUE, y=TRUE, genDat6_800[[k]])
    result[[k]] <- resid(glmFit, 'gof')
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]][5])) < 0.05)

save( result, 
      file = "simCH6_800_2_fl.rda")