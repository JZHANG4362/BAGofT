
library("ResourceSelection")
############################################################
# case4 100
############################################################
#=========================alternative========================
load(file = "genDat4_100.rda")

result <- list()
system.time( for(k in c(1:length(genDat4_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2, family = binomial("logit"), 
                   data = genDat4_100[[k]])
    result[[k]] <- hoslem.test(genDat4_100[[k]]$y, fitted(glmFit), g=10)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )
  
#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL4_100_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat4_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3, family = binomial("logit"), 
                  data = genDat4_100[[k]])
    result[[k]] <- hoslem.test(genDat4_100[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL4_100_2_fl.rda")


############################################################
# case4 200
############################################################
#=========================alternative========================
load(file = "genDat4_200.rda")

result <- list()
system.time( for(k in c(1:length(genDat4_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2, family = binomial("logit"), 
                  data = genDat4_200[[k]])
    result[[k]] <- hoslem.test(genDat4_200[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL4_200_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat4_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3, family = binomial("logit"), 
                  data = genDat4_200[[k]])
    result[[k]] <- hoslem.test(genDat4_200[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL4_200_2_fl.rda")

############################################################
# case4 800
############################################################
#=========================alternative========================
load(file = "genDat4_800.rda")

result <- list()
system.time( for(k in c(1:length(genDat4_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2, family = binomial("logit"), 
                  data = genDat4_800[[k]])
    result[[k]] <- hoslem.test(genDat4_800[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL4_800_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat4_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3, family = binomial("logit"), 
                  data = genDat4_800[[k]])
    result[[k]] <- hoslem.test(genDat4_800[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL4_800_2_fl.rda")

############################################################
# case5 100
############################################################
#=========================alternative========================
load(file = "genDat5_100.rda")

result <- list()
system.time( for(k in c(1:length(genDat5_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2, family = binomial("logit"), 
                  data = genDat5_100[[k]])
    result[[k]] <- hoslem.test(genDat5_100[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL5_100_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat5_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x1:x2, family = binomial("logit"), 
                  data = genDat5_100[[k]])
    result[[k]] <- hoslem.test(genDat5_100[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL5_100_2_fl.rda")

############################################################
# case5 200
############################################################
#=========================alternative========================
load(file = "genDat5_200.rda")

result <- list()
system.time( for(k in c(1:length(genDat5_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2, family = binomial("logit"), 
                  data = genDat5_200[[k]])
    result[[k]] <- hoslem.test(genDat5_200[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL5_200_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat5_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x1:x2, family = binomial("logit"), 
                  data = genDat5_200[[k]])
    result[[k]] <- hoslem.test(genDat5_200[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL5_200_2_fl.rda")

############################################################
# case5 200
############################################################
#=========================alternative========================
load(file = "genDat5_800.rda")

result <- list()
system.time( for(k in c(1:length(genDat5_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2, family = binomial("logit"), 
                  data = genDat5_800[[k]])
    result[[k]] <- hoslem.test(genDat5_800[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL5_800_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat5_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x1:x2, family = binomial("logit"), 
                  data = genDat5_800[[k]])
    result[[k]] <- hoslem.test(genDat5_800[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL5_800_2_fl.rda")


############################################################
# case6 100
############################################################
#=========================alternative========================
load(file = "genDat6_100.rda")

result <- list()
system.time( for(k in c(1:length(genDat6_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3, family = binomial("logit"), 
                  data = genDat6_100[[k]])
    result[[k]] <- hoslem.test(genDat6_100[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL6_100_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat6_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3 + I(x1^2), family = binomial("logit"), 
                  data = genDat6_100[[k]])
    result[[k]] <- hoslem.test(genDat6_100[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL6_100_2_fl.rda")

############################################################
# case6 200
############################################################
#=========================alternative========================
load(file = "genDat6_200.rda")

result <- list()
system.time( for(k in c(1:length(genDat6_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3, family = binomial("logit"), 
                  data = genDat6_200[[k]])
    result[[k]] <- hoslem.test(genDat6_200[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL6_200_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat6_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3 + I(x1^2), family = binomial("logit"), 
                  data = genDat6_200[[k]])
    result[[k]] <- hoslem.test(genDat6_200[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL6_200_2_fl.rda")

############################################################
# case6 800
############################################################
#=========================alternative========================
load(file = "genDat6_800.rda")

result <- list()
system.time( for(k in c(1:length(genDat6_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3, family = binomial("logit"), 
                  data = genDat6_800[[k]])
    result[[k]] <- hoslem.test(genDat6_800[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL6_800_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat6_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3 + I(x1^2), family = binomial("logit"), 
                  data = genDat6_800[[k]])
    result[[k]] <- hoslem.test(genDat6_800[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL6_800_2_fl.rda")
