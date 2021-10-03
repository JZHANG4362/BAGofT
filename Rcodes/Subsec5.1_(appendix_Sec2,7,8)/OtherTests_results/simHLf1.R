
library("ResourceSelection")
############################################################
# case1 100
############################################################
#=========================alternative========================
load(file = "genDat1_100.rda")

result <- list()
system.time( for(k in c(1:length(genDat1_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2, family = binomial("logit"), 
                   data = genDat1_100[[k]])
    result[[k]] <- hoslem.test(genDat1_100[[k]]$y, fitted(glmFit), g=10)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )
  
#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL1_100_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat1_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3, family = binomial("logit"), 
                  data = genDat1_100[[k]])
    result[[k]] <- hoslem.test(genDat1_100[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL1_100_2_fl.rda")


############################################################
# case1 200
############################################################
#=========================alternative========================
load(file = "genDat1_200.rda")

result <- list()
system.time( for(k in c(1:length(genDat1_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2, family = binomial("logit"), 
                  data = genDat1_200[[k]])
    result[[k]] <- hoslem.test(genDat1_200[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL1_200_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat1_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3, family = binomial("logit"), 
                  data = genDat1_200[[k]])
    result[[k]] <- hoslem.test(genDat1_200[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL1_200_2_fl.rda")

############################################################
# case1 800
############################################################
#=========================alternative========================
load(file = "genDat1_800.rda")

result <- list()
system.time( for(k in c(1:length(genDat1_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2, family = binomial("logit"), 
                  data = genDat1_800[[k]])
    result[[k]] <- hoslem.test(genDat1_800[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL1_800_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat1_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3, family = binomial("logit"), 
                  data = genDat1_800[[k]])
    result[[k]] <- hoslem.test(genDat1_800[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL1_800_2_fl.rda")

############################################################
# case2 100
############################################################
#=========================alternative========================
load(file = "genDat2_100.rda")

result <- list()
system.time( for(k in c(1:length(genDat2_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2, family = binomial("logit"), 
                  data = genDat2_100[[k]])
    result[[k]] <- hoslem.test(genDat2_100[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL2_100_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat2_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x1:x2, family = binomial("logit"), 
                  data = genDat2_100[[k]])
    result[[k]] <- hoslem.test(genDat2_100[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL2_100_2_fl.rda")

############################################################
# case2 200
############################################################
#=========================alternative========================
load(file = "genDat2_200.rda")

result <- list()
system.time( for(k in c(1:length(genDat2_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2, family = binomial("logit"), 
                  data = genDat2_200[[k]])
    result[[k]] <- hoslem.test(genDat2_200[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL2_200_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat2_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x1:x2, family = binomial("logit"), 
                  data = genDat2_200[[k]])
    result[[k]] <- hoslem.test(genDat2_200[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL2_200_2_fl.rda")

############################################################
# case2 200
############################################################
#=========================alternative========================
load(file = "genDat2_800.rda")

result <- list()
system.time( for(k in c(1:length(genDat2_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2, family = binomial("logit"), 
                  data = genDat2_800[[k]])
    result[[k]] <- hoslem.test(genDat2_800[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL2_800_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat2_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x1:x2, family = binomial("logit"), 
                  data = genDat2_800[[k]])
    result[[k]] <- hoslem.test(genDat2_800[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL2_800_2_fl.rda")


############################################################
# case3 100
############################################################
#=========================alternative========================
load(file = "genDat3_100.rda")

result <- list()
system.time( for(k in c(1:length(genDat3_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3, family = binomial("logit"), 
                  data = genDat3_100[[k]])
    result[[k]] <- hoslem.test(genDat3_100[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL3_100_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat3_100)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3 + I(x1^2), family = binomial("logit"), 
                  data = genDat3_100[[k]])
    result[[k]] <- hoslem.test(genDat3_100[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL3_100_2_fl.rda")

############################################################
# case3 200
############################################################
#=========================alternative========================
load(file = "genDat3_200.rda")

result <- list()
system.time( for(k in c(1:length(genDat3_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3, family = binomial("logit"), 
                  data = genDat3_200[[k]])
    result[[k]] <- hoslem.test(genDat3_200[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL3_200_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat3_200)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3 + I(x1^2), family = binomial("logit"), 
                  data = genDat3_200[[k]])
    result[[k]] <- hoslem.test(genDat3_200[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL3_200_2_fl.rda")

############################################################
# case3 800
############################################################
#=========================alternative========================
load(file = "genDat3_800.rda")

result <- list()
system.time( for(k in c(1:length(genDat3_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3, family = binomial("logit"), 
                  data = genDat3_800[[k]])
    result[[k]] <- hoslem.test(genDat3_800[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL3_800_1_fl.rda")

#====================null===========================
result <- list()
system.time( for(k in c(1:length(genDat3_800)) ){
  message(paste("replication: ",k))
  tryCatch({ 
    glmFit <- glm(y ~ x1 + x2 + x3 + I(x1^2), family = binomial("logit"), 
                  data = genDat3_800[[k]])
    result[[k]] <- hoslem.test(genDat3_800[[k]]$y, fitted(glmFit), g=10)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
} )

#mean(unlist(lapply(c(1:100), function(x)result[[x]]$p.value)) < 0.05)

save( result, 
      file = "simHL3_800_2_fl.rda")
