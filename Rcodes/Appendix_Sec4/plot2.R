source(file = "parRF.R")


library(plotly)

n = 1000
g = 10

HLplot <- function(bet4, n, cdf, g, angle){
  #generate data
  set.seed(20)
  x1dat <- runif(n, -3, 3)
  x2dat <- rchisq(n, cdf)
  lindat <- -2 + x1dat * 0.3 +
    x2dat * 0.3 +  x1dat^2 * bet4
  pdat <- 1/(1 + exp(-lindat))
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat4 <- data.frame(y = ydat, x1 = x1dat, x2 = x2dat)
  
  mod4 <- glm(y ~ x1 + x2, family = binomial, data = dat4)
  
  
  model<-mod4
  xvar<-"x1"
  yvar<- "x2"
  zvar<- "y"
  pvar <- "dg"
  res <- 100  # This will be the number of values generated when used in seq function
  
  
  #
  (xrange <- range(model$model[[xvar]]) )
  
  (yrange <- range(model$model[[yvar]]))
  
  newdata <- expand.grid(x = seq(xrange[1], xrange[2], length.out = res),
                         y = seq(yrange[1], yrange[2], length.out = res)) 
  
  
  head(newdata)
  
  (names(newdata) <- c(xvar, yvar))
  
  newdata[[zvar]] <- predict(model, newdata = newdata, type = "response") 
  newdata[[pvar]] <- 1/(1 + exp(-(-2 + newdata[[xvar]] * 0.3 +
                                    newdata[[yvar]] * 0.3 +  newdata[[xvar]]^2 * bet4)) )
  
  
  (x <- unique(newdata[[xvar]]))
  (y <- unique(newdata[[yvar]]))
  (z <- matrix(newdata[[zvar]], nrow = length(y), ncol = length(x)))
  (dg <- matrix(newdata[[pvar]], nrow = length(y), ncol = length(x)))
  
  #
  mtlist <- list(x, y, z, dg)
  #
  head(mtlist)
  (names(mtlist) <- c(xvar, yvar, zvar, pvar))
  
  cutyhat4 <- cut(fitted(mod4),
                  breaks = quantile(fitted(mod4), probs = seq(0,
                                                              1, 1/g)), include.lowest = TRUE)
  
  # add group indices to the scatterplot data                                                                                                                                                                                    1, 1/g)), include.lowest = TRUE)
  dat4$group <- cutyhat4
  
  newdata$ynew <- dat4$y
  newdata$x1new <- dat4$x1
  newdata$x2new <- dat4$x2
  newdata$group <- dat4$group
  
  
  
  p1 <- plot_ly(newdata) %>%
    add_trace(x=~newdata[[xvar]], 
              y=~newdata[[yvar]], 
              z=~newdata[[zvar]], 
              type='mesh3d', opacity=0.5) %>%
    add_trace(x=~newdata[[xvar]], 
              y=~newdata[[yvar]], 
              z=~newdata[[pvar]], 
              type='mesh3d', opacity=0.5) %>%
    add_markers(x=~newdata$x1new, 
                y=~newdata$x2new, 
                z=~newdata$ynew,
                color=~group,
                size=1) %>%
    layout(autosize = F, width = 700, height = 700,
           scene = list(
             camera=list(
               eye = list(x=angle[1], y=angle[2], z=angle[3])
             ),
             xaxis = list(title = xvar, tickangle = 0),
             yaxis = list(title = yvar, tickangle = 0),
             zaxis = list(title = "", tickangle = 0),
             title = "test"),
           showlegend = FALSE
    )
  
  return(p1)
}


BAGofTplot <- function(bet4, n, cdf, g, angle){
  #BaGofT partition function parameters
  
  #generate data
  set.seed(20)
  x1dat <- runif(n, -3, 3)
  x2dat <- rchisq(n, cdf)
  lindat <- -2 + x1dat * 0.3 +
    x2dat * 0.3 +  x1dat^2 * bet4
  pdat <- 1/(1 + exp(-lindat))
  ydat <- sapply(pdat, function(x) rbinom(1, 1, x))
  dat4 <- data.frame(y = ydat, x1 = x1dat, x2 = x2dat)
  
  mod4 <- glm(y ~ x1 + x2, family = binomial, data = dat4)
  
  
  model<-mod4
  xvar<-"x1"
  yvar<- "x2"
  zvar<- "y"
  pvar <- "dg"
  res <- 100  # This will be the number of values generated when used in seq function
  
  
  #
  (xrange <- range(model$model[[xvar]]) )
  
  (yrange <- range(model$model[[yvar]]))
  
  newdata <- expand.grid(x = seq(xrange[1], xrange[2], length.out = res),
                         y = seq(yrange[1], yrange[2], length.out = res)) 
  
  
  head(newdata)
  
  (names(newdata) <- c(xvar, yvar))
  
  newdata[[zvar]] <- predict(model, newdata = newdata, type = "response") 
  newdata[[pvar]] <- 1/(1 + exp(-(-2 + newdata[[xvar]] * 0.3 +
                                    newdata[[yvar]] * 0.3 +  newdata[[xvar]]^2 * bet4)) )
  
  
  (x <- unique(newdata[[xvar]]))
  (y <- unique(newdata[[yvar]]))
  (z <- matrix(newdata[[zvar]], nrow = length(y), ncol = length(x)))
  (dg <- matrix(newdata[[pvar]], nrow = length(y), ncol = length(x)))
  
  #
  mtlist <- list(x, y, z, dg)
  #
  head(mtlist)
  (names(mtlist) <- c(xvar, yvar, zvar, pvar))
  
  predT <- predict(mod4, type = "response")
  res <- resid(mod4, type = "pearson")
  par <- parRF(Rsp = "y", predT = predT, res = res,
               Train.data = dat4, Test.data = dat4)
  
  gup <- par$gup
  # add group indices to the scatterplot data                                                                                                                                                                                    1, 1/g)), include.lowest = TRUE)
  dat4$group <- gup
  
  newdata$ynew <- dat4$y
  newdata$x1new <- dat4$x1
  newdata$x2new <- dat4$x2
  newdata$group <- dat4$group
  
  
  
  p1 <- plot_ly(newdata) %>%
    add_trace(x=~newdata[[xvar]], 
              y=~newdata[[yvar]], 
              z=~newdata[[zvar]], 
              type='mesh3d', opacity=0.5) %>%
    add_trace(x=~newdata[[xvar]], 
              y=~newdata[[yvar]], 
              z=~newdata[[pvar]], 
              type='mesh3d', opacity=0.5) %>%
    add_markers(x=~newdata$x1new, 
                y=~newdata$x2new, 
                z=~newdata$ynew,
                color=~group,
                size=1) %>%
    layout(autosize = F, width = 700, height = 700,
           scene = list(
             camera=list(
               eye = list(x=angle[1], y=angle[2], z=angle[3])
             ),
             xaxis = list(title = xvar, tickangle = 0),
             yaxis = list(title = yvar, tickangle = 0),
             zaxis = list(title = "", tickangle = 0)),
           showlegend = FALSE
    )
  
  return(p1)
}

HLplot(bet4 = 0.3, n = 1000, cdf = 8, g = 10, angle = c(0, 2.5, 1))
HLplot(bet4 = 0.3, n = 1000, cdf = 8, g = 10, angle = c(0, 2, 2))

BAGofTplot(bet4 = 0.3, n = 1000, cdf = 8, g = 10, angle = c(0, 2.5, 1))
BAGofTplot(bet4 = 0.3, n = 1000, cdf = 8, g = 10, angle = c(0, 2, 2))





