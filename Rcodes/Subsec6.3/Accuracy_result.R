print(data.frame(acuracy = c(as.character(round(results$accuracy,3)),
                       paste("(", round(results$accuSE,4), ")", sep="")),
           AUC = c(as.character(round(results$AUC,3)),
                       paste("(", round(results$AUCSE,4), ")", sep=""))))

########################
#validation set
########################
#ne5
# 0.9
# acuracy      AUC
# 1    0.698    0.767
# 2 (0.0046) (0.0051)
#0.75
# acuracy     AUC
# 1    0.692   0.772
# 2 (0.0032) (0.003)
#0.5
# acuracy      AUC
# 1    0.681    0.753
# 2 (0.0025) (0.0021)


#ne7
# 0.9
# acuracy      AUC
# 1   0.716    0.784
# 2 (0.005) (0.0048)
# 0.75
# acuracy      AUC
# 1   0.707    0.775
# 2 (0.003) (0.0028)
# 0.5
# acuracy     AUC
# 1    0.697   0.763
# 2 (0.0021) (0.002)

#XGboost
# 0.9
# acuracy      AUC
# 1    0.659    0.726
# 2 (0.0061) (0.0067)
# 0.75
# acuracy      AUC
# 1    0.664    0.727
# 2 (0.0031) (0.0032)
# 0.5
# acuracy      AUC
# 1    0.654    0.709
# 2 (0.0023) (0.0025)

########################
#training set
########################
#ne5
# 0.9
# acuracy     AUC
# 1    0.837   0.942
# 2 (0.0016) (7e-04)
# 0.75
# acuracy     AUC
# 1   0.844   0.948
# 2 (0.002) (7e-04)
# 0.5
# acuracy     AUC
# 1    0.863   0.964
# 2 (0.0036) (9e-04)

#ne7
# 0.9
# acuracy     AUC
# 1   0.986   0.999
# 2 (4e-04) (1e-04)
# 0.75
# acuracy   AUC
# 1   0.989 0.999
# 2 (4e-04)   (0)
# 0.5
# acuracy AUC
# 1   0.994   1
# 2 (4e-04) (0)

#XGboost
# 0.9
# acuracy AUC
# 1       1   1
# 2     (0) (0)
# 0.75
# acuracy AUC
# 1       1   1
# 2     (0) (0)
# 0.5
# acuracy AUC
# 1       1   1
# 2     (0) (0)


# colsample_bytree =  0.1,
# subsample = 1, 
# eta = 0.005, 
# max_depth =5