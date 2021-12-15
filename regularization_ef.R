library(glmnet)
library(randomForest)
library(BART)
library(tidyr)
library(dplyr)
###################REGULARIZATION################################

load("data_in_dfs.RData")


# Missing data
#SST
anyNA(SST_df)
# Missing data
which(is.na(SST_df))

# Gather rectangular area with no missing values
SST_df_rect = SST_df[which(SST_df$long < 246),]
SST_df_rect = SST_df_rect[which(SST_df_rect$long > 152),]
# SST_df <- na.omit(SST_df)
# dim(SST_df)

#Pdat
anyNA(Pdat_df)
which(is.na(Pdat_df))
Pdat_df <- na.omit(Pdat_df)
dim(Pdat_df)

## long data set
Pdat_df_l <- gather(Pdat_df, time, precip, 3:ncol(Pdat_df))
SST_df_l <- gather(SST_df, time, sst, 3:ncol(SST_df))

## set up training and testing for long data set
## find what row is the start of 2017 for precip and sst
rowminprecip <- min(which(Pdat_df_l$time == "Jan2017"))
rowminsst <- min(which(SST_df_l$time == "Jan2017"))

## training and data precip
trainp <- c(1:(rowminprecip - 1)) # training data up to Jan 2017
testp <- c(rowminprecip:nrow(Pdat_df_l)) # testing data Jan 2017 and beyond
precip_test <- Pdat_df_l[-trainp, ]

## training and testing data sst
trains <- c(1:(rowminsst - 1)) # training data up to Jan 2017
tests <- c(rowminsst:nrow(SST_df_l)) # testing data Jan 2017 and beyond
sst_test <- SST_df_l[-trains, ]



# load 
load("SST_clustwide.RData")
load("Pdat_clustwide.RData")
load("SST_longagg.RData")
load("Pdat_longagg.RData")

## row 829 is beginning of 2017
train <- c(1:828)

all_train <- data.frame(Pdat_clustwide[train,], SST_clustwide[train,])
all_long_train <- gather(all_train, landclus, landclusavg, 2:13)
all_long_train <- all_long_train[-2]
all_test <- data.frame(Pdat_clustwide[-train,], SST_clustwide[-train,])
all_long_test <- gather(all_test, landclus, landclusavg, 2:13)
all_long_test <- all_long_test[-2]


#################################### RIDGE REGRESSION #################################

##SST data
##use cross validation to choose the tuning parameter
## actual data is too big so going to use a subset
set.seed(597546)
subset <- sample(1:nrow(SST_df_l), 0.4*nrow(SST_df_l))
x <- model.matrix(sst ~ ., SST_df_l[subset, ])[, -1]
y <- SST_df_l[subset, ]$sst

set.seed(897521)
train <- sample(1:nrow(x), 0.5*nrow(x))
test <- (-train)
y_test <- y[test]

grid <- 10^seq(10, -2, length = 100)
ridge_mod <- glmnet(x[train, ], y[train], alpha = 0,
                    lambda = grid, thresh = 1e-12)

set.seed(12598)
cv_out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv_out)
bestlam <- cv_out$lambda.min
bestlam

ridge_pred <- predict(ridge_mod, s = bestlam,
                      newx = x[test, ])

test_mse <- mean((ridge_pred - y_test)^2)

###Precipitation Data
##use cross validation to choose the tuning parameter
set.seed(597546)
subset <- sample(1:nrow(Pdat_df_l), 0.15*nrow(Pdat_df_l))
x <- model.matrix(precip ~ ., Pdat_df_l[subset, ])[, -1]
y <- Pdat_df_l[subset, ]$precip

set.seed(897521)
train <- sample(1:nrow(x), 0.5*nrow(x))
test <- (-train)
y_test <- y[test]

grid <- 10^seq(10, -2, length = 100)
ridge_mod <- glmnet(x[train, ], y[train], alpha = 0,
                    lambda = grid, thresh = 1e-12)

set.seed(12598)
cv_out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv_out)
bestlam <- cv_out$lambda.min
bestlam

ridge_pred <- predict(ridge_mod, s = bestlam,
                      newx = x[test, ])

test_mse <- mean((ridge_pred - y_test)^2)


#################################### LASSO REGRESSION #################################

##SST data
##use cross validation to choose the tuning parameter
set.seed(121895)
cv.out <- cv.glmnet(x = as.matrix(SST_df_l[trains, 1:2]),
                    y = as.matrix(SST_df_l[trains, 4]),
                    alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

lasso.sst = glmnet(
  x = as.matrix(SST_df_l[trains, 1:2]),
  y = as.matrix(SST_df_l[trains, 4]),
  alpha = 1,
  lambda = bestlam
)

lasso.pred <- predict(lasso.sst, 
                      s = bestlam,
                      newx = as.matrix(SST_df_l[tests, 1:2]))

mse.lasso.sst <- mean((as.matrix(SST_df_l[tests, 4]) - lasso.pred)^2)
mse.lasso.sst


###Precipitation Data

##use cross validation to choose the tuning parameter
set.seed(235871)
cv.out <- cv.glmnet(
  x = as.matrix(Pdat_df_l[trainp, 1:2]),
  y = as.matrix(Pdat_df_l[trainp, 4]),
  alpha = 1)

plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

lasso.precip = glmnet(
  x = as.matrix(Pdat_df_l[trainp, 1:2]),
  y = as.matrix(Pdat_df_l[trainp, 4]),
  alpha = 1,
  lambda = bestlam
)

lasso.pred <- predict(lasso.precip, 
                      s = bestlam,
                      newx = as.matrix(Pdat_df_l[testp, 1:2]))

mse.lasso.precip <- mean((as.matrix(Pdat_df_l[testp, 4]) - lasso.pred)^2)
mse.lasso.precip




#################################### BAGGING REGRESSION #################################

set.seed(122486)
## need to subset further
## take a subset from training set
subtr <- sample(1:nrow(SST_df_l[trains, ]), 0.05*nrow(SST_df_l[trains, ]))
## take subset from testing set
subte <- sample(1:nrow(SST_df_l[tests, ]), 0.05*nrow(SST_df_l[tests, ]))

bag.sst <- randomForest(SST_df_l[subtr, 4] ~ .,
                        data = SST_df_l[subtr, ],
                        mtry = 3,
                        importance = TRUE)

bag.sst


yhat.bag <- predict(bag.sst, newdata = as.matrix(SST_df_l[subte,]))
plot(yhat.bag, SST_df_l[subte, 4])
abline(0,1)
mean((yhat.bag - SST_df_l[subte, 4])^2)

#################################### RANDOM FOREST REGRESSION #################################


set.seed(125498)
rf.sst <- randomForest(SST_df_l[subtr, 4] ~ .,
                       data = SST_df_l[subtr, ],
                       mtry = 1,
                       importance = TRUE)

rf.sst$importance


yhat.rf <- predict(rf.sst, newdata = as.matrix(SST_df_l[subte,]))

plot(yhat.rf, SST_df_l[subte, 4])
abline(0,1)
mean((yhat.rf - SST_df_l[subte, 4])^2)





# load 
load("SST_clustwide.RData")
load("Pdat_clustwide.RData")
load("SST_longagg.RData")
load("Pdat_longagg.RData")
load("Pdat_clus.RData")

## row 829 is beginning of 2017
train <- c(1:828)

all_train <- data.frame(Pdat_clustwide[train,], SST_clustwide[train,])
all_long_train <- gather(all_train, landclus, landclusavg, 2:13)
all_long_train <- all_long_train[-2]

all_test <- data.frame(Pdat_clustwide[-train,], SST_clustwide[-train,])
all_long_test <- gather(all_test, landclus, landclusavg, 2:13)
all_long_test <- all_long_test[-2]

all <- data.frame(Pdat_clustwide, SST_clustwide)
all_long <- gather(all, landclus, landclusavg, 2:13)
all_long <- all_long[-2]
## split by cluster
all_long <- split(all_long, all_long$landclus)

save(all_long, file = "all_long.RData")
load("all_long.RData")


## using sst and precip
set.seed(125498)
m <- round(sqrt(ncol(all_long_train) - 2), 0)
rf.sst <- randomForest(landclusavg ~ SeaCluster1 + SeaCluster2 + SeaCluster3 + SeaCluster4 + SeaCluster5,
                       data = all_long_train,
                       mtry = m,
                       importance = TRUE)

rf.sst$importance


yhat.rf <- predict(rf.sst, newdata = as.matrix(all_long_test[,c(-1, -7)]))

plot(yhat.rf, all_long_test[, 8])
abline(0,1)
testmse <- mean((yhat.rf - all_long_test[, 8])^2)

# load("Pdat_df_l.RData")
precip_yyhat <- data.frame(all_long_test[, c(1, 7:8)], "yhat" = yhat.rf)
for(i in 1:nrow(precip_yyhat)){
  if(precip_yyhat$landclus[i] == "LandCluster1"){
    precip_yyhat$landclus[i] <- 1
  } else if(precip_yyhat$landclus[i] == "LandCluster2"){
    precip_yyhat$landclus[i] <- 2
  } else if(precip_yyhat$landclus[i] == "LandCluster3"){
    precip_yyhat$landclus[i] <- 3
  } else if(precip_yyhat$landclus[i] == "LandCluster4"){
    precip_yyhat$landclus[i] <- 4
  } else if(precip_yyhat$landclus[i] == "LandCluster5"){
    precip_yyhat$landclus[i] <- 5
  } else if(precip_yyhat$landclus[i] == "LandCluster6"){
    precip_yyhat$landclus[i] <- 6
  } else if(precip_yyhat$landclus[i] == "LandCluster7"){
    precip_yyhat$landclus[i] <- 7
  } else if(precip_yyhat$landclus[i] == "LandCluster8"){
    precip_yyhat$landclus[i] <- 8
  } else if(precip_yyhat$landclus[i] == "LandCluster9"){
    precip_yyhat$landclus[i] <- 9
  } else if(precip_yyhat$landclus[i] == "LandCluster10"){
    precip_yyhat$landclus[i] <- 10
  } else if(precip_yyhat$landclus[i] == "LandCluster11"){
    precip_yyhat$landclus[i] <- 11
  } else{
    precip_yyhat$landclus[i] <- 12
  }
}
precip_yyhat$landclus <- as.factor(precip_yyhat$landclus)

## limit long_Pdat to the test observations
long_Pdat_test <- long_Pdat[min(which(long_Pdat$Date == "Jan2017")):nrow(long_Pdat),]

all_precip_yyhat <- merge.data.frame(long_Pdat_test, precip_yyhat, 
                                     by.x = c(4, 3), by.y = 1:2)

## save data
save(all_precip_yyhat, file = "all_precip_yyhat.RData")

load("all_precip_yyhat.RData")
testmse_rf <- mean((all_precip_yyhat$yhat - all_precip_yyhat$Precipitation)^2)



## Now do random forest with lagged data
## lag the sst data
## set tau to 6 months
# all <- data.frame(Pdat_clustwide, SST_clustwide)
# all_long <- gather(all, landclus, landclusavg, 2:13)
# all_long <- all_long[-2]
# save(all_long, file = "all_long.RData")
load("all_long.RData")


lagdata <- function(df, tau){
  ## df is dataframe
  ## tau is distance from prediction point
  mat <- as.matrix(df)
  lag <- matrix(NA, nrow = nrow(mat) + tau, ncol = ncol(mat))
  colnames(lag) <- c(colnames(mat))
  for(i in 1:nrow(lag)){
    if(i < (tau+1)){
      lag[i, 1] <- mat[i, 1]
      lag[i, 2:6] <- c(rep(NA, 5))
      lag[i, 7:8] <- mat[i, 7:8]
    } else if(i < (nrow(mat) + 1)){
      lag[i, 1] <- mat[i, 1]
      lag[i, 2:6] <- mat[(i - tau), 2:6]
      lag[i, 7:8] <- mat[i, 7:8]
    } else{
      lag[i, 1] <- NA
      lag[i, 2:6] <- mat[(i - tau), 2:6]
      lag[i, 7:8] <- c(NA, 2)
    }
  }
  lagdf <- as.data.frame(lag)
  return(lagdf)
}

## lagged data when tau = 6 months
tau <- 6
lagdf <- vector("list", length = 12)
train <- vector("list", length = 12)
rf_lag <- vector("list", length = 12)
yhat.rflag <- vector("list", length = 12)
testmse <- vector("list", length = 12)
precip_yyhat_lag <- vector("list", length = 12)
for(i in 1:12){
  lagdf[[i]] <- lagdata(all_long[[i]], tau)
  ## remove top and bottom tau rows with NAs
  lagdf[[i]] <- lagdf[[i]][(tau + 1):nrow(all_long[[i]]),]
  
  ## need to switch to factor, numeric, etc
  lagdf[[i]]$SeaCluster1 <- as.numeric(lagdf[[i]]$SeaCluster1)
  lagdf[[i]]$SeaCluster2 <- as.numeric(lagdf[[i]]$SeaCluster2)
  lagdf[[i]]$SeaCluster3 <- as.numeric(lagdf[[i]]$SeaCluster3)
  lagdf[[i]]$SeaCluster4 <- as.numeric(lagdf[[i]]$SeaCluster4)
  lagdf[[i]]$SeaCluster5 <- as.numeric(lagdf[[i]]$SeaCluster5)
  lagdf[[i]]$landclusavg <- as.numeric(lagdf[[i]]$landclusavg)
  
  ## split lag data into train and test
  ## row 823 is beginning of 2017
  train[[i]] <- c(1:max(which(lagdf[[i]]$Date == "Dec2016")))
  
  ## do everything else again
  ## using lagged sst and precip
  set.seed(125498)
  m <- round(sqrt(ncol(lagdf[[i]]) - 2), 0)
  rf_lag[[i]] <- randomForest(landclusavg ~ SeaCluster1 + SeaCluster2 + SeaCluster3 + SeaCluster4 + SeaCluster5,
                         data = lagdf[[i]][train[[i]],],
                         mtry = m,
                         importance = TRUE)
  
  yhat.rflag[[i]] <- predict(rf_lag[[i]], newdata = as.matrix(lagdf[[i]][-train[[i]], c(-1, -7)]))
  
  testmse[[i]] <- mean((yhat.rflag[[i]] - lagdf[[i]][-train[[i]], 8])^2)
  
  precip_yyhat_lag[[i]] <- data.frame(lagdf[[i]][-train[[i]], c(1, 7:8)], "yhat" = yhat.rflag[[i]])
  for(j in 1:nrow(precip_yyhat_lag[[i]])){
    if(precip_yyhat_lag[[i]]$landclus[j] == "LandCluster1"){
      precip_yyhat_lag[[i]]$landclus[j] <- 1
    } else if(precip_yyhat_lag[[i]]$landclus[j] == "LandCluster2"){
      precip_yyhat_lag[[i]]$landclus[j] <- 2
    } else if(precip_yyhat_lag[[i]]$landclus[j] == "LandCluster3"){
      precip_yyhat_lag[[i]]$landclus[j] <- 3
    } else if(precip_yyhat_lag[[i]]$landclus[j] == "LandCluster4"){
      precip_yyhat_lag[[i]]$landclus[j] <- 4
    } else if(precip_yyhat_lag[[i]]$landclus[j] == "LandCluster5"){
      precip_yyhat_lag[[i]]$landclus[j] <- 5
    } else if(precip_yyhat_lag[[i]]$landclus[j] == "LandCluster6"){
      precip_yyhat_lag[[i]]$landclus[j] <- 6
    } else if(precip_yyhat_lag[[i]]$landclus[j] == "LandCluster7"){
      precip_yyhat_lag[[i]]$landclus[j] <- 7
    } else if(precip_yyhat_lag[[i]]$landclus[j] == "LandCluster8"){
      precip_yyhat_lag[[i]]$landclus[j] <- 8
    } else if(precip_yyhat_lag[[i]]$landclus[j] == "LandCluster9"){
      precip_yyhat_lag[[i]]$landclus[j] <- 9
    } else if(precip_yyhat_lag[[i]]$landclus[j] == "LandCluster10"){
      precip_yyhat_lag[[i]]$landclus[j] <- 10
    } else if(precip_yyhat_lag[[i]]$landclus[j] == "LandCluster11"){
      precip_yyhat_lag[[i]]$landclus[j] <- 11
    } else{
      precip_yyhat_lag[[i]]$landclus[j] <- 12
    }
  }
  precip_yyhat_lag[[i]]$landclus <- as.factor(precip_yyhat_lag[[i]]$landclus)
  
}

## mse with lagged data
testmse <- mean(unlist(testmse))

## need to set i for importance and plot
# rf_lag[[i]]$importance
# plot(yhat.rflag, lagdf[-train, 8])
# abline(0,1)

precip_yyhat_lag2 <- rbind.data.frame(precip_yyhat_lag[[1]], precip_yyhat_lag[[2]],
                                      precip_yyhat_lag[[3]], precip_yyhat_lag[[4]],
                                      precip_yyhat_lag[[5]], precip_yyhat_lag[[6]],
                                      precip_yyhat_lag[[7]], precip_yyhat_lag[[8]],
                                      precip_yyhat_lag[[9]], precip_yyhat_lag[[10]],
                                      precip_yyhat_lag[[11]], precip_yyhat_lag[[12]])

## limit lagdf to the test observations
all_yyhat_lag <- merge.data.frame(long_Pdat_test, precip_yyhat_lag2, 
                                     by.x = c(4, 3), by.y = 1:2)

## save data
save(all_yyhat_lag, file = "all_yyhat_lag.RData")

load("all_yyhat_lag.RData")
testmse_rflag <- mean((all_yyhat_lag$yhat - all_yyhat_lag$Precipitation)^2)




#################################### BART #################################

x <- SST_df_l[,1:2]
y <- SST_df_l[,4]
set.seed(1287469)
bartfit <- gbart(x[subtr,], y[subtr], x.test = x[subte,])

yhat_bart <- bartfit$yhat.test.mean
testmse <- mean((y[subte] - yhat_bart)^2)

ord <- order(bartfit$varcount.mean, decreasing = T)
bartfit$varcount.mean[ord]

