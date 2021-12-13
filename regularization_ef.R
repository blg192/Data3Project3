library(glmnet)
library(randomForest)
library(BART)
library(tidyr)
###################REGULARIZATION################################

load("data_in_dfs.RData")


# Missing data
#SST
anyNA(SST_df)
which(is.na(SST_df))
SST_df <- na.omit(SST_df)
dim(SST_df)

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
mean((yhat.bag - SST_df_l[subte, 4]))

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
mean((yhat.rf - SST_df_l[subte, 4]))




#################################### BART #################################

x <- SST_df_l[,1:2]
y <- SST_df_l[,4]
set.seed(1287469)
bartfit <- gbart(x[subtr,], y[subtr], x.test = x[subte,])

yhat_bart <- bartfit$yhat.test.mean
testmse <- mean((y[subte] - yhat_bart)^2)

ord <- order(bartfit$varcount.mean, decreasing = T)
bartfit$varcount.mean[ord]

