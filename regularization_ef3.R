library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(ncdf4.helpers)
library(PCICt)
library(lubridate)
library(dplyr)
library(tfdatasets)
library(keras)
library(reshape2)
library(maps)
library(fields)
library(glmnet)
library(randomForest)
library(BART)
library(pls)
library(tidyr)
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


#SST_df
anyNA(SST_df)
which(is.na(SST_df))
SST_df <- na.omit(SST_df)
dim(SST_df)

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


##use cross validation to choose the tuning parameter
## actual data is too big so going to use a subset
set.seed(597546)
y = as.matrix(all_long_train$landclusavg)
x = as.matrix(all_long_train[,c(-1, -(7:8))])

xtes = as.matrix(all_long_test[,c(-1, -(7:8))])
ytes = as.matrix(all_long_test$landclusavg)

grid <- 10^seq(10, -2, length = 100)
ridge_mod <- glmnet(x, y, alpha = 0,
                    lambda = grid, thresh = 1e-12)

set.seed(12598)
cv_out <- cv.glmnet(x, y, alpha = 0)
plot(cv_out)
bestlam <- cv_out$lambda.min
bestlam

ridge_pred <- predict(ridge_mod, s = bestlam,
                      newx = xtes)

test_mse_ridge <- mean((ridge_pred - ytes)^2)
test_mse_ridge


precip_yyhat <- data.frame(all_long_test[, c(1, 7:8)], "yhat" = ridge_pred)
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
testmse_ridge <- mean((all_precip_yyhat$s1 - all_precip_yyhat$Precipitation)^2)
testmse_ridge




#################################### LASSO REGRESSION #################################


set.seed(1)
grid <- 10^seq(10, -2, length = 100)
lasso_mod <- glmnet(x, y, alpha = 1,
                    lambda = grid, thresh = 1e-12)

set.seed(12598)
cv_out <- cv.glmnet(x, y, alpha = 1)
plot(cv_out)
bestlam <- cv_out$lambda.min
bestlam

lasso_pred <- predict(lasso_mod, s = bestlam,
                      newx = xtes)


test_mse_lasso <- mean((ridge_pred - ytes)^2)
test_mse_lasso


precip_yyhat <- data.frame(all_long_test[, c(1, 7:8)], "yhat" = lasso_pred)
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
testmse_lasso <- mean((all_precip_yyhat$s1 - all_precip_yyhat$Precipitation)^2)
testmse_lasso

#################################### PCR #################################


pcr.fit <- pcr(landclusavg ~ SeaCluster1 + SeaCluster2 + SeaCluster3 + SeaCluster4 + SeaCluster5,
               data = all_long_train,
               scale = TRUE,
               validation = "CV")

validationplot(pcr.fit , val.type = "MSEP")

#the lowest cross validation error at M=2
pcr.pred <- predict(pcr.fit, newdata = (all_long_test[,c(-1, -7)]), ncomp = 2)

testmse_PCRcont <- mean((pcr.pred - all_long_test[, 8])^2)
testmse_PCRcont


precip_yyhat <- data.frame(all_long_test[, c(1, 7:8)], "yhat" = pcr.pred)
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
testmse_pcr <- mean((all_precip_yyhat$landclusavg.2.comps - all_precip_yyhat$Precipitation)^2)
testmse_pcr


#################################### PLS #################################


pls.fit <- plsr(landclusavg ~ SeaCluster1 + SeaCluster2 + SeaCluster3 + SeaCluster4 + SeaCluster5,
                data = all_long_train,
                scale = TRUE,
                validation = "CV")

summary(pls.fit)

validationplot(pls.fit , val.type = "MSEP")

#the lowest cross validation error at M=2
yhat.pls.pred <- predict(pls.fit, newdata = (all_long_test[,c(-1, -7)]), ncomp = 2)

testmse_PLScont <- mean((yhat.pls.pred - all_long_test[, 8])^2)
testmse_PLScont

precip_yyhat <- data.frame(all_long_test[, c(1, 7:8)], "yhat" = yhat.pls.pred)
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
testmse_pls <- mean((all_precip_yyhat$landclusavg.2.comps - all_precip_yyhat$Precipitation)^2)
testmse_pls


#################################### BAGGING REGRESSION #################################


set.seed(125498)
m <- ncol((all_long_train)[,c(-1, -7)])-1
bag_model <- randomForest(landclusavg ~ SeaCluster1 + SeaCluster2 + SeaCluster3 + SeaCluster4 + SeaCluster5,
                          data = all_long_train,
                          mtry = m,
                          importance = TRUE)

bag_model$importance


yhat.bag <- predict(bag_model, newdata = as.matrix(all_long_test[,c(-1, -7)]))

plot(yhat.bag, all_long_test[, 8])
abline(0,1)

testmsebagcont <- mean((yhat.bag - all_long_test[, 8])^2)
testmsebagcont



precip_yyhat <- data.frame(all_long_test[, c(1, 7:8)], "yhat" = yhat.bag)
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
testmse_bag <- mean((all_precip_yyhat$yhat - all_precip_yyhat$Precipitation)^2)
testmse_bag


#################################### RANDOM FOREST REGRESSION #################################


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

testmserfcont <- mean((yhat.rf - all_long_test[, 8]))
testmserfcont

load("Pdat_df_l.RData")
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
head(all_precip_yyhat)

## save data
save(all_precip_yyhat, file = "all_precip_yyhat.RData")

load("all_precip_yyhat.RData")
testmse_rf <- mean((all_precip_yyhat$yhat - all_precip_yyhat$Precipitation)^2)
testmse_rf
