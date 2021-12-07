## PCA FILE

## PCA packages
ibrary(readr)
library(ggplot2)
library(kernlab)
library(NMF)
library(lle)
library(dimRed)
library(loe)
library(RSpectra)
library(maps)
library(fields)
library(raster) 


## Call Data
load("data_in_dfs.RData")

# test <- Pdat_df[, 3]
# for (i in 4:ncol(Pdat_df)) {
#     test <- append(test, Pdat_df[, i])    
# }


## SST Data
## Format for ease of use
SST_df <- na.omit(SST_df)
SST_df <- apply(SST_df, 2, as.numeric)
LongLat <- SST_df[, 1:2]
MonthlySST <- SST_df[ , -c(1,2)]

## Center data
SSTgrand <- mean(MonthlySST)
MonthlySST_cent <- MonthlySST - SSTgrand

## Start with SVD to get an idea of Prin Comp needed
SSTSVD <- svd(MonthlySST_cent)
SSTsingvals <- matrix(NA, nrow = 842, ncol = 2)
SSTsingvals[, 1] <- 1:842
SSTsingvals[, 2] <- SSTSVD$d
cumsum(SSTSVD$d[1:33]^2)/sum(SSTSVD$d^2)*100


## Set up random data
set.seed(938837)
rand <- matrix(NA, 2512, 842)
for (i in 1:842) {
    rand[ , i] <- MonthlySST_cent[sample(nrow(MonthlySST_cent)), i]
}

randdecomp <- svd(rand)
SSTsingvals <- cbind(SSTsingvals, as.numeric(randdecomp$d))

## Suggests 33 PCs (97% Var)
plot(SSTsingvals[1:150, 1], SSTsingvals[1:150, 2], type = 'l', xlab = 'Component Number', ylab = 'Singular Value', main = 'Randomized Data Comparison')
lines(SSTsingvals[1:150, 3], type = 'l', col = 'blue')
legend(100, 500, c('Real Trace', 'Random Trace'), fill = c('black', 'blue'))

## PC DF
SSTUD <- SSTSVD$u %*% diag(SSTSVD$d)
SSTUD <- cbind(LongLat, SSTUD[ , -c(34:842)])

## Plot PC 1
SST_raster <- rasterFromXYZ(SSTUD[, c(1, 2, 3)])
plot(SST_raster)

## Cluster with SVD
clustres <- kmeans(SSTUD[, -c(1, 2)], 3, nstart = 20)
