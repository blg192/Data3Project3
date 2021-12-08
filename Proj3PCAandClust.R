## PCA FILE

## PCA packages
library(readr)
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
# SSTgrand <- mean(MonthlySST)
# MonthlySST_cent <- MonthlySST - SSTgrand

## Start with SVD to get an idea of Prin Comp needed
SSTSVD <- svd(MonthlySST)
SSTsingvals <- matrix(NA, nrow = 842, ncol = 2)
SSTsingvals[, 1] <- 1:842
SSTsingvals[, 2] <- SSTSVD$d
cumsum(SSTSVD$d[1:33]^2)/sum(SSTSVD$d^2)*100


## Set up random data
set.seed(938837)
rand <- matrix(NA, 2512, 842)
for (i in 1:842) {
    rand[ , i] <- MonthlySST[sample(nrow(MonthlySST)), i]
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
colnames(SSTUD) <- c('Long', 'Lat', paste0('PC', 1:33))

## Plot PC 1
PC1_raster <- rasterFromXYZ(SSTUD[, c(1, 2, 3)])
plot(SST_raster)

## Cluster with SVD

## Options for assessing number of clusters
set.seed(662321)
SVDbestK <- NbClust(SSTUD[, -c(1,2)], method = 'kmeans',  min.nc = 2, max.nc = 20, index = "all")
par(mfrow = c(1,1))
SVDbestK2 <- sapply(1:20, 
                    function(k){kmeans(SSTUD[, -c(1,2)], k, nstart=50,iter.max = 15 )$tot.withinss})
SVDclustres <- kmeans(SSTUD[, -c(1, 2)], 2, nstart = 20, iter.max = 15)
SVDclustout <- factor(SVDclustres$cluster)

SVDclust_raster <- rasterFromXYZ(cbind(LongLat, SVDclustout))
plot(SVDclust_raster)

form <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))

baseplot <- ggplot(data = data.frame(SSTUD, SVDclustout), aes(x = PC1, y = PC2, color = SVDclustout))

baseplot + geom_point() + form + xlab('Principal Component 1') + 
           ylab('Principal Component 2') + ggtitle('Plot of Clustering for First Two PCs')

## KPCA
kpca1 <- kpca(MonthlySST, kernel = "rbfdot", kpar = list(sigma = .03)) ## maybe change up sigma but doesn't seem good
kpca2 <- kpca(MonthlySST, kernel = "vanilladot", kpar = list()) ##maybe 3-6 PCs?
kpca3 <- kpca(MonthlySST, kernel = "laplacedot", kpar = list(sigma = .05)) ##9 - 14 PCs?
