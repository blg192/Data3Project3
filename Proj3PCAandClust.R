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
library(NbClust)

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
set.seed(662321)
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
kpca2 <- kpca(MonthlySST, kernel = "vanilladot", kpar = list()) ##maybe 3-6 PCs? Better than Random suggests 32
kpca3 <- kpca(MonthlySST, kernel = "laplacedot", kpar = list(sigma = .03)) ##9 - 14 PCs?

kpcarand <- kpca(rand, kernel = "vanilladot", kpar = list())

KPCApcs <- kpca2@pcv[ , 1:6]

## Cluster for KPCA
set.seed(662321)
KPCAbestK <- NbClust(KPCApcs, method = 'kmeans',  min.nc = 2, max.nc = 20, index = "all")
par(mfrow = c(1,1))
KPCAbestK2 <- sapply(1:20, 
                    function(k){kmeans(KPCApcs, k, nstart=50,iter.max = 15 )$tot.withinss})
set.seed(662321)
KPCAclustres <- kmeans(KPCApcs, 5, nstart = 20, iter.max = 15)
KPCAclustout <- factor(KPCAclustres$cluster)

KPCAclust_raster <- rasterFromXYZ(cbind(LongLat, KPCAclustout))
plot(KPCAclust_raster)


baseplot <- ggplot(data = data.frame(KPCApcs, KPCAclustout), aes(x = KPCApcs[, 1], y = KPCApcs[, 2], color = KPCAclustout))

baseplot + geom_point() + form + xlab('Principal Component 1') + 
    ylab('Principal Component 2') + ggtitle('Plot of KPCA Clustering for First Two PCs')


## LLE
## Probably have to turn off parallel if using Windows, takes ages even with parallel
kopt <- calc_k(MonthlySST, m = 2, kmin = 15, kmax = 30, parallel = TRUE, cpus = 3)

## Opt K = 16 for 12, K = 15 for 2, not really sure how to pick m
lleres <- lle(MonthlySST, m = 2, k = 16)
Ymat <- data.frame(lleres$Y)

## Cluster for LLE
set.seed(662321)
LLEbestK <- NbClust(Ymat, method = 'kmeans',  min.nc = 2, max.nc = 20, index = "all")
par(mfrow = c(1,1))
LLEbestK2 <- sapply(1:20, 
                     function(k){kmeans(Ymat, k, nstart=50,iter.max = 15 )$tot.withinss})
set.seed(662321)
LLEclustres <- kmeans(Ymat, 10, nstart = 20, iter.max = 15)
LLEclustout <- factor(LLEclustres$cluster)

LLEclust_raster <- rasterFromXYZ(cbind(LongLat, LLEclustout))
plot(LLEclust_raster)


baseplot <- ggplot(data = data.frame(LLEpcs, LLEclustout), aes(x = LLEpcs[, 1], y = LLEpcs[, 2], color = LLEclustout))

baseplot + geom_point() + form + xlab('Principal Component 1') + 
    ylab('Principal Component 2') + ggtitle('Plot of LLE Clustering for First Two PCs')

## Laplacian Eigenmaps (I don't think this is a good option for clustering regions on. But would be good for a spatial wide data set)

LE <- embed(MonthlySST, .method = 'LaplacianEigenmaps')
LEout <- data.frame(Obs = 1:2512, LE@data@data)

## Cluster for Eigenmap
## Cluster for LLE
set.seed(662321)
LEbestK <- NbClust(LEout, method = 'kmeans',  min.nc = 2, max.nc = 20, index = "all")
par(mfrow = c(1,1))
LEbestK2 <- sapply(1:20, 
                    function(k){kmeans(Ymat, k, nstart=50,iter.max = 15 )$tot.withinss})
set.seed(662321)
LEclustres <- kmeans(LEout, 2, nstart = 20, iter.max = 15) ## suggests 2 or 3 clusters
LEclustout <- factor(LEclustres$cluster)

LEclust_raster <- rasterFromXYZ(cbind(LongLat, LEclustout))
plot(LEclust_raster)


baseplot <- ggplot(data = data.frame(LEout, LEclustout), aes(x = LEout[, 1], y = LEout[, 2], color = LEclustout))

baseplot + geom_point() + form + xlab('Principal Component 1') + 
    ylab('Principal Component 2') + ggtitle('Plot of LE Clustering for First Two PCs')
