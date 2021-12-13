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
library(reshape2)

## Call Data
load("data_in_dfs.RData")

# test <- Pdat_df[, 3]
# for (i in 4:ncol(Pdat_df)) {
#     test <- append(test, Pdat_df[, i])    
# }


## Just want to run final PCAs? Scroll to line 325 and start there. 

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
plot(PC1_raster)

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

## Make sure to run this line for GGPlots
form <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))

baseplot <- ggplot(data = data.frame(SSTUD, SVDclustout), aes(x = PC1, y = PC2, color = SVDclustout))

baseplot + geom_point() + form + xlab('Principal Component 1') + 
           ylab('Principal Component 2') + ggtitle('Plot of Clustering for First Two PCs')

## KPCA
kpca1 <- kpca(MonthlySST, kernel = "rbfdot", kpar = list(sigma = .03)) ## maybe change up sigma but doesn't seem good
kpca2 <- kpca(MonthlySST, kernel = "vanilladot", kpar = list()) ##maybe 3-6 PCs? Better than Random suggests 32. This with 6 PCs is best?
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
## Suggests 5 Clusters
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
                    function(k){kmeans(LEout, k, nstart=50,iter.max = 15 )$tot.withinss})
set.seed(662321)
LEclustres <- kmeans(LEout, 2, nstart = 20, iter.max = 15) ## suggests 2 or 3 clusters
LEclustout <- factor(LEclustres$cluster)

LEclust_raster <- rasterFromXYZ(cbind(LongLat, LEclustout))
plot(LEclust_raster)


baseplot <- ggplot(data = data.frame(LEout, LEclustout), aes(x = LEout[, 1], y = LEout[, 2], color = LEclustout))

baseplot + geom_point() + form + xlab('Principal Component 1') + 
    ylab('Principal Component 2') + ggtitle('Plot of LE Clustering for First Two PCs')




## Precip PCAs
## Precip Data
## Format for ease of use
Pdat_df <- na.omit(Pdat_df)
Pdat_df <- apply(Pdat_df, 2, as.numeric)
LongLat2 <- Pdat_df[, 1:2]
MonthlyPrecip <- Pdat_df[ , -c(1,2)]

## Start with SVD to get an idea of Prin Comp needed
PrecipSVD <- svd(MonthlyPrecip)
Precipsingvals <- matrix(NA, nrow = 842, ncol = 2)
Precipsingvals[, 1] <- 1:842
Precipsingvals[, 2] <- PrecipSVD$d
cumsum(PrecipSVD$d[1:26]^2)/sum(PrecipSVD$d^2)*100

## Set up random data
set.seed(374378)
rand2 <- matrix(NA, 5390, 842)
for (i in 1:842) {
    rand2[ , i] <- MonthlyPrecip[sample(nrow(MonthlyPrecip)), i]
}

rand2decomp <- svd(rand2)
Precipsingvals <- cbind(Precipsingvals, as.numeric(rand2decomp$d))

## Suggests 26 PCs (95.6% Var)
plot(Precipsingvals[1:150, 1], Precipsingvals[1:150, 2], type = 'l', xlab = 'Component Number', 
     ylab = 'Singular Value', main = 'Randomized Data Comparison')
lines(Precipsingvals[1:150, 3], type = 'l', col = 'blue')
legend(100, 500, c('Real Trace', 'Random Trace'), fill = c('black', 'blue'))

## PC DF
PrecipUD <- PrecipSVD$u %*% diag(PrecipSVD$d)
PrecipUD <- cbind(LongLat2, PrecipUD[ , -c(27:842)])
colnames(PrecipUD) <- c('Long', 'Lat', paste0('PC', 1:26))

## Plot PC 1
PC1_raster <- rasterFromXYZ(PrecipUD[, c(1, 2, 3)])
plot(PC1_raster)

## Cluster with SVD (NOT WORKING YET)

## Options for assessing number of clusters
set.seed(629787)
PSVDbestK <- NbClust(PrecipUD[, -c(1,2)], method = 'kmeans',  min.nc = 2, max.nc = 15, index = "all")
par(mfrow = c(1,1))
PSVDbestK2 <- sapply(1:20, 
                    function(k){kmeans(PrecipUD[, -c(1,2)], k, nstart=50,iter.max = 20)$tot.withinss})
set.seed(629787)
PSVDclustres <- kmeans(PrecipUD[, -c(1, 2)], 4, nstart = 20, iter.max = 20) ## 2 or 4 suggested as best
PSVDclustout <- factor(PSVDclustres$cluster)

PSVDclust_raster <- rasterFromXYZ(cbind(LongLat2, PSVDclustout))
plot(PSVDclust_raster)

baseplot <- ggplot(data = data.frame(PrecipUD, PSVDclustout), aes(x = PC1, y = PC2, color = PSVDclustout))

baseplot + geom_point() + form + xlab('Principal Component 1') + 
    ylab('Principal Component 2') + ggtitle('Plot of Clustering for First Two PCs')

## KPCA
Pkpca1 <- kpca(MonthlyPrecip, kernel = "rbfdot", kpar = list(sigma = .005)) ## Hard to beat the SVD with reduction
Pkpca2 <- kpca(MonthlyPrecip, kernel = "vanilladot", kpar = list()) ##maybe 2-4 PCs? Better than Random suggests 25. I think this with the fewer PCs is best?
Pkpca3 <- kpca(MonthlyPrecip, kernel = "laplacedot", kpar = list(sigma = .01)) ##Maybe similar to SVD
cumsum(Pkpca1@eig[1:26]^2)/sum(Pkpca1@eig^2)*100

Pkpcarand2 <- kpca(rand2, kernel = "vanilladot", kpar = list())

PKPCApcs <- Pkpca2@pcv[ , 1:4]

## Cluster for KPCA
set.seed(662321)
PKPCAbestK <- NbClust(PKPCApcs, method = 'kmeans',  min.nc = 2, max.nc = 20, index = "all")
par(mfrow = c(1,1))
PKPCAbestK2 <- sapply(1:20, 
                     function(k){kmeans(PKPCApcs, k, nstart=50,iter.max = 15 )$tot.withinss})
## 12 Suggested
set.seed(662321)
PKPCAclustres <- kmeans(PKPCApcs, 12, nstart = 20, iter.max = 15)
PKPCAclustout <- factor(PKPCAclustres$cluster)

PKPCAclust_raster <- rasterFromXYZ(cbind(LongLat2, PKPCAclustout))
plot(PKPCAclust_raster)

baseplot <- ggplot(data = data.frame(PKPCApcs, PKPCAclustout), aes(x = PKPCApcs[, 1], y = PKPCApcs[, 2], color = PKPCAclustout))

baseplot + geom_point() + form + xlab('Principal Component 1') + 
    ylab('Principal Component 2') + ggtitle('Plot of KPCA Clustering for First Two PCs')

## LLE
## Probably have to turn off parallel if using Windows, takes ages even with parallel
kopt2 <- calc_k(MonthlyPrecip, m = 2, kmin = 15, kmax = 30, parallel = TRUE, cpus = 3)

## Opt K = 25 for 2
Plleres <- lle(MonthlyPrecip, m = 2, k = 25)
PYmat <- data.frame(Plleres$Y)

## Cluster for LLE
set.seed(662321)
PLLEbestK <- NbClust(PYmat, method = 'kmeans',  min.nc = 2, max.nc = 20, index = "all")
par(mfrow = c(1,1))
PLLEbestK2 <- sapply(1:20, 
                    function(k){kmeans(PYmat, k, nstart=50,iter.max = 20)$tot.withinss})
set.seed(662321)
PLLEclustres <- kmeans(PYmat, 3, nstart = 20, iter.max = 25) #3 suggested, 2 next
PLLEclustout <- factor(PLLEclustres$cluster)

PLLEclust_raster <- rasterFromXYZ(cbind(LongLat2, PLLEclustout))
plot(PLLEclust_raster)


baseplot <- ggplot(data = data.frame(PYmat, PLLEclustout), aes(x = PYmat[, 1], y = PYmat[, 2], color = PLLEclustout))

baseplot + geom_point() + form + xlab('Principal Component 1') + 
    ylab('Principal Component 2') + ggtitle('Plot of LLE Clustering for First Two PCs')

# Laplacian Eigenmaps (I don't think this is a good option for clustering regions on. But would be good for a spatial wide data set)

PLE <- embed(MonthlyPrecip, .method = 'LaplacianEigenmaps')
PLEout <- data.frame(Obs = 1:5390, LE@data@data)

## Cluster for Eigenmap
## Cluster for LLE
set.seed(662321)
PLEbestK <- NbClust(PLEout, method = 'kmeans',  min.nc = 2, max.nc = 20, index = "all")
par(mfrow = c(1,1))
PLEbestK2 <- sapply(1:20, 
                   function(k){kmeans(PLEout, k, nstart=50,iter.max = 15 )$tot.withinss})
set.seed(662321)
PLEclustres <- kmeans(PLEout, 3, nstart = 20, iter.max = 20) ## suggests 2 or 3 clusters
PLEclustout <- factor(PLEclustres$cluster)

PLEclust_raster <- rasterFromXYZ(cbind(LongLat2, PLEclustout))
plot(PLEclust_raster)


baseplot <- ggplot(data = data.frame(PLEout, PLEclustout), aes(x = PLEout[, 1], y = PLEout[, 2], color = PLEclustout))

baseplot + geom_point() + form + xlab('Principal Component 1') + 
    ylab('Principal Component 2') + ggtitle('Plot of LE Clustering for First Two PCs')


## I think KPCA might be the best options for both data sets

## START HERE FOR NECESSARY CODE
load("data_in_dfs.RData")

form <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))

SST_df <- na.omit(SST_df)
SST_df <- apply(SST_df, 2, as.numeric)
LongLat <- SST_df[, 1:2]
MonthlySST <- SST_df[ , -c(1,2)]

kpca2 <- kpca(MonthlySST, kernel = "vanilladot", kpar = list())
KPCApcs <- kpca2@pcv[ , 1:6]
set.seed(662321)
KPCAclustres <- kmeans(KPCApcs, 5, nstart = 20, iter.max = 15)
KPCAclustout <- factor(KPCAclustres$cluster)
KPCAclust_raster <- rasterFromXYZ(cbind(LongLat, KPCAclustout))
plot(KPCAclust_raster)
baseplot <- ggplot(data = data.frame(KPCApcs, KPCAclustout), aes(x = KPCApcs[, 1], y = KPCApcs[, 2], color = KPCAclustout))
baseplot + geom_point() + form + xlab('Principal Component 1') + 
    ylab('Principal Component 2') + ggtitle('Plot of KPCA Clustering for First Two PCs')

Pdat_df <- na.omit(Pdat_df)
Pdat_df <- apply(Pdat_df, 2, as.numeric)
LongLat2 <- Pdat_df[, 1:2]
MonthlyPrecip <- Pdat_df[ , -c(1,2)]

Pkpca2 <- kpca(MonthlyPrecip, kernel = "vanilladot", kpar = list())
PKPCApcs <- Pkpca2@pcv[ , 1:4]
set.seed(662321)
PKPCAclustres <- kmeans(PKPCApcs, 12, nstart = 20, iter.max = 15)
PKPCAclustout <- factor(PKPCAclustres$cluster)
PKPCAclust_raster <- rasterFromXYZ(cbind(LongLat2, PKPCAclustout))
plot(PKPCAclust_raster)
baseplot <- ggplot(data = data.frame(PKPCApcs, PKPCAclustout), aes(x = PKPCApcs[, 1], y = PKPCApcs[, 2], color = PKPCAclustout))
baseplot + geom_point() + form + xlab('Principal Component 1') + 
    ylab('Principal Component 2') + ggtitle('Plot of KPCA Clustering for First Two PCs')


## Adding clusters to data frames and reformatting 

## Long Format SST
SST_df_clusts <- data.frame(SST_df, Clusters = KPCAclustout)
long_SST <- melt(SST_df_clusts, id.vars = colnames(SST_df_clusts[, c(1, 2, 845)]))
colnames(long_SST) <- c('Longitude', 'Latitude', 'Cluster', 'Date', 'Temperature')
## saved data frame
# save(long_SST, file = "SST_clus.RData")

## Long Format Precip
Pdat_df_clusts <- data.frame(Pdat_df, Clusters = PKPCAclustout)
long_Pdat <- melt(Pdat_df_clusts, id.vars = colnames(SST_df_clusts[, c(1, 2, 845)]))
colnames(long_Pdat) <- c('Longitude', 'Latitude', 'Cluster', 'Date', 'Precipitation')
## saved data frame
# save(long_Pdat, file = "Pdat_clus.RData")

## Set up for a network? (IGNORE THIS FOR NOW)
SST_clustwide <- dcast(data = long_SST[, -c(1,2)], Date ~ Cluster, fun.aggregate = mean)
colnames(SST_clustwide) <- c('Date', 'SeaCluster1', 'SeaCluster2', 'SeaCluster3', 'SeaCluster4', 'SeaCluster5')

Pdat_clustwide <- dcast(data = long_Pdat[, -c(1,2)], Date ~ Cluster, fun.aggregate = mean)
colnames(Pdat_clustwide) <- c('Date', 'LandCluster1', 'LandCluster2', 'LandCluster3', 'LandCluster4', 'LandCluster5',
                             'LandCluster6', 'LandCluster7', 'LandCluster8', 'LandCluster9', 'LandCluster10', 'LandCluster11',
                             'LandCluster12')

## or... ()

SST_longagg <- aggregate(long_SST$Temperature, by = list(long_SST$Date, long_SST$Cluster), mean)
colnames(SST_longagg) <- c('Date', 'SeaCluster', 'MeanTemp')

Pdat_longagg <- aggregate(long_Pdat$Precipitation, by = list(long_Pdat$Date, long_Pdat$Cluster), mean)
colnames(SST_longagg) <- c('Date', 'LandCluster', 'MeanPrecip')
