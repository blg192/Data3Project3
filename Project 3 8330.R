#install.packages("ncdf4")
#install.packages("raster")
#install.packages("rgdal")
#install.packages("ggplot2")
#install.packages("ncdf4.helpers")
#install.packages("PCICt")
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(ncdf4.helpers)
library(PCICt)
library(lubridate)
library(dplyr)

# Set working directory to the one containing this file
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

nc_data <- nc_open('SSTdata_011948_022018.nc')
print(nc_data)

#reading in the variables from nc
longitude<- ncvar_get(nc_data,"X")
nX<- dim(longitude)
head(longitude)

latitude<- ncvar_get(nc_data, "Y")
dim(latitude)
head(latitude)         

zlev<- ncvar_get(nc_data, "zlev")
dim(zlev)
head(zlev)

time<- ncvar_get(nc_data, "T")
dim(time)
head(time)

nc_data$dim$T$units
nc_data$dim$T$calendar

# tas_time<- nc.get.time.series(nc_data, v="anom", time.dim.name = "T")

#


#file Pdat
Pdat<- nc_open('Pdata_011948_022018-1.nc')
print(Pdat)

time1<- ncvar_get(Pdat, "T")

Longitude1<- ncvar_get(Pdat, "X")

Latitude1<- ncvar_get(Pdat, "Y")

# sst.time<- nc.get.time.series(nc_data, v="anom", time.dim.name = "time")


SST <- ncvar_get(nc_data, "anom")
dim(SST)

library(raster)
library(ncdf4)

brick.dat <- brick('SSTdata_011948_022018.nc')
brick.dat
plot(brick.dat)

#sea surface temp and precip same month 
#previous month 
#response is precipitation 
# PCA ON SEA surface images
#dimension reduction
#prediction tools for each pixel 
#precipitation is regional nearby pixels are gonna be similar 
#cluster pixels ahead of time
#preserve the spatial structure 
#contemporaneous versus lag relationship 
#NOAh does these forecasts in precipitation in categories 
#continuous 


# Converting Pdat data to data frames #######################################

Pdat_dates_raw = as.Date('1960-01-01') %m+% months(floor(time))

Pdat_dates = paste0(month(ymd(Pdat_dates_raw), label = TRUE), year(Pdat_dates_raw))

pdat_brick <- brick('Pdata_011948_022018-1.nc')
plot(pdat_brick[[1]])

Pdat_df = data.frame()

Pdat_df = as.data.frame(pdat_brick[[1]], xy = TRUE)

# Takes around 10 seconds to run
for (i in 2:nlayers(pdat_brick)) {
  Pdat_df[, names(pdat_brick[[i]])] = values(pdat_brick[[i]])
}

names(Pdat_df) = c("long", "lat", Pdat_dates)

# # Code to convert back from data frame to plottable raster
# Pdat_raster <- rasterFromXYZ(Pdat_df[, c("long", "lat", "Jan2000")])
# plot(Pdat_raster)



# Converting SST data to data frames #######################################

SST_dates_raw = as.Date('1960-01-01') %m+% months(floor(time1))

SST_dates = paste0(month(ymd(SST_dates_raw), label = TRUE), year(SST_dates_raw))

plot(brick.dat[[1]])

SST_df = data.frame()

SST_df = as.data.frame(brick.dat[[1]], xy = TRUE)

# Takes about 10 seconds to run
for (i in 2:nlayers(brick.dat)) {
  SST_df[, names(brick.dat[[i]])] = values(brick.dat[[i]])
}

names(SST_df) = c("long", "lat", SST_dates)

# # Code to convert back from data frame to plottable raster
# SST_raster <- rasterFromXYZ(SST_df[, c("long", "lat", "Jan2000")])
# plot(SST_raster)


# save(Pdat_df, SST_df, file = "data_in_dfs.RData")

# To load the RData
load("data_in_dfs.RData")


# Climatology Predictions (average of all previous months) ##############

# All months through 2016
Pdat_df_train = Pdat_df[,1:grep(pattern = "Dec2016", x = names(Pdat_df))]

# All months in 2017 only
Pdat_df_test = Pdat_df[, c(grep(pattern = "2017", x = names(Pdat_df)))]
Pdat_df_test = cbind(Pdat_df[,c("long", "lat")], Pdat_df_test)

# Predict using climatology method (average of all previous months)
Pdat_df_preds = data.frame(Pdat_df_train[,c("long", "lat")])
for(i in month.abb) {
  Pdat_df_preds[, paste0(i, "2017pred")] =
    rowMeans(Pdat_df_train[, grep(pattern = i,
                                  x = names(Pdat_df_train))])
}

# MSE from climatology predictions (average of all previous months)
MSE_clim = vector()
for(i in month.abb) {
  MSE_clim[i] = colMeans(((Pdat_df_test[grep(pattern = i,
                                             x = names(Pdat_df_test))] -
                             Pdat_df_preds[, grep(pattern = i,
                                                  x = names(Pdat_df_preds))]) ^ 2),
                         na.rm = TRUE)
}

# Plot of the January predictions
Jan2017Pred_raster <- rasterFromXYZ(Pdat_df_preds[, c("long", "lat", "Jan2017pred")])
plot(Jan2017Pred_raster)

# Plot of the January true values
Jan2017True_raster <- rasterFromXYZ(Pdat_df_test[, c("long", "lat", "Jan2017")])
plot(Jan2017True_raster)




library(maps)
library(fields)
state_map = map_data("state")
bmap = map_data("state")


# Fancy plots
ggplot() +
  coord_fixed(ratio = 1) +
  geom_raster(data = Pdat_df_preds,
              aes(x = long, y = lat, fill = May2017pred),
              alpha = 1) +
  geom_polygon(
    data = bmap,
    aes(x = long, y = lat, group = group),
    inherit.aes = F,
    colour = 'black',
    fill = NA,
    lwd = 0.5
  ) +
  scale_fill_gradientn(
    na.value = "white",
    # Limits need to be updated accordingly
    limits = c(min(Pdat_df_preds$May2017pred) - 0.5, 
               max(Pdat_df_preds$May2017pred) + 0.5),
    colours = c("blue", "green", "orange", "yellow")
  )+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black",
                                fill = NA,
                                size = 0.5),
    panel.background = element_blank()
  ) + 
  labs(fill = "Precipication \nUnits", 
       title = "May 2017 Predicted Precipitation")




# Persistence Predictions (pred_(t + tau) = pred_t) ##############


# Need to define how far out we want tau to be
# Project document suggests 6 months?

## I based this on the following tutorial:
# https://machinelearningmastery.com/persistence-time-series-forecasting-with-python/


## Step 1
## transform univariate dataset into supervised learning problem
## load the data set and create a lagged representation
## i.e. given the observation at t predict the observation at t + tau
predprecip <- function(df, tau){
  ## df is dataframe
  ## tau is distance from prediction point
  mat <- as.matrix(df)
  predprecip <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat) + tau)
  colnames(predprecip) <- c(colnames(mat), c(paste("x", 1:tau, sep = "")))
  for(i in 1:ncol(mat)){
    if(i < 3){
      predprecip[, i] <- mat[, i]
    } else{
      ttau <- i + tau
      predprecip[, ttau] <- mat[, i]
    }
  }
  preddf <- as.data.frame(predprecip)
  return(preddf)
}

## predicted precipitations when tau = 6 months
preddf <- predprecip(Pdat_df, 6)

## Step 2
## establish train and test datasets for test harness
## This isn't necessary, but if we want to add it we can

## Step 3
## Define persistence model
## This step is also unnecessary because we can extract it from step 1

## Step 4
## Make forecast and establish baseline performance
library(tidyr)
testmse <- function(Pdat_df, preddf){
  longdatorig <- gather(Pdat_df, time, precip, 3:ncol(Pdat_df))
  longdatpred <- gather(preddf, time, precip, 3:ncol(preddf))
  
  y <- data.frame("ytest" = longdatorig$precip, "yhat" = longdatpred[1:nrow(longdatorig), ]$precip)
  y <- na.omit(y)
  testmse <- mean((y$yhat - y$ytest)^2)
  return(testmse)
}


## It seems MSE increases as tau approaches the 6 months mark and decreases 
## as it approaches the 12 month mark
n <- 24
msemat <- matrix(c(1:n, rep(NA, n)), nrow = n, ncol = 2, byrow = FALSE)
colnames(msemat) <- c("tau", "mse")
for(i in 1:n){
  preddf <- predprecip(Pdat_df, i)
  msemat[i, 2] <- testmse(Pdat_df, preddf)
}

ggplot(data=as.data.frame(msemat), aes(x=tau, y=mse, group=1)) +
  geom_line()+
  geom_point() +
  xlab("tau: Number of Months Ahead of Prediction") +
  ylab("Test MSE") +
  labs(title = "Test MSE for Varying Values of tau")
