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
nc_data <- nc_open('SSTdata_011948_022018.nc')
print(nc_data)

#reading in the variables from nc
longitude<- ncvar_get(nc_data,"X")
nX<- dim(X)
head(X)

latitude<- ncvar_get(nc_data, "Y")
dim(Y)
head(Y)         

zlev<- ncvar_get(nc_data, "zlev")
dim(zlev)
head(zlev)

time<- ncvar_get(nc_data, "T")
dim(time)
head(time)

nc_data$dim$T$units
nc_data$dim$T$calendar

tas_time<- nc.get.time.series(nc_data, v="anom", time.dim.name = "T")

#


#file Pdat
Pdat<- nc_open('Pdata_011948_022018-1.nc')
print(Pdat)

time1<- ncvar_get(Pdat, "T")

Longitude1<- ncvar_get(Pdat, "X")

Latitude1<- ncvar_get(Pdat, "Y")

sst.time<- nc.get.time.series(nc_data, v="anom", time.dim.name = "time")


SST<- ncvar_get(nc_data, "anom")
dim(SST)
Precipdat <- ncvar_get(Pdat, "rain")
dim(Precipdat)

SSTLongLat <- data.frame(expand.grid(longitude, latitude))
colnames(SSTLongLat) <- c("Longitude", "Latitude")
chartime <- paste0("Months", time)

# SSTtimewide <- matrix(NA, nrow(SSTLongLat), length(chartime))
# SSTtimewide <- data.frame(SSTLatLong, SSTtimewide)

SSTtimewide <- as.data.frame.table(SST)
SSTtimewide <- pivot_wider(SSTtimewide, names_from = "Var3", values_from = "Freq")
SSTtimewide <- data.frame(SSTLongLat, SSTtimewide[, -c(1,2)])
colnames(SSTtimewide) <- c("Longitude", "Latitude", chartime)

library(raster)
library(ncdf4)

brick.dat<- brick('SSTdata_011948_022018.nc')
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

