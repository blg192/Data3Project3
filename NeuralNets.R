

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

nc_data <- nc_open('SSTdata_011948_022018.nc')
sst_brick <- brick('SSTdata_011948_022018.nc')
sst_stack = stack(sst_brick)


Pdat <- nc_open('Pdata_011948_022018-1.nc')
pdat_brick <- brick('Pdata_011948_022018-1.nc')
pdat_stack = stack(pdat_brick)


load("data_in_dfs.RData")

# load("l_pdat_all.RData")
# load("seasons.RData")
load("precip_cat_df_wide.RData")
load("precip_cat_seas_df_wide.RData")

# long_Pdat
# load("pdat_clus.Rdata")
load("precip_region_cont_df_wide.RData")

# precip_clus_cat
# load("precip_clus_cat.Rdata")
load("precip_region_cat_df_wide.RData")





# I have to do this step because my keras is messed up otherwise
# setwd("C:/Users/Emilys/Documents")


# Data Format Conversion ##############################################


# # Code to transform data from long to wide like other precip data
# precip_cat_df_long = l_pdat_all %>%
#   select(-precip)
# 
# precip_cat_df_wide = data.frame(long = precip_cat_df_long$long[1:5390])
# precip_cat_df_wide$lat = precip_cat_df_long$lat[1:5390]
# precip_cat_df_wide[,unique(precip_cat_df_long$time)[1]] =
#   precip_cat_df_long$cat[1:5390]
# 
# temp_seq = seq(1, (nrow(precip_cat_df_long) + 5390), by = 5390)
# 
# for(i in 1:(length(temp_seq) - 1)) {
#   precip_cat_df_wide[,unique(precip_cat_df_long$time)[i]] =
#     precip_cat_df_long$cat[(temp_seq[i]):(temp_seq[i + 1] - 1)]
# }

# save(precip_cat_df_wide, file = "precip_cat_df_wide.RData")


state_map = map_data("state")
bmap = map_data("state")

# ggplot() +
#   coord_fixed(ratio = 1) + 
#   geom_raster(data = precip_cat_df_wide,
#               aes(x = long, y = lat, fill = precip_cat_df_wide[,800]),
#               alpha = 1) +
#   geom_polygon(
#     data = bmap,
#     aes(x = long, y = lat, group = group),
#     inherit.aes = F,
#     colour = 'black',
#     fill = NA,
#     lwd = 0.5
#   ) + labs(fill = "")


# # Code to convert season List to a wide data frame
# seasons_df = rbind(seasons[[1]], 
#                    seasons[[2]], 
#                    seasons[[3]], 
#                    seasons[[4]])
# 
# precip_cat_seas_df_long = seasons_df %>%
#   select(-precip, -season)
# 
# precip_cat_seas_df_long = left_join(x = precip_cat_df_long,
#                                     y = precip_cat_seas_df_long, 
#                                     by = c("lat", "long", "time"),
#                                     suffix = c("simple", "season"))
# 
# precip_cat_seas_df_long = precip_cat_seas_df_long %>% 
#   select(-catsimple)
# 
# names(precip_cat_seas_df_long)[4] = "cat"
# 
# precip_cat_seas_df_wide = data.frame(long = precip_cat_seas_df_long$long[1:5390])
# precip_cat_seas_df_wide$lat = precip_cat_seas_df_long$lat[1:5390]
# precip_cat_seas_df_wide[,unique(precip_cat_seas_df_long$time)[1]] =
#   precip_cat_seas_df_long$cat[1:5390]
# 
# temp_seq = seq(1, (nrow(precip_cat_seas_df_long) + 5390), by = 5390)
# 
# for(i in 1:(length(temp_seq) - 1)) {
#   precip_cat_seas_df_wide[,unique(precip_cat_seas_df_long$time)[i]] =
#     precip_cat_seas_df_long$cat[(temp_seq[i]):(temp_seq[i + 1] - 1)]
# }

# save(precip_cat_seas_df_wide, file = "precip_cat_seas_df_wide.RData")




# # Code to transform data from long to wide like other precip data
# precip_region_cont_df_long = long_Pdat
# 
# precip_region_cont_df_long$Date = lapply(precip_region_cont_df_long$Date, as.character)
# 
# precip_region_cont_df_wide = data.frame(long = precip_region_cont_df_long$Longitude[1:5390])
# precip_region_cont_df_wide$lat = precip_region_cont_df_long$Latitude[1:5390]
# precip_region_cont_df_wide$Cluster = precip_region_cont_df_long$Cluster[1:5390]
# precip_region_cont_df_wide[,unique(precip_region_cont_df_long$Date)[[1]]] =
#   precip_region_cont_df_long$Precipitation[1:5390]
# 
# temp_seq = seq(1, (nrow(precip_region_cont_df_long) + 5390), by = 5390)
# 
# for(i in 1:(length(temp_seq) - 1)) {
#   precip_region_cont_df_wide[,unique(precip_region_cont_df_long$Date)[[i]]] =
#     precip_region_cont_df_long$Precipitation[(temp_seq[i]):(temp_seq[i + 1] - 1)]
#   print(i)
# }
# 
# save(precip_region_cont_df_wide, file = "precip_region_cont_df_wide.RData")





# # Code to transform data from long to wide like other precip data
# precip_region_cat_df_long = precip_clus_cat
# 
# precip_region_cat_df_long$Date = lapply(precip_region_cat_df_long$Date, as.character)
# 
# precip_region_cat_df_long = precip_region_cat_df_long %>% 
#   select(-Precipitation)
# 
# precip_region_cat_df_wide = data.frame(long = precip_region_cat_df_long$Longitude[1:5390])
# precip_region_cat_df_wide$lat = precip_region_cat_df_long$Latitude[1:5390]
# precip_region_cat_df_wide$Cluster = precip_region_cat_df_long$Cluster[1:5390]
# precip_region_cat_df_wide[,unique(precip_region_cat_df_long$Date)[[1]]] =
#   precip_region_cat_df_long$cat[1:5390]
# 
# temp_seq = seq(1, (nrow(precip_region_cat_df_long) + 5390), by = 5390)
# 
# for(i in 1:(length(temp_seq) - 1)) {
#   precip_region_cat_df_wide[,unique(precip_region_cat_df_long$Date)[[i]]] =
#     precip_region_cat_df_long$cat[(temp_seq[i]):(temp_seq[i + 1] - 1)]
#   print(i)
# }
# 
# save(precip_region_cat_df_wide, file = "precip_region_cat_df_wide.RData")







# Data Exploration #####################################################



# temp_plots = list()
# for (i in 1:length(SST_df[833:844])) {
#   monthName = names(SST_df[833:844])[i]
#   
#   # Fancy plots
#   p = ggplot() +
#     coord_fixed(ratio = 1) +
#     geom_raster(data = SST_df,
#                 aes(x = long, y = lat, fill = SST_df[, monthName]),
#                 alpha = 1) +
#     scale_fill_gradientn(
#       na.value = "white",
#       # Limits need to be updated accordingly
#       limits = c(min(SST_df[, monthName]) - 0.5,
#                  max(SST_df[, monthName]) + 0.5),
#       colours = c("blue", "green", "orange", "yellow")
#     ) +
#     theme(
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       panel.border = element_rect(
#         colour = "black",
#         fill = NA,
#         size = 0.5
#       ),
#       panel.background = element_blank()
#     ) +
#     labs(fill = "Precipication \nUnits",
#          title = paste(monthName, " Precipitation"))
#   
#   temp_plots[[i]] = ggplotGrob(p)
# }
# 
# 
# gridExtra::grid.arrange(temp_plots[[9]],
#                         temp_plots[[10]],
#                         temp_plots[[11]],
#                         temp_plots[[12]])

# Missing data
which(is.na(SST_df))

# Gather rectangular area with no missing values
SST_df_rect = SST_df[which(SST_df$long < 246),]
SST_df_rect = SST_df_rect[which(SST_df_rect$long > 152),]

# # Visual check that there are no missing observations
# monthName = "Mar2017"
# ggplot() +
#   coord_fixed(ratio = 1) +
#   geom_raster(data = SST_df_rect,
#               aes(x = long, y = lat, fill = SST_df_rect[, monthName]),
#               alpha = 1) +
#   scale_fill_gradientn(
#     na.value = "white",
#     # Limits need to be updated accordingly
#     limits = c(min(SST_df[, monthName]) - 0.5,
#                max(SST_df[, monthName]) + 0.5),
#     colours = c("blue", "green", "orange", "yellow")
#   ) + labs(fill = "")

# No more missing data
which(is.na(SST_df_rect))


# Convert SST data to stacked array
SST_list_rect = list()
for (i in 3:length(names(SST_df_rect))) {
  variable = names(SST_df_rect)[i]
  SST_list_rect[[i - 2]] = acast(SST_df_rect,
                                 lat ~ long,
                                 value.var = variable,
                                 fill = 0)
}
SST_array_rect = simplify2array(SST_list_rect)


# Simple Precipitation Clusters with simple categories
Pdat_df = precip_cat_df_wide

# Simple Precipitation Clusters with seasonal categories
Pdat_df = precip_cat_seas_df_wide

# Regional Precipitation Clusters with Continuous Data
Pdat_df = precip_region_cont_df_wide
colnames(Pdat_df)[grep("Cluster", colnames(Pdat_df))] = "clust"

# Regional Precipitation Clusters with (simple) categorical Data
Pdat_df = precip_region_cat_df_wide
colnames(Pdat_df)[grep("Cluster", colnames(Pdat_df))] = "clust"


# Create basic region clusters for the temp data
# -95 long
# 38 lat
Pdat_df$clust = NA
Pdat_df$clust[which(Pdat_df$lat <= 38 & Pdat_df$long <= -95)] = "SW"
Pdat_df$clust[which(Pdat_df$lat <= 38 & Pdat_df$long > -95)] = "SE"
Pdat_df$clust[which(Pdat_df$lat > 38 & Pdat_df$long <= -95)] = "NW"
Pdat_df$clust[which(Pdat_df$lat > 38 & Pdat_df$long > -95)] = "NE"


# This line is for the continuous precip
Pdat_clust = Pdat_df %>%
  group_by(clust) %>%
  summarise_at(vars(names(Pdat_df[-grep(pattern = "long|lat|clust", 
                                        x = colnames(Pdat_df))])), 
               list(name = mean), na.rm = T)




# Create mode function: 
# https://stackoverflow.com/questions/29255473/most-frequent-value-mode-by-group
getMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# This section is for the categorical (low, normal, high) precip
Pdat_clust = data.frame(clustName = apply(Pdat_df[(Pdat_df$clust == unique(Pdat_df$clust)[1]),] %>%
                                              select(-lat, -long), 2, getMode))

for(clustIndex in 2:length(unique(Pdat_df$clust))) {
  clustName = unique(Pdat_df$clust)[clustIndex]
  Pdat_clust[, clustIndex] = data.frame(apply(Pdat_df[(Pdat_df$clust == clustName),] %>%
                                                select(-lat, -long), 2, getMode))
}
Pdat_clust = data.frame(t(Pdat_clust))
row.names(Pdat_clust) = 1:nrow(Pdat_clust)
Pdat_clust <- Pdat_clust %>%
  select(clust, everything())

# Convert groups from character to numeric
Pdat_clust[Pdat_clust == "low"] = 1
Pdat_clust[Pdat_clust == "normal"] = 2
Pdat_clust[Pdat_clust == "high"] = 3

Pdat_clust[,-1] =  sapply(Pdat_clust[,-1], as.numeric)






# Convert precip data to stacked array
precip_list_clust = list()
for (i in 2:length(colnames(Pdat_clust))) {
  variable = colnames(Pdat_clust)[i]
  precip_list_clust[[i - 1]] = acast(Pdat_clust,
                                     clust ~ 1,
                                     value.var = variable,
                                     fill = 0)
}
precip_array_clust = simplify2array(precip_list_clust)


# Get record of all month and year combinations
monthYears = names(SST_df_rect)[-c(1, 2)]


### Testing on a subset of data ############################################


model <- keras_model_sequential()

model %>%
  layer_conv_1d(
    filters = 32,
    kernel_size = 5,
    activation = "relu",
    input_shape = c(33, 46)
  ) %>%
  layer_dropout(rate = 0.25) %>%
  layer_flatten() %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 4, activation = "sigmoid")

summary(model)

model %>%
  compile(loss = 'mse',
          optimizer = "adam",
          metrics = c('accuracy'))


# Select first 24 months
sst_train <- SST_array_rect[,,1:24]
precip_train <- precip_array_clust[,,1:24]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))


sst_val <- SST_array_rect[,,25:36]
precip_val <- precip_array_clust[,,25:36]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_val = aperm(sst_val, c(3, 1, 2))
precip_val = aperm(precip_val, c(2, 1))


sst_test <- SST_array_rect[,,37:48]
precip_test <- precip_array_clust[,,37:48]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))

# train
history = model %>% fit(sst_train, precip_train, batch_size = 32, epochs = 10)
# plot(history)

min(history$metrics$loss)

# evaluate
score <- model %>% evaluate(sst_val, precip_val, batch_size = 32)

# Same as predicting data and then calculating MSE
score$loss






### Full Data Simple Regions: Continuous Temperature #######################



# Select all but 2017 and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2017|2018", x = monthYears)]
precip_train <- precip_array_clust[,,-grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))


# Select 2017 and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2017|2018", x = monthYears)]
precip_test <- precip_array_clust[,,grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))





model <- keras_model_sequential()

model %>%
  layer_conv_1d(
    filters = 32,
    kernel_size = 5,
    activation = "relu",
    input_shape = c(33, 46)
  ) %>%
  # layer_dropout(rate = 0.1) %>%
  layer_flatten() %>%
  layer_batch_normalization() %>%
  layer_dense(units = 800, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dense(units = 400, activation = "relu") %>%
  layer_batch_normalization() %>%
  # layer_dropout(rate = 0.3) %>%
  # layer_dense(units = 50, activation = "relu") %>%
  # layer_dropout(rate = 0.3) %>%
  # layer_dense(units = 25, activation = "relu") %>%
  # layer_dropout(rate = 0.3) %>%
  layer_dense(units = 4, activation = "relu")

summary(model)

model %>%
  compile(
    loss = 'mse',
    optimizer = optimizer_adam(learning_rate = 0.01,
                               decay = 0),
    metrics = c('accuracy')
  )


# train
history = model %>% fit(sst_train, precip_train, batch_size = 32, epochs = 10)
# plot(history)

min(history$metrics$loss)

# evaluate
score <- model %>% evaluate(sst_test, precip_test, batch_size = 32)

# Same as predicting data and then calculating MSE
# 0.6463123
score$loss

# The best model for this case appears to come from the information above

# Need to convert the MSE from the CNN to be relavant to the other MSEs we
#   have which are by grid point. Do so by replicating the predictions for 
#   each cluster across the full grid. Then calculate MSE

# Get predictions for each clusters
preds = model %>% predict(sst_test)

# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = monthYears[grep(pattern = "2017|2018", x = monthYears)]
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells
preds_df_join = left_join(
  x = Pdat_df[, c(monthYears[grep(pattern = "2017|2018",
                                  x = monthYears)], "clust", "lat", "long")],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)



# Calculate MSE
trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

MSE = vector()
for(i in 1:length(trueCols)) {
  MSE[i] = mean((preds_df_join[, trueCols[i]] -
                   preds_df_join[, predCols[i]]) ^ 2, na.rm = T)
}
names(MSE) = sub("\\_.*", "", trueCols)

# Some months are better than others
# All are worse than climatology forecast
MSE

# MSE for the year 2017
mean(MSE[1:12])



### Plotting ############################################



# Function to plot the difference between the cluster predictions and true values
predDifferencePlot = function(trueColumn, predColumn, monthYear) {
  ggplot() +
    coord_fixed(ratio = 1) +
    geom_raster(data = preds_df_join,
                aes(
                  x = long,
                  y = lat,
                  fill = (preds_df_join[, trueColumn] - preds_df_join[, predColumn])
                ),
                alpha = 0.5) +
    scale_fill_gradientn(
      na.value = "white",
      # Limits need to be updated accordingly
      limits = c(
        min(preds_df_join[, trueColumn] - preds_df_join[, predColumn]) - 0.5,
        max(preds_df_join[, trueColumn] - preds_df_join[, predColumn]) + 0.5
      ),
      colours = c("blue", "green", "orange", "yellow")
    ) +
    geom_polygon(
      data = bmap,
      aes(x = long, y = lat, group = group),
      inherit.aes = F,
      colour = 'black',
      fill = NA,
      lwd = 0.5
    ) + 
    labs(fill = "", 
         title = paste("True - Prediction", monthYear))
}

predDifferencePlot("Apr2017_true", "Apr2017_pred")


# View the differences in true and predicted over time
# Looks like clusters could be improved (as expected)
temp_plots = list()
for (i in 1:length(monthYears[grep(pattern = "2017", x = monthYears)])) {
  trueName = colnames(preds_df_join)[grep(pattern = paste0(month.abb[i],
                                                           "2017_true"),
                                          x = colnames(preds_df_join))]
  
  predName = colnames(preds_df_join)[grep(pattern = paste0(month.abb[i],
                                                           "2017_pred"),
                                          x = colnames(preds_df_join))]
  
  p = predDifferencePlot(trueName, predName, paste(month.abb[i], "2017"))
  
  # print(p)
  
  temp_plots[[i]] = ggplotGrob(p)
}

gridExtra::grid.arrange(temp_plots[[9]],
                        temp_plots[[10]],
                        temp_plots[[11]],
                        temp_plots[[12]])



### Full Data Simple Regions: Continuous Temperature 6 month lead ###########



# Select all but 2016, 2017, and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2016|2017|2018", x = monthYears)]
sst_train = sst_train[, , 1:(dim(sst_train)[3] - 6)]

precip_train <- precip_array_clust[,,-grep(pattern = "2016|2017|2018", x = monthYears)]

# Create 6-month lead for precip
colnames(precip_train) = monthYears[-grep(pattern = "2016|2017|2018", x = monthYears)]
colnames(precip_train) = lead(colnames(precip_train), n = 6)
precip_train = precip_train[, 1:(ncol(precip_train) - 6)]


# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))




# Select 2016, 2017, and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2016|2017|2018", x = monthYears)]
sst_test = sst_test[, , 1:(dim(sst_test)[3] - 6)]

precip_test <- precip_array_clust[,,grep(pattern = "2016|2017|2018", x = monthYears)]


# Create 6-month lead for precip
colnames(precip_test) = monthYears[grep(pattern = "2016|2017|2018", x = monthYears)]
colnames(precip_test) = lead(colnames(precip_test), n = 6)
precip_test = precip_test[, 1:(ncol(precip_test) - 6)]


# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))



model <- keras_model_sequential()

model %>%
  layer_conv_1d(
    filters = 32,
    kernel_size = 5,
    activation = "relu",
    input_shape = c(33, 46)
  ) %>%
  layer_dropout(rate = 0.1) %>%
  layer_flatten() %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 4, activation = "relu")

summary(model)

model %>%
  compile(
    loss = 'mse',
    optimizer = optimizer_adam(learning_rate = 0.01,
                               decay = 0),
    metrics = c('accuracy')
  )


# train
history = model %>% fit(sst_train, precip_train, batch_size = 32, epochs = 10)
# plot(history)

min(history$metrics$loss)

# evaluate
score <- model %>% evaluate(sst_test, precip_test, batch_size = 32)

# Same as predicting data and then calculating MSE
# 0.6031917
score$loss


# Calculate MSE



# Get predictions for each clusters
preds = model %>% predict(sst_test)

# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = rownames(precip_test)
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells

preds_df_join = left_join(
  x = Pdat_df[, c(which(names(Pdat_df) %in% c(rownames(precip_test),
                                              "clust", "lat", "long") == T))],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)


trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

MSE = vector()
for(i in 1:length(trueCols)) {
  MSE[i] = mean((preds_df_join[, trueCols[i]] -
                   preds_df_join[, predCols[i]]) ^ 2, na.rm = T)
}
names(MSE) = sub("\\_.*", "", trueCols)

# Some months are better than others
# Still not as good as climatology forecast but better than without lag
MSE

# MSE for 2017
# Persistence model gave MSE near 6.770514 for 2017 when tau = 6
mean(MSE[grep(pattern = "2017", x = names(MSE))])



# Monthly persistence calculations

# The prediction in July 2016 is the true value in January 2016

# January 2016
jan2016 = grep(pattern = "Jul2016", names(Pdat_df)) - 6

# July 2016
jul2016 = grep(pattern = "Jul2016", names(Pdat_df))

MSE_per = vector()
for(i in 0:(length(names(Pdat_df)[jul2016:(length(names(Pdat_df)) - 1)]) - 1)) {
  MSE_per[i + 1] = mean((Pdat_df[, jan2016 + i] - Pdat_df[, jul2016 + i]) ^ 2,
                        na.rm = T)
}

names(MSE_per) = names(Pdat_df)[jul2016:(length(names(Pdat_df)) - 1)]
MSE_per

# MSE 6.770514 for 2017
mean(MSE_per[grep(pattern = "2017", x = names(MSE_per))])




### SST PCs Simple Regions: Continuous Temperature #######################



# Select all but 2017 and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2017|2018", x = monthYears)]

sst_train_mat = matrix(data = sst_train, 
                       nrow = dim(sst_train)[1] * dim(sst_train)[2], 
                       ncol = dim(sst_train)[3])

sst_train_prComps = prcomp(sst_train_mat, center = FALSE, scale. = FALSE)

sst_train_prComps_array = array(sst_train_prComps$x,
                                dim = c(dim(sst_train)[1],
                                        dim(sst_train)[2],
                                        dim(sst_train)[3]))

# # Principal components
# sst_train_prComps$x[1:5,1:5]


precip_train <- precip_array_clust[,,-grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))




# Select 2017 and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2017|2018", x = monthYears)]

sst_test_mat = matrix(data = sst_test, 
                       nrow = dim(sst_test)[1] * dim(sst_test)[2], 
                       ncol = dim(sst_test)[3])

sst_test_prComps = prcomp(sst_test_mat, center = FALSE, scale. = FALSE)

sst_test_prComps_array = array(sst_test_prComps$x,
                               dim = c(dim(sst_test)[1],
                                       dim(sst_test)[2],
                                       dim(sst_test)[3]))



precip_test <- precip_array_clust[,,grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))





model <- keras_model_sequential()

model %>%
  layer_conv_1d(
    filters = 32,
    kernel_size = 5,
    activation = "relu",
    input_shape = c(33, 46)
  ) %>%
  layer_dropout(rate = 0.1) %>%
  layer_flatten() %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 4, activation = "relu")

summary(model)

model %>%
  compile(
    loss = 'mse',
    optimizer = optimizer_adam(learning_rate = 0.01,
                               decay = 0),
    metrics = c('accuracy')
  )


# train
history = model %>% fit(sst_train, precip_train, batch_size = 32, epochs = 10)
# plot(history)

min(history$metrics$loss)

# evaluate
score <- model %>% evaluate(sst_test, precip_test, batch_size = 32)

# Same as predicting data and then calculating MSE
# 0.7692011
score$loss

# The best model for this case appears to come from the information above

# Need to convert the MSE from the CNN to be relavant to the other MSEs we
#   have which are by grid point. Do so by replicating the predictions for 
#   each cluster across the full grid. Then calculate MSE

# Get predictions for each clusters
preds = model %>% predict(sst_test)

# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = monthYears[grep(pattern = "2017|2018", x = monthYears)]
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells
preds_df_join = left_join(
  x = Pdat_df[, c(monthYears[grep(pattern = "2017|2018",
                                  x = monthYears)], "clust", "lat", "long")],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)



# Calculate MSE
trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

MSE = vector()
for(i in 1:length(trueCols)) {
  MSE[i] = mean((preds_df_join[, trueCols[i]] -
                   preds_df_join[, predCols[i]]) ^ 2, na.rm = T)
}
names(MSE) = sub("\\_.*", "", trueCols)

# Some months are better than others
# All are worse than climatology forecast
MSE

# MSE for the year 2017
mean(MSE[1:12])



### Full Data Simple Regions: Basic Temp Categories #######################


accuracy <- function(pred, truth) {
  mean(drop(pred) == drop(truth))
}

# Select all but 2017 and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2017|2018", x = monthYears)]
precip_train <- precip_array_clust[,,-grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))

# Select 2017 and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2017|2018", x = monthYears)]
precip_test <- precip_array_clust[,,grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))



preds = matrix(nrow = nrow(precip_test), ncol = ncol(precip_test))


# train
Accuracy = vector()
for (i in 1:4) {
  set.seed(1)
  
  rm(model)
  
  model <- keras_model_sequential()
  
  model %>%
    layer_conv_1d(
      filters = 32,
      kernel_size = 5,
      activation = "relu",
      input_shape = c(33, 46)
    ) %>%
    # layer_dropout(rate = 0.1) %>%
    layer_flatten() %>%
    layer_batch_normalization() %>% 
    layer_dense(units = 800, activation = "relu") %>%
    layer_batch_normalization() %>%
    layer_dense(units = 400, activation = "relu") %>%
    layer_dropout(rate = 0.2) %>%
    # layer_dense(units = 50, activation = "relu") %>%
    # layer_dropout(rate = 0.3) %>%
    # layer_dense(units = 25, activation = "relu") %>%
    # layer_dropout(rate = 0.3) %>%
    layer_dense(units = 4, activation = "softmax")
  
  summary(model)
  
  model %>%
    compile(
      loss = 'sparse_categorical_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.01,
                                 decay = 0),
      metrics = c('accuracy')
    )
  
  
  history = model %>% fit(sst_train,
                          precip_train[, i],
                          batch_size = 32,
                          epochs = 10)
  # plot(history)
  
  min(history$metrics$loss)
  
  # evaluate
  score <- model %>% evaluate(sst_test, precip_test[, i], batch_size = 32)
  
  # Same as predicting data and then calculating MSE
  # 0.6463123
  score$loss
  
  # The best model for this case appears to come from the information above
  
  # Need to convert the MSE from the CNN to be relevant to the other MSEs we
  #   have which are by grid point. Do so by replicating the predictions for
  #   each cluster across the full grid. Then calculate MSE
  
  # Get predictions for each clusters
  preds[, i] = model %>%
    predict(sst_test) %>%
    k_argmax() %>%
    keras$backend$get_value()
  
  # Test Accuracy
  Accuracy[i] = preds[, i] %>%
    accuracy(precip_test[, i])
}

# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = monthYears[grep(pattern = "2017|2018", x = monthYears)]
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells
preds_df_join = left_join(
  x = Pdat_df[, c(monthYears[grep(pattern = "2017|2018",
                                  x = monthYears)], "clust", "lat", "long")],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)


preds_df_join[preds_df_join == 1] = "low"
preds_df_join[preds_df_join == 2] = "normal"
preds_df_join[preds_df_join == 3] = "high"


# Calculate Accuracy
trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

Accuracy = vector()
for(i in 1:length(trueCols)) {
  temp_table = table(preds_df_join[, trueCols[i]] , preds_df_join[, predCols[i]])
  Accuracy[i] = sum(diag(temp_table)) / sum(temp_table)
  
}
names(Accuracy) = sub("\\_.*", "", trueCols)

# Some months are better than others
# All are worse than climatology forecast
Accuracy

# MSE for the year 2017
mean(Accuracy[1:12])


### Full Data Simple Regions: Basic Categorical Temperature 6 month lead #####



# The prediction in July 2016 is the true value in January 2016

# Select all but 2016, 2017, and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2016|2017|2018", x = monthYears)]
sst_train = sst_train[, , 1:(dim(sst_train)[3] - 6)]

precip_train <- precip_array_clust[,,-grep(pattern = "2016|2017|2018", x = monthYears)]

# Create 6-month lead for precip
colnames(precip_train) = monthYears[-grep(pattern = "2016|2017|2018", x = monthYears)]
colnames(precip_train) = lead(colnames(precip_train), n = 6)
precip_train = precip_train[, 7:ncol(precip_train)]


# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))


# SST in January predicts temperature in July

# Select 2016, 2017, and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2016|2017|2018", x = monthYears)]
sst_test = sst_test[, , 1:(dim(sst_test)[3] - 6)]
# monthYears[grep(pattern = "2016|2017|2018", x = monthYears)[1:(dim(sst_test)[3] - 6)]]


precip_test <- precip_array_clust[,,grep(pattern = "2016|2017|2018", x = monthYears)]


# Create 6-month lead for precip
colnames(precip_test) = monthYears[grep(pattern = "2016|2017|2018", x = monthYears)]
colnames(precip_test) = lead(colnames(precip_test), n = 6)
precip_test = precip_test[, 1:(ncol(precip_test) - 6)]


# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))



preds = matrix(nrow = nrow(precip_test), ncol = ncol(precip_test))

# train
Accuracy = vector()
for (i in 1:4) {
  model <- keras_model_sequential()
  
  model %>%
    layer_conv_1d(
      filters = 32,
      kernel_size = 5,
      activation = "relu",
      input_shape = c(33, 46)
    ) %>%
    layer_dropout(rate = 0.1) %>%
    layer_flatten() %>%
    layer_dense(units = 100, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 50, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 25, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 4, activation = "softmax")
  
  
  summary(model)
  
  model %>%
    compile(
      loss = 'sparse_categorical_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.01,
                                 decay = 0),
      metrics = c('accuracy')
    )
  


  history = model %>% fit(sst_train,
                          precip_train[, i],
                          batch_size = 32,
                          epochs = 30)
  # plot(history)
  
  min(history$metrics$loss)
  
  # evaluate
  score <- model %>% evaluate(sst_test, precip_test[, i], batch_size = 32)
  
  # Same as predicting data and then calculating MSE
  # 0.6463123
  score$loss
  
  # The best model for this case appears to come from the information above
  
  # Need to convert the MSE from the CNN to be relevant to the other MSEs we
  #   have which are by grid point. Do so by replicating the predictions for
  #   each cluster across the full grid. Then calculate MSE
  
  # Get predictions for each clusters
  preds[, i] = model %>%
    predict(sst_test) %>%
    k_argmax() %>%
    keras$backend$get_value()
  
  # Test Accuracy
  Accuracy[i] = preds[, i] %>%
    accuracy(precip_test[, i])
}

# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = rownames(precip_test)
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells
preds_df_join = left_join(
  x = Pdat_df[, c(monthYears[grep(pattern = "2017|2018",
                                  x = monthYears)], "clust", "lat", "long")],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)


preds_df_join[preds_df_join == 1] = "low"
preds_df_join[preds_df_join == 2] = "normal"
preds_df_join[preds_df_join == 3] = "high"


# Calculate Accuracy
trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

Accuracy = vector()
for(i in 1:length(trueCols)) {
  temp_table = table(preds_df_join[, trueCols[i]] , preds_df_join[, predCols[i]])
  Accuracy[i] = sum(diag(temp_table)) / sum(temp_table)
  
}
names(Accuracy) = sub("\\_.*", "", trueCols)

# Some months are better than others
# All are worse than climatology forecast
Accuracy

# MSE for the year 2017
mean(Accuracy[1:12])



### SST PCs Simple Regions: Basic Temp Categories #######################



# Select all but 2017 and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2017|2018", x = monthYears)]

sst_train_mat = matrix(data = sst_train, 
                       nrow = dim(sst_train)[1] * dim(sst_train)[2], 
                       ncol = dim(sst_train)[3])

sst_train_prComps = prcomp(sst_train_mat, center = FALSE, scale. = FALSE)

sst_train_prComps_array = array(sst_train_prComps$x,
                                dim = c(dim(sst_train)[1],
                                        dim(sst_train)[2],
                                        dim(sst_train)[3]))

# # Principal components
# sst_train_prComps$x[1:5,1:5]


precip_train <- precip_array_clust[,,-grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))




# Select 2017 and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2017|2018", x = monthYears)]

sst_test_mat = matrix(data = sst_test, 
                      nrow = dim(sst_test)[1] * dim(sst_test)[2], 
                      ncol = dim(sst_test)[3])

sst_test_prComps = prcomp(sst_test_mat, center = FALSE, scale. = FALSE)

sst_test_prComps_array = array(sst_test_prComps$x,
                               dim = c(dim(sst_test)[1],
                                       dim(sst_test)[2],
                                       dim(sst_test)[3]))



precip_test <- precip_array_clust[,,grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))


preds = matrix(nrow = nrow(precip_test), ncol = ncol(precip_test))

# train
Accuracy = vector()
for (i in 1:4) {
  set.seed(1)
  
  rm(model)
  
  model <- keras_model_sequential()
  
  model %>%
    layer_conv_1d(
      filters = 32,
      kernel_size = 5,
      activation = "relu",
      input_shape = c(33, 46)
    ) %>%
    layer_dropout(rate = 0.1) %>%
    layer_flatten() %>%
    layer_dense(units = 100, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 50, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 25, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 4, activation = "softmax")
  
  
  summary(model)
  
  model %>%
    compile(
      loss = 'sparse_categorical_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.01,
                                 decay = 0),
      metrics = c('accuracy')
    )
  
  history = model %>% fit(sst_train,
                          precip_train[, i],
                          batch_size = 32,
                          epochs = 30)
  # plot(history)
  
  min(history$metrics$loss)
  
  # evaluate
  score <- model %>% evaluate(sst_test, precip_test[, i], batch_size = 32)
  
  # Same as predicting data and then calculating MSE
  # 0.6463123
  score$loss
  
  # The best model for this case appears to come from the information above
  
  # Need to convert the MSE from the CNN to be relevant to the other MSEs we
  #   have which are by grid point. Do so by replicating the predictions for
  #   each cluster across the full grid. Then calculate MSE
  
  # Get predictions for each clusters
  preds[, i] = model %>%
    predict(sst_test) %>%
    k_argmax() %>%
    keras$backend$get_value()
  
  # Test Accuracy
  Accuracy[i] = preds[, i] %>%
    accuracy(precip_test[, i])
}

# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = rownames(precip_test)
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells
preds_df_join = left_join(
  x = Pdat_df[, c(monthYears[grep(pattern = "2017|2018",
                                  x = monthYears)], "clust", "lat", "long")],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)


preds_df_join[preds_df_join == 1] = "low"
preds_df_join[preds_df_join == 2] = "normal"
preds_df_join[preds_df_join == 3] = "high"


# Calculate Accuracy
trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

Accuracy = vector()
for(i in 1:length(trueCols)) {
  temp_table = table(preds_df_join[, trueCols[i]] , preds_df_join[, predCols[i]])
  Accuracy[i] = sum(diag(temp_table)) / sum(temp_table)
  
}
names(Accuracy) = sub("\\_.*", "", trueCols)

# Some months are better than others
# All are worse than climatology forecast
Accuracy

# MSE for the year 2017
mean(Accuracy[1:12])


### Full Data Simple Regions: Seasonal Temp Categories #######################


accuracy <- function(pred, truth) {
  mean(drop(pred) == drop(truth))
}

# Select all but 2017 and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2017|2018", x = monthYears)]
precip_train <- precip_array_clust[,,-grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))

# Select 2017 and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2017|2018", x = monthYears)]
precip_test <- precip_array_clust[,,grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))


preds = matrix(nrow = nrow(precip_test), ncol = ncol(precip_test))

# train
Accuracy = vector()
for (i in 1:4) {
  set.seed(1)
  
  rm(model)
  
  model <- keras_model_sequential()
  
  model %>%
    layer_conv_1d(
      filters = 32,
      kernel_size = 5,
      activation = "relu",
      input_shape = c(33, 46)
    ) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.1) %>%
    layer_batch_normalization() %>%
    layer_flatten() %>%
    layer_batch_normalization() %>%
    layer_dense(units = 800, activation = "relu") %>%
    # layer_batch_normalization() %>%
    # layer_dense(units = 400, activation = "relu") %>%
    # layer_dropout(rate = 0.1) %>%
    # layer_dense(units = 20, activation = "relu") %>%
    # layer_dropout(rate = 0.2) %>%
    # layer_dense(units = 25, activation = "relu") %>%
    # layer_dropout(rate = 0.3) %>%
    layer_batch_normalization() %>%
    layer_dense(units = 4, activation = "softmax")
  
  summary(model)
  
  model %>%
    compile(
      loss = 'sparse_categorical_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.0001,
                                 decay = 0),
      metrics = c('accuracy')
    )
  
  
  counter = funModeling::freq(precip_train[, i], plot = F) %>% 
    select(var, frequency)
  majority = max(counter$frequency)
  counter$weight = ceiling(majority / counter$frequency)
  l_weights = setNames(as.list(counter$weight), counter$var)
  
  history = model %>% fit(sst_train,
                          precip_train[, i],
                          batch_size = 32,
                          epochs = 20,
                          class_weight = l_weights)
  # plot(history)
  
  min(history$metrics$loss)
  
  # evaluate
  score <- model %>% evaluate(sst_test, precip_test[, i], batch_size = 32)
  
  # Same as predicting data and then calculating MSE
  # 0.5877982
  score$loss
  
  # The best model for this case appears to come from the information above
  
  # Need to convert the MSE from the CNN to be relevant to the other MSEs we
  #   have which are by grid point. Do so by replicating the predictions for
  #   each cluster across the full grid. Then calculate MSE
  
  # Get predictions for each clusters
  preds[, i] = model %>%
    predict(sst_test) %>%
    k_argmax() %>%
    keras$backend$get_value()
  
  # Test Accuracy
  Accuracy[i] = preds[, i] %>%
    accuracy(precip_test[, i])
}

# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = monthYears[grep(pattern = "2017|2018", x = monthYears)]
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells
preds_df_join = left_join(
  x = Pdat_df[, c(monthYears[grep(pattern = "2017|2018",
                                  x = monthYears)], "clust", "lat", "long")],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)


preds_df_join[preds_df_join == 1] = "low"
preds_df_join[preds_df_join == 2] = "normal"
preds_df_join[preds_df_join == 3] = "high"


# Calculate Accuracy
trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

Accuracy = vector()
for(i in 1:length(trueCols)) {
  temp_table = table(preds_df_join[, trueCols[i]] , preds_df_join[, predCols[i]])
  Accuracy[i] = sum(diag(temp_table)) / sum(temp_table)
  
}
names(Accuracy) = sub("\\_.*", "", trueCols)

# Some months are better than others
# All are worse than climatology forecast
Accuracy

# MSE for the year 2017
mean(Accuracy[1:12])


### Full Data Simple Regions: Seasonal Categorical Temperature 6 month lead #####



# The prediction in July 2016 is the true value in January 2016

# Select all but 2016, 2017, and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2016|2017|2018", x = monthYears)]
sst_train = sst_train[, , 1:(dim(sst_train)[3] - 6)]

precip_train <- precip_array_clust[,,-grep(pattern = "2016|2017|2018", x = monthYears)]

# Create 6-month lead for precip
colnames(precip_train) = monthYears[-grep(pattern = "2016|2017|2018", x = monthYears)]
colnames(precip_train) = lead(colnames(precip_train), n = 6)
precip_train = precip_train[, 7:ncol(precip_train)]


# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))


# SST in January predicts temperature in July

# Select 2016, 2017, and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2016|2017|2018", x = monthYears)]
sst_test = sst_test[, , 1:(dim(sst_test)[3] - 6)]
# monthYears[grep(pattern = "2016|2017|2018", x = monthYears)[1:(dim(sst_test)[3] - 6)]]


precip_test <- precip_array_clust[,,grep(pattern = "2016|2017|2018", x = monthYears)]


# Create 6-month lead for precip
colnames(precip_test) = monthYears[grep(pattern = "2016|2017|2018", x = monthYears)]
colnames(precip_test) = lead(colnames(precip_test), n = 6)
precip_test = precip_test[, 1:(ncol(precip_test) - 6)]


# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))



preds = matrix(nrow = nrow(precip_test), ncol = ncol(precip_test))


# train
Accuracy = vector()
for (i in 1:4) {
  set.seed(1)
  
  rm(model)
  
  model <- keras_model_sequential()
  
  model %>%
    layer_conv_1d(
      filters = 32,
      kernel_size = 5,
      activation = "relu",
      input_shape = c(33, 46)
    ) %>%
    # layer_dropout(rate = 0.1) %>%
    layer_flatten() %>%
    layer_batch_normalization() %>% 
    # layer_dense(units = 800, activation = "relu") %>%
    # layer_batch_normalization() %>% 
    layer_dense(units = 400, activation = "relu") %>%
    layer_batch_normalization() %>% 
    # layer_dense(units = 100, activation = "relu") %>%
    # layer_dropout(rate = 0.2) %>%
    # layer_dense(units = 50, activation = "relu") %>%
    # layer_dropout(rate = 0.2) %>%
    # layer_dense(units = 25, activation = "relu") %>%
    # layer_dropout(rate = 0.2) %>%
    layer_dense(units = 4, activation = "softmax")
  
  
  summary(model)
  
  model %>%
    compile(
      loss = 'sparse_categorical_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.001,
                                 decay = 0),
      metrics = c('accuracy')
    )
  
  counter = funModeling::freq(precip_train[, i], plot = F) %>% 
    select(var, frequency)
  majority = max(counter$frequency)
  counter$weight = ceiling(majority / counter$frequency)
  l_weights = setNames(as.list(counter$weight), counter$var)
  
  history = model %>% fit(sst_train,
                          precip_train[, i],
                          batch_size = 32,
                          epochs = 10, 
                          class_weights = l_weights)
  
  # plot(history)
  
  min(history$metrics$loss)
  
  # evaluate
  score <- model %>% evaluate(sst_test, precip_test[, i], batch_size = 32)
  
  # Same as predicting data and then calculating MSE
  # 0.6463123
  score$loss
  
  # The best model for this case appears to come from the information above
  
  # Need to convert the MSE from the CNN to be relevant to the other MSEs we
  #   have which are by grid point. Do so by replicating the predictions for
  #   each cluster across the full grid. Then calculate MSE
  
  # Get predictions for each clusters
  preds[, i] = model %>%
    predict(sst_test) %>%
    k_argmax() %>%
    keras$backend$get_value()
  
  as.vector(precip_test[, i])
  
  # Test Accuracy
  Accuracy[i] = preds[, i] %>%
    accuracy(precip_test[, i])
}

# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = rownames(precip_test)
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells
preds_df_join = left_join(
  x = Pdat_df[, c(monthYears[grep(pattern = "2017|2018",
                                  x = monthYears)], "clust", "lat", "long")],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)


preds_df_join[preds_df_join == 1] = "low"
preds_df_join[preds_df_join == 2] = "normal"
preds_df_join[preds_df_join == 3] = "high"


# Calculate Accuracy
trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

Accuracy = vector()
for(i in 1:length(trueCols)) {
  temp_table = table(preds_df_join[, trueCols[i]] , preds_df_join[, predCols[i]])
  Accuracy[i] = sum(diag(temp_table)) / sum(temp_table)
  
}
names(Accuracy) = sub("\\_.*", "", trueCols)

# Some months are better than others
# All are worse than climatology forecast
Accuracy


# MSE for the year 2017
mean(Accuracy[1:12])


### SST PCs Simple Regions: Seasonal Temp Categories #######################



# Select all but 2017 and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2017|2018", x = monthYears)]

sst_train_mat = matrix(data = sst_train, 
                       nrow = dim(sst_train)[1] * dim(sst_train)[2], 
                       ncol = dim(sst_train)[3])

sst_train_prComps = prcomp(sst_train_mat, center = FALSE, scale. = FALSE)

sst_train_prComps_array = array(sst_train_prComps$x,
                                dim = c(dim(sst_train)[1],
                                        dim(sst_train)[2],
                                        dim(sst_train)[3]))

# # Principal components
# sst_train_prComps$x[1:5,1:5]


precip_train <- precip_array_clust[,,-grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))




# Select 2017 and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2017|2018", x = monthYears)]

sst_test_mat = matrix(data = sst_test, 
                      nrow = dim(sst_test)[1] * dim(sst_test)[2], 
                      ncol = dim(sst_test)[3])

sst_test_prComps = prcomp(sst_test_mat, center = FALSE, scale. = FALSE)

sst_test_prComps_array = array(sst_test_prComps$x,
                               dim = c(dim(sst_test)[1],
                                       dim(sst_test)[2],
                                       dim(sst_test)[3]))



precip_test <- precip_array_clust[,,grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))


preds = matrix(nrow = nrow(precip_test), ncol = ncol(precip_test))

# train
Accuracy = vector()
for (i in 1:4) {
  set.seed(1)
  
  rm(model)
  
  model <- keras_model_sequential()
  
  model %>%
    layer_conv_1d(
      filters = 32,
      kernel_size = 5,
      activation = "relu",
      input_shape = c(33, 46)
    ) %>%
    layer_batch_normalization() %>%
    layer_dropout(rate = 0.1) %>%
    layer_batch_normalization() %>%
    layer_flatten() %>%
    layer_batch_normalization() %>%
    # layer_dense(units = 800, activation = "relu") %>%
    # layer_batch_normalization() %>%
    layer_dense(units = 400, activation = "relu") %>%
    # layer_dropout(rate = 0.1) %>%
    # layer_dense(units = 20, activation = "relu") %>%
    # layer_dropout(rate = 0.2) %>%
    # layer_dense(units = 25, activation = "relu") %>%
    # layer_dropout(rate = 0.3) %>%
    layer_batch_normalization() %>%
    layer_dense(units = 4, activation = "softmax")
  
  
  summary(model)
  
  model %>%
    compile(
      loss = 'sparse_categorical_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.001,
                                 decay = 0),
      metrics = c('accuracy')
    )
  
  counter = funModeling::freq(precip_train[, i], plot = F) %>% 
    select(var, frequency)
  majority = max(counter$frequency)
  counter$weight = ceiling(majority / counter$frequency)
  l_weights = setNames(as.list(counter$weight), counter$var)
  
  history = model %>% fit(sst_train,
                          precip_train[, i],
                          batch_size = 32,
                          epochs = 20)
  
  min(history$metrics$loss)
  
  # evaluate
  score <- model %>% evaluate(sst_test, precip_test[, i], batch_size = 32)
  
  # Same as predicting data and then calculating MSE
  # 0.6463123
  score$loss
  
  # The best model for this case appears to come from the information above
  
  # Need to convert the MSE from the CNN to be relevant to the other MSEs we
  #   have which are by grid point. Do so by replicating the predictions for
  #   each cluster across the full grid. Then calculate MSE
  
  # Get predictions for each clusters
  preds[, i] = model %>%
    predict(sst_test) %>%
    k_argmax() %>%
    keras$backend$get_value()
  
  # Test Accuracy
  Accuracy[i] = preds[, i] %>%
    accuracy(precip_test[, i])
}

# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = monthYears[grep(pattern = "2017|2018", x = monthYears)]
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells
preds_df_join = left_join(
  x = Pdat_df[, c(monthYears[grep(pattern = "2017|2018",
                                  x = monthYears)], "clust", "lat", "long")],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)


preds_df_join[preds_df_join == 1] = "low"
preds_df_join[preds_df_join == 2] = "normal"
preds_df_join[preds_df_join == 3] = "high"


# Calculate Accuracy
trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

Accuracy = vector()
for(i in 1:length(trueCols)) {
  temp_table = table(preds_df_join[, trueCols[i]] , preds_df_join[, predCols[i]])
  Accuracy[i] = sum(diag(temp_table)) / sum(temp_table)
  
}
names(Accuracy) = sub("\\_.*", "", trueCols)

# Some months are better than others
# All are worse than climatology forecast
Accuracy

# MSE for the year 2017
mean(Accuracy[1:12])


### Full Data Formal Regions: Continuous Temperature ######################



# Select all but 2017 and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2017|2018", x = monthYears)]
precip_train <- precip_array_clust[,,-grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))


# Select 2017 and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2017|2018", x = monthYears)]
precip_test <- precip_array_clust[,,grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))





model <- keras_model_sequential()

model %>%
  layer_conv_1d(
    filters = 32,
    kernel_size = 5,
    activation = "relu",
    input_shape = c(33, 46)
  ) %>%
  # layer_dropout(rate = 0.2) %>%
  layer_flatten() %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 12, activation = "relu")

summary(model)

model %>%
  compile(
    loss = 'mse',
    optimizer = optimizer_adam(learning_rate = 0.001,
                               decay = 0),
    metrics = c('accuracy')
  )


# train
history = model %>% fit(sst_train, precip_train, batch_size = 32, epochs = 100)
# plot(history)

min(history$metrics$loss)

# evaluate
score <- model %>% evaluate(sst_test, precip_test, batch_size = 32)

# Same as predicting data and then calculating MSE
# 2.661991
score$loss

# The best model for this case appears to come from the information above

# Need to convert the MSE from the CNN to be relavant to the other MSEs we
#   have which are by grid point. Do so by replicating the predictions for 
#   each cluster across the full grid. Then calculate MSE

# Get predictions for each clusters
preds = model %>% predict(sst_test)

# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = monthYears[grep(pattern = "2017|2018", x = monthYears)]
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells
preds_df_join = left_join(
  x = Pdat_df[, c(monthYears[grep(pattern = "2017|2018",
                                  x = monthYears)], "clust", "lat", "long")],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)


# Calculate MSE
trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

MSE = vector()
for(i in 1:length(trueCols)) {
  MSE[i] = mean((preds_df_join[, trueCols[i]] -
                   preds_df_join[, predCols[i]]) ^ 2, na.rm = T)
}
names(MSE) = sub("\\_.*", "", trueCols)

# Some months are better than others
# Only better than climatology for December
MSE

# MSE for the year 2017
mean(MSE[1:12])


### Full Data Formal Regions: Continuous Temperature 6 month lead ###########



# Select all but 2016, 2017, and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2016|2017|2018", x = monthYears)]
sst_train = sst_train[, , 1:(dim(sst_train)[3] - 6)]

precip_train <- precip_array_clust[,,-grep(pattern = "2016|2017|2018", x = monthYears)]

# Create 6-month lead for precip
colnames(precip_train) = monthYears[-grep(pattern = "2016|2017|2018", x = monthYears)]
colnames(precip_train) = lead(colnames(precip_train), n = 6)
precip_train = precip_train[, 1:(ncol(precip_train) - 6)]


# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))




# Select 2016, 2017, and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2016|2017|2018", x = monthYears)]
sst_test = sst_test[, , 1:(dim(sst_test)[3] - 6)]

precip_test <- precip_array_clust[,,grep(pattern = "2016|2017|2018", x = monthYears)]


# Create 6-month lead for precip
colnames(precip_test) = monthYears[grep(pattern = "2016|2017|2018", x = monthYears)]
colnames(precip_test) = lead(colnames(precip_test), n = 6)
precip_test = precip_test[, 1:(ncol(precip_test) - 6)]


# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))



model <- keras_model_sequential()

model %>%
  layer_conv_1d(
    filters = 32,
    kernel_size = 5,
    activation = "relu",
    input_shape = c(33, 46)
  ) %>%
  # layer_dropout(rate = 0.2) %>%
  layer_flatten() %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 12, activation = "relu")

summary(model)

model %>%
  compile(
    loss = 'mse',
    optimizer = optimizer_adam(learning_rate = 0.001,
                               decay = 0),
    metrics = c('accuracy')
  )


# train
history = model %>% fit(sst_train, precip_train, batch_size = 32, epochs = 100)
# plot(history)

min(history$metrics$loss)

# evaluate
score <- model %>% evaluate(sst_test, precip_test, batch_size = 32)

# Same as predicting data and then calculating MSE
# 2.493758
score$loss


# Calculate MSE



# Get predictions for each clusters
preds = model %>% predict(sst_test)

# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = rownames(precip_test)
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells

preds_df_join = left_join(
  x = Pdat_df[, c(which(names(Pdat_df) %in% c(rownames(precip_test),
                                              "clust", "lat", "long") == T))],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)


trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

MSE = vector()
for(i in 1:length(trueCols)) {
  MSE[i] = mean((preds_df_join[, trueCols[i]] -
                   preds_df_join[, predCols[i]]) ^ 2, na.rm = T)
}
names(MSE) = sub("\\_.*", "", trueCols)

# Some months are better than others
# Only better than December Climatology
MSE

# MSE for 2017
# Persistence model gave MSE near 6.770514 for 2017 when tau = 6
mean(MSE[grep(pattern = "2017", x = names(MSE))])


# Monthly persistence calculations

# The prediction in July 2016 is the true value in January 2016

# January 2016
jan2016 = grep(pattern = "Jul2016", names(Pdat_df)) - 6

# July 2016
jul2016 = grep(pattern = "Jul2016", names(Pdat_df))

MSE_per = vector()
for(i in 0:(length(names(Pdat_df)[jul2016:(length(names(Pdat_df)) - 1)]) - 1)) {
  MSE_per[i + 1] = mean((Pdat_df[, jan2016 + i] - Pdat_df[, jul2016 + i]) ^ 2,
                        na.rm = T)
}

names(MSE_per) = names(Pdat_df)[jul2016:(length(names(Pdat_df)) - 1)]
MSE_per

# MSE 6.770514 for 2017
mean(MSE_per[grep(pattern = "2017", x = names(MSE_per))])


### SST PCs Formal Regions: Continuous Temperature #######################



# Select all but 2017 and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2017|2018", x = monthYears)]

sst_train_mat = matrix(data = sst_train, 
                       nrow = dim(sst_train)[1] * dim(sst_train)[2], 
                       ncol = dim(sst_train)[3])

sst_train_prComps = prcomp(sst_train_mat, center = FALSE, scale. = FALSE)

sst_train_prComps_array = array(sst_train_prComps$x,
                                dim = c(dim(sst_train)[1],
                                        dim(sst_train)[2],
                                        dim(sst_train)[3]))

# # Principal components
# sst_train_prComps$x[1:5,1:5]


precip_train <- precip_array_clust[,,-grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))




# Select 2017 and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2017|2018", x = monthYears)]

sst_test_mat = matrix(data = sst_test, 
                      nrow = dim(sst_test)[1] * dim(sst_test)[2], 
                      ncol = dim(sst_test)[3])

sst_test_prComps = prcomp(sst_test_mat, center = FALSE, scale. = FALSE)

sst_test_prComps_array = array(sst_test_prComps$x,
                               dim = c(dim(sst_test)[1],
                                       dim(sst_test)[2],
                                       dim(sst_test)[3]))



precip_test <- precip_array_clust[,,grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))



model <- keras_model_sequential()

model %>%
  layer_conv_1d(
    filters = 32,
    kernel_size = 5,
    activation = "relu",
    input_shape = c(33, 46)
  ) %>%
  # layer_dropout(rate = 0.2) %>%
  layer_flatten() %>%
  layer_dense(units = 100, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 12, activation = "relu")

summary(model)

model %>%
  compile(
    loss = 'mse',
    optimizer = optimizer_adam(learning_rate = 0.001,
                               decay = 0),
    metrics = c('accuracy')
  )


# train
history = model %>% fit(sst_train,
                        precip_train,
                        batch_size = 32,
                        epochs = 50)
# plot(history)

min(history$metrics$loss)

# evaluate
score <- model %>% evaluate(sst_test, precip_test, batch_size = 32)

# Same as predicting data and then calculating MSE
# 2.98115
score$loss

# The best model for this case appears to come from the information above

# Need to convert the MSE from the CNN to be relavant to the other MSEs we
#   have which are by grid point. Do so by replicating the predictions for 
#   each cluster across the full grid. Then calculate MSE

# Get predictions for each clusters
preds = model %>% predict(sst_test)

# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = monthYears[grep(pattern = "2017|2018", x = monthYears)]
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells
preds_df_join = left_join(
  x = Pdat_df[, c(monthYears[grep(pattern = "2017|2018",
                                  x = monthYears)], "clust", "lat", "long")],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)



# Calculate MSE
trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

MSE = vector()
for(i in 1:length(trueCols)) {
  MSE[i] = mean((preds_df_join[, trueCols[i]] -
                   preds_df_join[, predCols[i]]) ^ 2, na.rm = T)
}
names(MSE) = sub("\\_.*", "", trueCols)

# Some months are better than others
# All are worse than climatology forecast
MSE

# MSE for the year 2017
mean(MSE[1:12])





### Full Data Formal Regions: Basic Temp Categories #######################


accuracy <- function(pred, truth) {
  mean(drop(pred) == drop(truth))
}

# Select all but 2017 and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2017|2018", x = monthYears)]
precip_train <- precip_array_clust[,,-grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))

# Select 2017 and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2017|2018", x = monthYears)]
precip_test <- precip_array_clust[,,grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))



preds = matrix(nrow = nrow(precip_test), ncol = ncol(precip_test))


# train
Accuracy = vector()
for(i in 1:12) {
  set.seed(1)
  
  rm(model)
  
  model <- keras_model_sequential()
  
  model %>%
    layer_conv_1d(
      filters = 32,
      kernel_size = 5,
      activation = "relu",
      input_shape = c(33, 46)
    ) %>%
    # layer_dropout(rate = 0.1) %>%
    layer_flatten() %>%
    layer_batch_normalization() %>%
    layer_dense(units = 800, activation = "relu") %>%
    # layer_batch_normalization() %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 400, activation = "relu") %>%
    # layer_dropout(rate = 0.2) %>%
    # layer_dense(units = 100, activation = "relu") %>%
    # layer_dropout(rate = 0.2) %>%
    # layer_dense(units = 50, activation = "relu") %>%
    # layer_dropout(rate = 0.2) %>%
    # layer_dense(units = 25, activation = "relu") %>%
    # layer_dropout(rate = 0.2) %>%
    layer_dense(units = 12, activation = "softmax")
  
  summary(model)
  
  model %>%
    compile(
      loss = 'sparse_categorical_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.01,
                                 decay = 0.01),
      metrics = c('accuracy')
    )
  
  history = model %>% fit(sst_train,
                          precip_train[, i],
                          batch_size = 32,
                          epochs = 10)
  # plot(history)
  
  min(history$metrics$loss)
  
  # evaluate
  score <- model %>% evaluate(sst_test, precip_test[, i], batch_size = 32)
  
  # Same as predicting data and then calculating MSE
  # 0.6463123
  score$loss
  
  # The best model for this case appears to come from the information above
  
  # Need to convert the MSE from the CNN to be relevant to the other MSEs we
  #   have which are by grid point. Do so by replicating the predictions for
  #   each cluster across the full grid. Then calculate MSE
  
  # Get predictions for each clusters
  preds[, i] = model %>%
    predict(sst_test) %>%
    k_argmax() %>%
    keras$backend$get_value()
  
  # Test Accuracy
  Accuracy[i] = preds[, i] %>%
    accuracy(precip_test[, i])
}

# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = monthYears[grep(pattern = "2017|2018", x = monthYears)]
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells
preds_df_join = left_join(
  x = Pdat_df[, c(monthYears[grep(pattern = "2017|2018",
                                  x = monthYears)], "clust", "lat", "long")],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)


preds_df_join[preds_df_join == 1] = "low"
preds_df_join[preds_df_join == 2] = "normal"
preds_df_join[preds_df_join == 3] = "high"


# Calculate Accuracy
trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

Accuracy = vector()
for(i in 1:length(trueCols)) {
  temp_table = table(preds_df_join[, trueCols[i]] , preds_df_join[, predCols[i]])
  Accuracy[i] = sum(diag(temp_table)) / sum(temp_table)
}
names(Accuracy) = sub("\\_.*", "", trueCols)

# Some months are better than others
# All are worse than climatology forecast
Accuracy

# MSE for the year 2017
mean(Accuracy[1:12])




### Full Data Formal Regions: Basic Categorical Temperature 6 month lead #####



# The prediction in July 2016 is the true value in January 2016

# Select all but 2016, 2017, and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2016|2017|2018", x = monthYears)]
sst_train = sst_train[, , 1:(dim(sst_train)[3] - 6)]

precip_train <- precip_array_clust[,,-grep(pattern = "2016|2017|2018", x = monthYears)]

# Create 6-month lead for precip
colnames(precip_train) = monthYears[-grep(pattern = "2016|2017|2018", x = monthYears)]
colnames(precip_train) = lead(colnames(precip_train), n = 6)
precip_train = precip_train[, 7:ncol(precip_train)]


# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))


# SST in January predicts temperature in July

# Select 2016, 2017, and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2016|2017|2018", x = monthYears)]
sst_test = sst_test[, , 1:(dim(sst_test)[3] - 6)]
# monthYears[grep(pattern = "2016|2017|2018", x = monthYears)[1:(dim(sst_test)[3] - 6)]]


precip_test <- precip_array_clust[,,grep(pattern = "2016|2017|2018", x = monthYears)]


# Create 6-month lead for precip
colnames(precip_test) = monthYears[grep(pattern = "2016|2017|2018", x = monthYears)]
colnames(precip_test) = lead(colnames(precip_test), n = 6)
precip_test = precip_test[, 1:(ncol(precip_test) - 6)]


# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))



preds = matrix(nrow = nrow(precip_test), ncol = ncol(precip_test))


# train
Accuracy = vector()
for (i in 1:12) {
  model <- keras_model_sequential()
  
  model %>%
    layer_conv_1d(
      filters = 32,
      kernel_size = 5,
      activation = "relu",
      input_shape = c(33, 46)
    ) %>%
    layer_dropout(rate = 0.1) %>%
    layer_flatten() %>%
    layer_dense(units = 100, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 50, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 25, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 12, activation = "softmax")
  
  
  summary(model)
  
  model %>%
    compile(
      loss = 'sparse_categorical_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.01,
                                 decay = 0),
      metrics = c('accuracy')
    )
  


  history = model %>% fit(sst_train,
                          precip_train[, i],
                          batch_size = 32,
                          epochs = 30)
  # plot(history)
  
  min(history$metrics$loss)
  
  # evaluate
  score <- model %>% evaluate(sst_test, precip_test[, i], batch_size = 32)
  
  # Same as predicting data and then calculating MSE
  # 0.6463123
  score$loss
  
  # The best model for this case appears to come from the information above
  
  # Need to convert the MSE from the CNN to be relevant to the other MSEs we
  #   have which are by grid point. Do so by replicating the predictions for
  #   each cluster across the full grid. Then calculate MSE
  
  # Get predictions for each clusters
  preds[, i] = model %>%
    predict(sst_test) %>%
    k_argmax() %>%
    keras$backend$get_value()
  
  # Test Accuracy
  Accuracy[i] = preds[, i] %>%
    accuracy(precip_test[, i])
}



# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = rownames(precip_test)
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells
preds_df_join = left_join(
  x = Pdat_df[, c(monthYears[grep(pattern = "2017|2018",
                                  x = monthYears)], "clust", "lat", "long")],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)


preds_df_join[preds_df_join == 1] = "low"
preds_df_join[preds_df_join == 2] = "normal"
preds_df_join[preds_df_join == 3] = "high"


# Calculate Accuracy
trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

Accuracy = vector()
for(i in 1:length(trueCols)) {
  temp_table = table(preds_df_join[, trueCols[i]] , preds_df_join[, predCols[i]])
  Accuracy[i] = sum(diag(temp_table)) / sum(temp_table)
  
}
names(Accuracy) = sub("\\_.*", "", trueCols)

# Some months are better than others
# All are worse than climatology forecast
Accuracy

# MSE for the year 2017
mean(Accuracy[1:12])


### SST PCs Formal Regions: Basic Temp Categories #######################



# Select all but 2017 and 2018 for training
sst_train <- SST_array_rect[,,-grep(pattern = "2017|2018", x = monthYears)]

sst_train_mat = matrix(data = sst_train, 
                       nrow = dim(sst_train)[1] * dim(sst_train)[2], 
                       ncol = dim(sst_train)[3])

sst_train_prComps = prcomp(sst_train_mat, center = FALSE, scale. = FALSE)

sst_train_prComps_array = array(sst_train_prComps$x,
                                dim = c(dim(sst_train)[1],
                                        dim(sst_train)[2],
                                        dim(sst_train)[3]))

# # Principal components
# sst_train_prComps$x[1:5,1:5]


precip_train <- precip_array_clust[,,-grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_train = aperm(sst_train, c(3, 1, 2))
precip_train = aperm(precip_train, c(2, 1))




# Select 2017 and 2018 for testing
sst_test <- SST_array_rect[,,grep(pattern = "2017|2018", x = monthYears)]

sst_test_mat = matrix(data = sst_test, 
                      nrow = dim(sst_test)[1] * dim(sst_test)[2], 
                      ncol = dim(sst_test)[3])

sst_test_prComps = prcomp(sst_test_mat, center = FALSE, scale. = FALSE)

sst_test_prComps_array = array(sst_test_prComps$x,
                               dim = c(dim(sst_test)[1],
                                       dim(sst_test)[2],
                                       dim(sst_test)[3]))



precip_test <- precip_array_clust[,,grep(pattern = "2017|2018", x = monthYears)]

# Needs order of (n_images, x_shape, y_shape, channels)
sst_test = aperm(sst_test, c(3, 1, 2))
precip_test = aperm(precip_test, c(2, 1))


preds = matrix(nrow = nrow(precip_test), ncol = ncol(precip_test))


# train
Accuracy = vector()
for (i in 1:12) {
  model <- keras_model_sequential()
  
  model %>%
    layer_conv_1d(
      filters = 32,
      kernel_size = 5,
      activation = "sigmoid",
      input_shape = c(33, 46)
    ) %>%
    # layer_dropout(rate = 0.1) %>%
    layer_flatten() %>%
    layer_dense(units = 100, activation = "sigmoid") %>%
    # layer_dropout(rate = 0.3) %>%
    layer_dense(units = 50, activation = "sigmoid") %>%
    # layer_dropout(rate = 0.3) %>%
    layer_dense(units = 25, activation = "sigmoid") %>%
    # layer_dropout(rate = 0.3) %>%
    layer_dense(units = 12, activation = "sigmoid")
  
  
  summary(model)
  
  model %>%
    compile(
      loss = 'sparse_categorical_crossentropy',
      optimizer = optimizer_adam(learning_rate = 0.01,
                                 decay = 0),
      metrics = c('accuracy')
    )
  

  history = model %>% fit(sst_train,
                          precip_train[, i],
                          batch_size = 32,
                          epochs = 30)
  # plot(history)
  
  min(history$metrics$loss)
  
  # evaluate
  score <- model %>% evaluate(sst_test, precip_test[, i], batch_size = 32)
  
  # Same as predicting data and then calculating MSE
  # 0.6463123
  score$loss
  
  # The best model for this case appears to come from the information above
  
  # Need to convert the MSE from the CNN to be relevant to the other MSEs we
  #   have which are by grid point. Do so by replicating the predictions for
  #   each cluster across the full grid. Then calculate MSE
  
  # Get predictions for each clusters
  preds[, i] = model %>%
    predict(sst_test) %>%
    k_argmax() %>%
    keras$backend$get_value()
  
  # Test Accuracy
  Accuracy[i] = preds[, i] %>%
    accuracy(precip_test[, i])
}


# Convert predictions to dataframe and add cluster assignments
preds_df = data.frame(t(preds))
colnames(preds_df) = rownames(precip_test)
preds_df$clust = colnames(precip_test)

# Join true data with predicted data
# Replicates cluster results for relevant grid cells
preds_df_join = left_join(
  x = Pdat_df[, c(monthYears[grep(pattern = "2017|2018",
                                  x = monthYears)], "clust", "lat", "long")],
  y = preds_df,
  by = "clust",
  suffix = c("_true", "_pred"),
  all.y = TRUE
)


preds_df_join[preds_df_join == 1] = "low"
preds_df_join[preds_df_join == 2] = "normal"
preds_df_join[preds_df_join == 3] = "high"


# Calculate Accuracy
trueCols = names(preds_df_join)[grep(pattern = "_true", x = names(preds_df_join))]
predCols = names(preds_df_join)[grep(pattern = "_pred", x = names(preds_df_join))]

Accuracy = vector()
for(i in 1:length(trueCols)) {
  temp_table = table(preds_df_join[, trueCols[i]] , preds_df_join[, predCols[i]])
  Accuracy[i] = sum(diag(temp_table)) / sum(temp_table)
  
}
names(Accuracy) = sub("\\_.*", "", trueCols)

# Some months are better than others
# All are worse than climatology forecast
Accuracy

# MSE for the year 2017
mean(Accuracy[1:12])


### Ridge Regression ############################################



library(glmnet)

grid = 10 ^ seq(10, -2, length = 100)
ridge_mod = cv.glmnet(t(SST_df_rect[, 3:830]),
                      t(Pdat_clust[2, 2:829]),
                      alpha = 0,
                      lambda = grid)

plot(ridge_mod)

bestlam = ridge_mod$lambda.min

ridge_mod = glmnet(t(SST_df_rect[, 3:830]),
                   t(Pdat_clust[2, 2:829]),
                   alpha = 0,
                   lambda = bestlam)



ridge_pred = predict(ridge_mod,
                     s = bestlam,
                     newx = t(SST_df_rect[, 831:ncol(SST_df_rect)]))

# MSE for cluster 1 in 2017: 0.3756079
# MSE for cluster 2 in 2017: 0.200776
mean((ridge_pred[grep(pattern = "2017", x = rownames(ridge_pred))] - 
        t(Pdat_clust[2, grep(pattern = "2017", x = colnames(Pdat_clust))])) ^ 2)









