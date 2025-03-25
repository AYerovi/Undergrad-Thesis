setwd("~/Documents/Adrian/Universitario/Thesis/02 Computations/01 Data/Weather/CPC Copernicus")
library(ncdf4)
library(tidyverse)

dir <- "~/Documents/Adrian/Universitario/Thesis/02 Computations/01 Data/Weather/CPC Copernicus/"
ff_1 <- "CPC_total_precipitation_day_0.5x0.5_global_"
ff_2 <- "_v1.0.nc"
yy <- as.character(2015)
file <- paste(ff_1, yy, ff_2, sep = "")
ncin <- nc_open(file)
print(ncin)
lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")
time <- ncvar_get(ncin,"time")

lon <- as.vector(lon)
lat <- as.vector(lat)
time <- as.vector(time)

rain_array <- ncvar_get(ncin,"pr")
rain_array[1,1,]

library(rworldmap)
grid <- as.data.frame(gridCountriesDegreesHalf)
colnames(grid)[2:3] <- c("Lon", "Lat")
mat <- data.frame(Lon = lon[1], Lat = lat)
for(i in 2:length(lon)){
  temp <-  data.frame(Lon = lon[i], Lat = lat)
  mat <- rbind(mat, temp)
}
mat$N <- c(1:dim(mat)[1])
mat <- left_join(grid, mat)

test <- as.data.frame(rain_array[, , ])


