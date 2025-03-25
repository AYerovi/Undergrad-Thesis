setwd("/home/adrian/Documents/Adrian/Universitario/Thesis/Computations/Data/Elevation")
library(ncdf4)
library(tidyverse)
e <- "/home/adrian/Documents/Adrian/Universitario/Thesis/Computations/Data/Elevation/GMTED2010_15n120_0500deg.nc"
ncin <- nc_open(e)
print(ncin)
lon <- ncvar_get(ncin,"nlon")
lon_array <- ncvar_get(ncin,"longitude")
lat_array <- ncvar_get(ncin,"latitude")
lon <- as.vector(lon_array)
lat <- as.vector(lat_array)
elevation_array <- ncvar_get(ncin,"elevation")
colnames(elevation_array) <- lat
rownames(elevation_array) <- lon
elevation_array <- as_tibble(elevation_array)
elevation_array$x = lon
elevation <- gather(elevation_array, "y", "elevation", -x)
elevation$x <- as.numeric(elevation$x)
elevation$y <- as.numeric(elevation$y)

grid <- as.data.frame(gridCountriesDegreesHalf)
colnames(grid)[c(2,3)] <- c("x", "y")

elevation <- left_join(grid, elevation)

write.csv(elevation, "elevation.csv")
