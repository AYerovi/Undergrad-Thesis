setwd("/home/adrian/Documents/Adrian/Universitario/Thesis/02 Computations/01 Data/Cropland Data")
library(raster)
library(tiff)
library(tidyverse)

## Rainfed crops --------------------------------------------------------------

name1 <- "ESACCI-LC-L4-LCCS-Map-300m-P1Y-"
name2 <- "-v2.0.7.tif"
for(i in 1992:2015){
  name <- paste(name1, as.character(i), name2, sep = "")
  data <- raster(name)
  
  print(Sys.time())
  print(paste0("started calc ", as.character(i), sep=""))
  data <- calc(data, fun = function(x){x==10}) # Takes ~ 4 min
  print(paste0("done calc ", as.character(i), sep=""))
  print(Sys.time())
  print(paste0("started aggregate ", as.character(i), sep="")) 
  data <- raster::aggregate(data, fact = 180, fun = mean, na.rm = T) # Takes ~ 30 mins.
  print(paste0("done aggregate ", as.character(i), sep=""))
  print(Sys.time())
  print(paste0("started writing ", as.character(i), sep=""))
  writeRaster(data, filename = paste("5_degree_Rainfed", as.character(i), ".tif", sep=""), 
              format = "GTiff", overwrite = T)
  print(paste0("done writing ", as.character(i), sep=""))
  print(Sys.time())
  
  rm(data)
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
}


## Irrigated Cropland ----------------------------------------------------------

name1 <- "ESACCI-LC-L4-LCCS-Map-300m-P1Y-"
name2 <- "-v2.0.7.tif"
for(i in 2006:2015){
  name <- paste(name1, as.character(i), name2, sep = "")
  data <- raster(name)
  
  print(Sys.time())
  print(paste0("started calc ", as.character(i), sep=""))
  data <- calc(data, fun = function(x){x==20}) # Takes ~ 4 min
  print(paste0("done calc ", as.character(i), sep=""))
  print(Sys.time())
  print(paste0("started aggregate ", as.character(i), sep="")) 
  data <- raster::aggregate(data, fact = 180, fun = mean, na.rm = T) # Takes ~ 30 mins.
  print(paste0("done aggregate ", as.character(i), sep=""))
  print(Sys.time())
  print(paste0("started writing ", as.character(i), sep=""))
  writeRaster(data, filename = paste("5_degree_Irrigated", as.character(i), ".tif", sep=""), 
              format = "GTiff", overwrite = T)
  print(paste0("done writing ", as.character(i), sep=""))
  print(Sys.time())
  
  rm(data)
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
}

## Urban Areas ---------------------------------------------------------------


name1 <- "ESACCI-LC-L4-LCCS-Map-300m-P1Y-"
name2 <- "-v2.0.7.tif"
for(i in 2011:2015){  #1992
  name <- paste(name1, as.character(i), name2, sep = "")
  data <- raster(name)
  
  print(Sys.time())
  print(paste0("started calc ", as.character(i), sep=""))
  data <- calc(data, fun = function(x){x==190}) # Takes ~ 4 min
  print(paste0("done calc ", as.character(i), sep=""))
  print(Sys.time())
  print(paste0("started aggregate ", as.character(i), sep="")) 
  data <- raster::aggregate(data, fact = 180, fun = mean, na.rm = T) # Takes ~ 30 mins.
  print(paste0("done aggregate ", as.character(i), sep=""))
  print(Sys.time())
  print(paste0("started writing ", as.character(i), sep=""))
  writeRaster(data, filename = paste("5_degree_Urban", as.character(i), ".tif", sep=""), 
              format = "GTiff", overwrite = T)
  print(paste0("done writing ", as.character(i), sep=""))
  print(Sys.time())
  
  rm(data)
  unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE)
}


## Joining first the cropland data in a single dataset ------------------------

# JI function and grids for reference 
library(rworldmap)
data("gridCountriesDegreesHalf")
grids <- as.data.frame(gridCountriesDegreesHalf)
colnames(grids)[2:3] <- c("x", "y") 

ji <- function(xy, origin=c(0,0), cellsize=c(0.5,0.5)) {
  t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
}

# Rainfed 
data <- grids
for(i in 1992:2015){
  name <- paste("5_degree_Rainfed", as.character(i), ".tif", sep = "")
  temp <- raster(name)
  temp <- as.data.frame(temp, xy = T)
  colnames(temp) <- c("x", "y", 
                      paste("rainfed_crops", as.character(i), sep = "") )
  JI <- ji(cbind(temp$x, temp$y))
  temp$X <- JI[, 1]
  temp$Y <- JI[, 2]
  if(sum(temp$x != temp$X) != 0 & sum(temp$y != temp$Y) != 0){
    print("grid coordinates did not coincide")
    break
  }
  temp <- temp[, c(1:3)]
  data <- left_join(data, temp)
}
dataRainfed <- left_join(grids, data)
dataRainfed <- gather(dataRainfed, "Year", "Percent_Rainfed", 
                      rainfed_crops1992:rainfed_crops2015)
dataRainfed$Year <- substr(dataRainfed$Year, 14, 17)
dataRainfed$Year <- as.numeric(as.character(dataRainfed$Year))
write_rds(dataRainfed, "Rainfed_data.rds")

# Irrigated 
data <- grids
for(i in 1992:2015){
  name <- paste("5_degree_Irrigated", as.character(i), ".tif", sep = "")
  temp <- raster(name)
  temp <- as.data.frame(temp, xy = T)
  colnames(temp) <- c("x", "y", 
                      paste("irrigated_crops", as.character(i), sep = "") )
  JI <- ji(cbind(temp$x, temp$y))
  temp$X <- JI[, 1]
  temp$Y <- JI[, 2]
  if(sum(temp$x != temp$X) != 0 & sum(temp$y != temp$Y) != 0){
    print("grid coordinates did not coincide")
    break
  }
  temp <- temp[, c(1:3)]
  data <- left_join(data, temp)
}
dataIrrigated <- left_join(grids, data)
dataIrrigated <- gather(dataIrrigated, "Year", "Percent_Irrigated", irrigated_crops1992:irrigated_crops2015)
dataIrrigated$Year <- substr(dataIrrigated$Year, 16, 19)
dataIrrigated$Year <- as.numeric(as.character(dataIrrigated$Year))
write_rds(dataIrrigated, "Irrigated_data.rds")

#Urban areas
data <- grids
for(i in 1992:2015){
  name <- paste("5_degree_Urban", as.character(i), ".tif", sep = "")
  temp <- raster(name)
  temp <- as.data.frame(temp, xy = T)
  colnames(temp) <- c("x", "y", 
                      paste("urban_area", as.character(i), sep = "") )
  JI <- ji(cbind(temp$x, temp$y))
  temp$X <- JI[, 1]
  temp$Y <- JI[, 2]
  if(sum(temp$x != temp$X) != 0 & sum(temp$y != temp$Y) != 0){
    print("grid coordinates did not coincide")
    break
  }
  temp <- temp[, c(1:3)]
  data <- left_join(data, temp)
}
dataUrban <- left_join(grids, data)
dataUrban <- gather(dataUrban, "Year", "Percent_Urban", urban_area1992:urban_area2015)
dataUrban$Year <- substr(dataUrban$Year, 11, 14)
dataUrban$Year <- as.numeric(as.character(dataUrban$Year))
write_rds(dataUrban, "Urban_data.rds")

cropland_data <- left_join(dataRainfed, dataIrrigated)
cropland_data <- left_join(cropland_data, dataUrban)
write_rds(cropland_data, "Cropland_data.rds")
