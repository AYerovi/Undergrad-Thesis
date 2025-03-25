options(stringsAsFactors = FALSE)
library(tidyverse)
library(sf)
# library(inborutils)

dir_win <- "D:/OneDrive - Escuela Politécnica Nacional/Adrian/Universitario/Thesis/02 Computations/01 Data/GADM/gadm41_ECU.gpkg"
dir_linux <- "/home/adrian/Documents/Adrian/Universitario/Thesis/02 Computations/01 Data/GADM/gadm41_ECU.gpkg"

st_layers(dir_win)

ecu <- st_read(dir_win, layer = "ADM_ADM_1")
ecu_geom <- st_geometry(ecu)

ji <- function(xy, origin=c(0,0), cellsize=c(0.5,0.5)) {
  t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
}

dim(ecu)[1]
border <- data.frame(st_coordinates(ecu_geom[[1]]))[,c(1,2)]

JI <- ji(cbind(border$X, border$Y)) # Var1 and Var2 are the default names
border$X_g <- JI[, 1]
border$Y_g <- JI[, 2]


pt <- st_sfc(st_point(c(border$X_g[1], border$Y_g[1])))
st_within(pt, ecu_geom[[1]])


border$X <- round(border$X, digits = 4)
border$Y <- round(border$Y, digits = 4)

border <- border[!duplicated(border[, c(1,2)]), ]

k <- border[!duplicated(border[, c(3,4)]),]
k$C = 100

# Note: st_within or st_contains

# library(ggmap)
# library(ggplot2)
# library(raster)
# library(maptools)
# mapa <- borders("world", regions = "Ecuador", colour = "black", size = 0.3)
# m <- ggplot() + mapa + theme_bw() + xlab("Longitude (decimals)") + ylab("Latitude (decimals)") + 
#   theme(panel.border = element_blank(), panel.grid.major =  element_blank(), panel.grid.minor = element_blank())
# ggplot(aes(x = X, y = Y, data = k))


ggplot(ecu_geom[[1]]) + geom_sf(aes(geometry = geometry)) 
