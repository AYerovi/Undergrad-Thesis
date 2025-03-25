setwd("~/Documents/Adrian/Universitario/Thesis/02 Computations/01 Data/NatureData/Kummu_et_al_data")
library(ncdf4)
library(tidyverse)
library(geodata)

kummu <- nc_open("GDP_PPP_1990_2015_5arcmin_v2.nc")
kummu

gdp_array <- ncvar_get(kummu,"GDP_PPP")

ji <- function(xy, origin=c(0,0), cellsize=c(0.5,0.5)) {
  t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
}

# First create a dataframe with the longitude-latitude pairs in two separate cols.
lon <- ncvar_get(kummu,"longitude")
lat <- ncvar_get(kummu,"latitude")
time <- ncvar_get(kummu,"time") %>% as.vector()
lonlat <- as.matrix(expand.grid(lon,lat))

# The the loop
for(i in 1:26){
  
  # GDP data for year i: 
  gdp_slice <- gdp_array[,,i]
  gdp_vec <- as.vector(gdp_slice)
  df <- data.frame(cbind(lonlat, gdp_vec))
  
  # Create a grid at the 5-deg level
  JI <- ji(cbind(df$Var1, df$Var2)) # Var1 and Var2 are the default names
  df$X <- JI[, 1]  # X for longitude 
  df$Y <- JI[, 2]  # Y for latitude
  
  # Aggregate (sum) by lon-lat pairs
  df$Cell <- paste(df$X, df$Y)  # Pair identifier (a string)
  sum_d <- by(df, df$Cell, 
              function(d) c(d$X[1], d$Y[1], d$Cell[1], sum(d$gdp_vec, na.rm = T))) 
  sum_d.m <- t(matrix(unlist(sum_d), nrow=4))
  colnames(sum_d.m) <- c("X", "Y", "Z", "GDP")
  write.csv(as.data.frame(sum_d.m), 
            paste("GDP_PPP_", as.character(time[i]), ".csv", sep = "" ))
  rm(gdp_slice, gdp_vec, df, JI, sum_d, sum_d.m)
  
} #Takes ~30-35 mins.


kummu <- nc_open("HDI_1990_2015_v2.nc")
hdi_array <- ncvar_get(kummu,"HDI")

for(i in 1:26){
  
  hdi_slice <- hdi_array[,,i]
  hdi_vec <- as.vector(hdi_slice)
  df <- data.frame(cbind(lonlat, hdi_vec))
  
  JI <- ji(cbind(df$Var1, df$Var2))
  df$X <- JI[, 1]
  df$Y <- JI[, 2]
  
  df$Cell <- paste(df$X, df$Y)
  sum_d <- by(df, df$Cell, 
              function(d) c(d$X[1], d$Y[1], d$Cell[1], mean(d$hdi_vec, na.rm = T)))
  sum_d.m <- t(matrix(unlist(sum_d), nrow=4)) %>% as.data.frame()
  colnames(sum_d.m) <- c("X", "Y", "Z", "HDI")
  sum_d.m$HDI <- ifelse(sum_d.m$HDI == "NaN", NA, sum_d.m$HDI)
  # NaN values correspond to places with no population (0/0)
  sum_d.m$HDI <- sum_d.m$HDI %>% as.numeric()
  write.csv(as.data.frame(sum_d.m), 
            paste("HDI_", as.character(time[i]), ".csv", sep = "" ))
  rm(hdi_slice, hdi_vec, df, JI, sum_d, sum_d.m)
  
} #Takes ~30-35 mins.

rm(lonlat)

gc()

# Merging all GDP obs. in a single table
library(rworldmap)
data(gridCountriesDegreesHalf)
grid <- as.data.frame(gridCountriesDegreesHalf)
colnames(grid) <- c("UN_country", "X", "Y")
write.csv(grid, "grid.csv")
GDP_PPP_1990_2015 <- grid
temp <- read.csv("GDP_PPP_1990.csv", row.names = 1)
temp <- subset(temp, select = -Z)
colnames(temp)[3] <-"GDP_1990"
GDP_PPP_1990_2015 <- left_join(GDP_PPP_1990_2015, temp)

for(i in 1991:2015){
  temp <- read.csv(paste("GDP_PPP_", as.character(i), ".csv", sep=""), row.names = 1)
  temp <- subset(temp, select = -Z)
  colnames(temp)[3] <- paste("GDP_", as.character(i), sep="")
  GDP_PPP_1990_2015 <- left_join(GDP_PPP_1990_2015, temp)
  rm(temp)
}

write.csv(GDP_PPP_1990_2015, "GDP_PPP_1990_2015.csv")

# Merging all HDI obs. in a single table
temp <- read.csv("HDI_1990.csv", row.names = 1)
temp <- subset(temp, select = -Z)
colnames(temp)[3] <- "HDI_1990"
HDI_1990_2015 <- grid
HDI_1990_2015 <- left_join(HDI_1990_2015, temp)

for(i in 1991:2015){
  temp <- read.csv(paste("HDI_", as.character(i), ".csv", sep=""), row.names = 1)
  temp <- subset(temp, select = -Z)
  colnames(temp)[3] <- paste("HDI_", as.character(i), sep="")
  HDI_1990_2015 <- left_join(HDI_1990_2015, temp)
  rm(temp)
}

write.csv(HDI_1990_2015, "HDI_1990_2015.csv")

# The first GDP differences
D_GDP_PPP_1990_2015 <- GDP_PPP_1990_2015[, c(1:3)]
D_GDP_PPP_1990_2015[, c(4:29)] <- 0
cols_D <- colnames(GDP_PPP_1990_2015)[4:29]
cols_D <- paste0("D_", cols_D, sep= "")
colnames(D_GDP_PPP_1990_2015)[4:29] <- cols_D

for(j in 5:dim(D_GDP_PPP_1990_2015)[2]){
  D_GDP_PPP_1990_2015[, j] = 
    ifelse(is.na(GDP_PPP_1990_2015[, j]) | is.na(GDP_PPP_1990_2015[, j-1]), 
           NA, GDP_PPP_1990_2015[, j] - GDP_PPP_1990_2015[, j-1])
}

write.csv(D_GDP_PPP_1990_2015, "D_GDP_PPP_1990_2015.csv")

# The first HDI differences
D_HDI_1990_2015 <- HDI_1990_2015[, c(1:3)]
D_HDI_1990_2015[, c(4:29)] <- 0
cols_D <- colnames(HDI_1990_2015)[4:29]
cols_D <- paste0("D_", cols_D, sep= "")
colnames(D_HDI_1990_2015)[4:29] <- cols_D

for(j in 5:dim(D_HDI_1990_2015)[2]){
  D_HDI_1990_2015[, j] = 
    ifelse(is.na(HDI_1990_2015[, j]) | is.na(HDI_1990_2015[, j-1]), 
           NA, HDI_1990_2015[, j] - HDI_1990_2015[, j-1])
}

write.csv(D_HDI_1990_2015, "D_HDI_1990_2015.csv")


library(rworldmap)
data(gridCountriesDegreesHalf)
grid <- as.data.frame(gridCountriesDegreesHalf)
k <- unique(grid$ISO_N3)
path <- "~/Documents/Adrian/Universitario/Thesis/02 Computations/01 Data/NatureData/Kummu_et_al_data"
gad <- gadm("ECU", level = 2, path = path, verison = "latest")
ECU <- readRDS("~/Documents/Adrian/Universitario/Thesis/02 Computations/01 Data/NatureData/Kummu_et_al_data/gadm/gadm41_ECU_1_pk.rds")
test <- as.data.frame(ECU)
ECU$NL_NAME_1




kummu <- nc_open("admin_areas_GDP_HDI.nc")
gdp_array <- ncvar_get(kummu,"admin_units")
lon <- ncvar_get(kummu,"longitude")
lat <- ncvar_get(kummu,"latitude")
#time <- ncvar_get(kummu,"time") %>% as.vector()
lonlat <- as.matrix(expand.grid(lon,lat))

gdp_slice <- gdp_array[,,1]
gdp_vec <- as.vector(gdp_slice)
df <- data.frame(cbind(lonlat, gdp_vec))
k <- unique(df$gdp_vec)
k

k <- as.character(k)
t <- k[which(grepl("218", k))]
k[which(k == "218")]

