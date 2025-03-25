setwd("~/Documents/Adrian/Universitario/Thesis/02 Computations/01 Data/Weather/Delaware")
library(tidyverse)
library(rworldmap)
library(writexl)

# Reference grid ---------------------------------------------------------------

data(gridCountriesDegreesHalf)
grid <- as.data.frame(gridCountriesDegreesHalf)
regions <- as.data.frame(countryRegions)

colnames(grid)[2:3] <- c("x", "y") 

dir_t <- "~/Documents/Adrian/Universitario/Thesis/02 Computations/01 Data/Weather/Delaware/Willmott_Mutsuura_Data/Temp (world, 1900-2017)/air_temp."
dir_p <- "~/Documents/Adrian/Universitario/Thesis/02 Computations/01 Data/Weather/Delaware/Willmott_Mutsuura_Data/Precip (world, 1900-2017)/precip."

temperature <- grid
precipitation <- grid

# This is to have a reference table with each ISO Numeric code and Name
k <- unique(grid$ISO_N3)
order(k)
ISO_codes <- data.frame(ISO_N3 = k, Name = c(1:length(k))) 
for(i in 1:dim(ISO_codes)[1]){
  ISO_codes$Name[i] = isoToName(ISO_codes$ISO_N3[i])
}
write_xlsx(ISO_codes, "ISO_codes.xlsx")

# Yearly precipitation and temperature calculations ----------------------------

for(i in 1900:2015){
  
  yy_t <- paste(dir_t, as.character(i), sep = "")
  yy_p <- paste(dir_p, as.character(i), sep = "")
  
  # Temperature
  t <- read.table(paste(yy_t), quote="\"", comment.char="") # read data
  colnames(t)[1:2] <- c("x", "y")
  t <- left_join(grid, t)
  t <- t[,c(1:3, 16)] # Only keep the average yearly Temp.
  colnames(t)[4] <- paste("MeanT", "_", as.character(i), sep="") # Colname: MeanT_<year>
  
  # Precipitation
  p <- read.table(paste(yy_p), quote="\"", comment.char="") # read data
  colnames(p)[1:2] <- c("x", "y")
  p <- left_join(grid, p)
  # Compute the Seasonality Index w/ the precip. data
  p$D = 0           # Initialize entropy vector
  colnames(p)[which(colnames(p) %in% c("V15"))] <- "MeanP" # Change the name
  for(j in 4:15){   # Compute entropy
    p$p_hat = p[, j]/(p$MeanP)
    k = which(p$p_hat == 0)
    p_zero <- p[c(k), ]
    p_nzero <- p[-c(k),]
    p_zero$D = p_zero$D + 0           # Continuity argument from Cover & Thomas
    p_nzero$D = p_nzero$D + p_nzero$p_hat*log2(12*p_nzero$p_hat) 
    p <- rbind(p_zero, p_nzero)
    k = which(colnames(p) %in% c("p_hat"))
    p <- p[, -k]
    rm(p_zero, p_nzero, k)
  }

  p <- p[,c(1:3, 16:17)] # Only keep the needed
  
  colnames(p)[4] <-  paste("MeanP", "_", as.character(i), sep="") # Colname: MeanP_<year>
  colnames(p)[5] <-  paste("D", "_", as.character(i), sep="")  # Colname: D_<year>
  
  # Join into a single dataframe
  temperature <- left_join(temperature, t)
  precipitation <- left_join(precipitation, p)
  rm(p,t)
  
} # end of loop. Takes ~5 min.

precipitation$MeanPLR = 0
precipitation$SdPLR = 0
precipitation$DLR = 0
for(i in 1:dim(precipitation)[1]){
  
  # LR precipitation mean and SD
  k <- which(colnames(precipitation) %in% 
               grep("^MeanP_", colnames(precipitation), value = T))
  p <- precipitation[i, c(k)] %>% as.numeric()
  if(sum(is.na(p)) == length(p)){
    precipitation$MeanPLR[i] = NA
  } else {
    precipitation$MeanPLR[i] <- p %>% mean(na.rm = T)
  }
  precipitation$SdPLR[i] <- p %>% sd(na.rm = T) 
  
  # LR entropy
  k <- which(colnames(precipitation) %in% 
               grep("^D_", colnames(precipitation), value = T))
  p <- precipitation[i, c(k)] %>% as.numeric()
  if(sum(is.na(p)) == length(p)){
    precipitation$DLR[i] = NA
  } else {
    precipitation$DLR[i] <- p %>% mean(na.rm = T)
  }
  precipitation$SdDLR[i] <- p %>% sd(na.rm = T) 

  rm(k, p)
  
} # Takes ~4 min

temperature$MeanTLR = 0
temperature$SdTLR = 0
for(i in 1:dim(temperature)[1]){
  
  # LR temperature mean and SD
  k <- colnames(temperature) %in% 
               grep("^MeanT_", colnames(temperature), value = T) %>%
               which()
  t <- temperature[i, c(k)] %>% as.numeric()
  if(sum(is.na(t)) == length(t)){
    temperature$MeanTLR[i] = NA
  } else {
    temperature$MeanTLR[i] <- t %>% mean(na.rm = T)
  }
  temperature$SdTLR[i] <- t %>% sd(na.rm = T) 
  
  rm(k, t)
  
} # Takes ~2 min

# Save
saveRDS(temperature, "complete_temperature_data.rds")
saveRDS(precipitation, "complete_precipitation_data.rds")
# temperature <- readRDS("complete_temperature_data.rds")
# precipitation <- readRDS("complete_precipitation_data.rds")
gc()

## Subsetting the data ---------------------------------------------------------

yy <- c(1990:2015) %>% as.character()
k <- colnames(precipitation) %in% 
          grep(paste(yy, collapse="|"),  # regular expression
               colnames(precipitation), value=TRUE) %>% 
          which()
p <- precipitation[, c(1:3, k, 236:239)] 
                  # keep country, lon, lat, LR means, LR sd
k <- colnames(temperature) %in% 
  grep(paste(yy, collapse="|"), colnames(temperature), value=TRUE) %>% 
  which()
t <- temperature[, c(1:3, k, 120, 121)]
                  # keep country, lon, lat, LR mean, LR sd
weather_data <- inner_join(p, t)
saveRDS(weather_data, "weather_data.rds")
# weather_data <- readRDS("weather_data.rds")
rm(t, p, k)

# Long run seasonality by month ------------------------------------------------

dir_t <- "~/Documents/Adrian/Universitario/Thesis/02 Computations/01 Data/Weather/Delaware/Willmott_Mutsuura_Data/Temp (world, 1900-2017)/air_temp."
dir_p <- "~/Documents/Adrian/Universitario/Thesis/02 Computations/01 Data/Weather/Delaware/Willmott_Mutsuura_Data/Precip (world, 1900-2017)/precip."

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", 
                       "Nov", "Dec")

## Monthly LR Distribution of rain ---------------------------------------------

yy_p <- paste(dir_p, "1900", sep = "")
p <- read.table(paste(yy_p), quote="\"", comment.char="") # read data
colnames(p) <- c("x", "y", c(months), "MeanP")
for(j in 3:14){   # Compute the LR monthly distrib.
  p[, j] = p[, j]/p$MeanP 
}
p <- p[, -15]

for(j in 1901:1989){
  
  temp <- read.table(paste(yy_p), quote="\"", comment.char="") # read data
  colnames(temp) <- c("x", "y", c(months), "MeanP")
  for(j in 3:14){   # Compute the LR monthly distrib.
    temp[, j] = temp[, j]/temp$MeanP 
  }
  temp <- temp[, -15]
  p <- rbind(p, temp)
  rm(temp)
  
}

LR_S <- aggregate(cbind(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) ~ x + y,
                   data=p, FUN=mean)

LR_S$SUM = 0
k = dim(LR_S)[2] - 1
for(i in 3:k){
  LR_S$SUM = LR_S$SUM + LR_S[, i]
} # The sum is verified to equal 1
rm(k)
LR_S <- LR_S[, -15]

write_rds(LR_S, "LR_monthly_P.rds")
rm(p)
gc()

ag <- aggregate(LR_S, )

## Hellinger distance  ------------------------------------------------

precipitation_d <- grid

for(i in 1990:2015){
  
  yy_p <- paste(dir_p, as.character(i), sep = "")

  # Precipitation
  p <- read.table(paste(yy_p), quote="\"", comment.char="") # read data
  colnames(p)[1:2] <- c("x", "y")
  p <- left_join(grid, p)
  p <- left_join(p, LR_S)
  for(j in 4:15){ # The distribution
    p[, j] = p[, j]/p[, 16]
  }
  p$s = 0           # Initialize vector
  colnames(p)[which(colnames(p) %in% c("V15"))] <- "MeanP" # Change the name
  for(j in 4:15){   # Compute the inner sum
    p$s = p$s + (sqrt(p[,j]) - sqrt(p[, j+13]))^2
  }
  p$H = sqrt(p$s)*(2)^(-1/2)
  p <- p[,c(1:3, 30)] # Only keep the needed
  
  colnames(p)[4] <-  paste("H", "_", as.character(i), sep="")  # Colname: D_<year>
  
  # Join into a single dataframe
  precipitation_d <- left_join(precipitation_d, p)
  rm(p)
  
} # end of loop. Takes ~5 min.

write_rds(precipitation_d, "H_dist_1990-2015.rds")

