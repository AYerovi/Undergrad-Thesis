setwd("~/Documents/Adrian/Universitario/Thesis/R/Plots")

# TESTING ----------------------------------------------------------------------------

# From https://github.com/nacmarino/Maps/blob/master/Scripts/Latin%20America%20Map.R
# Environments and map data
library(ggmap)
library(ggplot2)
library(raster)
library(maptools)

latam <- c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
           "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", 
           "Nicaragua", "Honduras", "El Salvador", "Belize", "Guatemala", "Mexico", "Trinidad and Tobago",
           "Caribe", "Puerto Rico", "Dominican Republic", "Haiti", "Jamaica", "Cuba", "Bahamas", "Antiles",
           "Dominica", "Saba")

mapa <- borders("world", regions = latam, colour = "black", size = 0.3)

m <- ggplot() + mapa + theme_bw() + xlab("Longitude (decimals)") + ylab("Latitude (decimals)") + 
  xlim(-120, 75) + ylim(-60, 40) + 
  theme(panel.border = element_blank(), panel.grid.major =  element_blank(), panel.grid.minor = element_blank())
m

ggplot(aes(x = X, y = Y, fill = Temp_Degrees), data = LATAM_weather_2017) + geom_tile() + mapa +
  xlim(-120, 75) + ylim(-60, 40) +
  theme(panel.border = element_blank(), panel.grid.major =  element_blank(), panel.grid.minor = element_blank())

m <- ggplot(aes(x = X, y = Y, fill = Temp_Degrees), data = LATAM_weather_2017) + 
  geom_tile() + mapa + xlab("Longitude (decimals)") + ylab("Latitude (decimals)") +
  labs(colour = "Temperature (Degrees)") +
  theme(panel.border = element_blank(), 
        panel.grid.major =  element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=12, family="Times")) # To see available fonts run names(pdfFonts())
ggsave(m, filename="test.png",  width=7.27, height=6.27, dpi=300)
rm(LATAM_weather_2017, m, latam)

# FIGURES ------------------------------------------------------------------------

# Mean precipitation and temperature per gridcell
data <- read.csv("~/Documents/Adrian/Universitario/Thesis/R/Dataset construction/Pop. & Weather/LATAM_weather&pop.csv", row.names=1)
grids <- read.csv("~/Documents/Adrian/Universitario/Thesis/R/Dataset construction/grids.csv", row.names=1)
library(tidyverse)
library(dplyr)
library(ggplot2)

Mean_Var <- grids # Creating a duplicate
g <- unique(grids$Z) # Find the elements in common
g1 <- unique(data$Z)
g2 <- intersect(g, g1)
Mean_Var <- grids[grids$Z %in% g2,] 
Mean_Var <- Mean_Var[!duplicated(Mean_Var$Z),] # Finding and removing duplicates
grids <- Mean_Var[!duplicated(grids$Z),]

# Finding mean precip. and temp. along the 2000 - 2017 period for each cell
Mean_Var$Mean_Precip_mm = 0
Mean_Var$SD_Precip_mm = 0
Mean_Var$Mean_Temp_Degrees = 0
Mean_Var$SD_Temp_Degrees = 0
Mean_Var$log_GDP_K = 0
Mean_Var$log_GDP_O = 0
for (j in 1:dim(Mean_Var)[1]){ # For each gridcell, create a subset and compute the mean, sd
    temp <- subset(year_data1, Z == Mean_Var$Z[j])
    Mean_Var$Mean_Precip_mm[j] = mean(temp$Precip_mm) # mean
    Mean_Var$Mean_Temp_Degrees[j] = mean(temp$Temp_Degrees)
    Mean_Var$SD_Precip_mm[j] = sd(temp$Precip_mm)   # sd
    Mean_Var$SD_Temp_Degrees[j] = sd(temp$Temp_Degrees)
    Mean_Var$log_GDP_K[j] = mean(temp$log_GDP_K, na.rm = T)   # gdp
    Mean_Var$log_GDP_O[j] = mean(temp$log_GDP_O, na.rm = T)
    rm(temp)
} # Takes a while
Mean_Var$log_GDP_O = ifelse(is.na(Mean_Var$log_GDP_O), 0, Mean_Var$log_GDP_O)

# Mean
Mean_temp_fig <- ggplot(aes(x = X, y = Y, fill = Mean_Temp_Degrees), data = Mean_Var) +
  labs(fill = "Mean temp. (°C)") +
  geom_tile() + mapa + xlab("Longitude (decimals)") + ylab("Latitude (decimals)") +
  theme(panel.border = element_blank(), 
        panel.grid.major =  element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=15, family="Times")) + # To see available fonts run names(pdfFonts())
  scale_fill_gradient(low = "yellow", high = "red")

ggsave(Mean_temp_fig, filename="Mean_temp_fig.png",  width=7.27, height=6.27, dpi=300)

Mean_precip_fig <- ggplot(aes(x = X, y = Y, fill = Mean_Precip_mm), data = Mean_Var) +
  labs(fill = "Mean precip. (mm)") +
  geom_tile() + mapa + xlab("Longitude (decimals)") + ylab("Latitude (decimals)") +
  theme(panel.border = element_blank(), 
        panel.grid.major =  element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=15, family="Times")) + # To see available fonts run names(pdfFonts())
  scale_fill_gradient(low = "#cad6eb", high = "#002c70")
  
ggsave(Mean_precip_fig, filename="Mean_precip_fig.png",  width=7.27, height=6.27, dpi=300)

# SD
Sd_temp_fig <- ggplot(aes(x = X, y = Y, fill = SD_Temp_Degrees), data = Mean_Var) +
  labs(fill = "Temp. SD (°C)") +
  geom_tile() + mapa + xlab("Longitude (decimals)") + ylab("Latitude (decimals)") +
  theme(panel.border = element_blank(), 
        panel.grid.major =  element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=15, family="Times")) + # To see available fonts run names(pdfFonts())
  scale_fill_gradient(low = "white", high = "red")

ggsave(Sd_temp_fig, filename="Sd_temp_fig.png",  width=7.27, height=6.27, dpi=300)


Sd_precip_fig <- ggplot(aes(x = X, y = Y, fill = SD_Precip_mm), data = Mean_Var) +
  labs(fill = "Precip. SD (mm)") +
  geom_tile() + mapa + xlab("Longitude (decimals)") + ylab("Latitude (decimals)") +
  theme(panel.border = element_blank(), 
        panel.grid.major =  element_blank(), 
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        text=element_text(size=15, family="Times")) + # To see available fonts run names(pdfFonts())
  scale_fill_gradient(low = "white", high = "blue")

ggsave(Sd_precip_fig, filename="Sd_precip_fig.png",  width=7.27, height=6.27, dpi=300)

#GDP
gdp_k_fig <- ggplot(aes(x = X, y = Y, fill = log_GDP_K), data = Mean_Var) +
  labs(fill = "log(GDP)") +
  geom_tile() + mapa + xlab("Longitude (decimals)") + ylab("Latitude (decimals)") +
  theme(panel.border = element_blank(), 
        panel.grid.major =  element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=15, family="Times")) + # To see available fonts run names(pdfFonts())
  scale_fill_gradient(low = "white", high = "#079400")
ggsave(gdp_k_fig, filename="gdp_k_fig.png",  width=7.27, height=7.27, dpi=300)

gdp_o_fig <- ggplot(aes(x = X, y = Y, fill = log_GDP_O), data = Mean_Var) +
  labs(fill = "log(GDP)") +
  geom_tile() + mapa + xlab("Longitude (decimals)") + ylab("Latitude (decimals)") +
  theme(panel.border = element_blank(), 
        panel.grid.major =  element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=15, family="Times")) + # To see available fonts run names(pdfFonts())
  scale_fill_gradient(low = "white", high = "#079400")
ggsave(gdp_o_fig, filename="gdp_o_fig.png",  width=7.27, height=7.27, dpi=300)





