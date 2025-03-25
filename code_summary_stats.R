setwd("~/Documents/Adrian/Universitario/Thesis/R/Summary Stats")

library(tidyverse)
library(dplyr)

year_data <- read.csv("~/Documents/Adrian/Universitario/Thesis/R/Dataset construction/year_data.csv", row.names=1)

year_data$log_GDP_O = log(exp(year_data$log_GDP_PopD)*1000000+1)
year_data$log_GDP_K= log(year_data$GDP_PPP + 1)


countries <- unique(year_data$Country) %>% sort()
years <- vector()
cells <- vector()
Precip <- vector()
Precip_sd <- vector()
Temp <- vector()
Temp_sd <- vector()
GDP_K <- vector()
GDP_K_sd <- vector()
GDP_O <- vector()
GDP_O_sd <- vector()
Pop_Density <- vector()
HDI <- vector()
HDI_sd <- vector()
for(i in 1:length(countries)){
  temp <- subset(year_data, Country == countries[i])
  years <- append(years, paste(min(temp$Year), max(temp$Year), sep = "–"))
  c <- unique(temp$Z) %>% length() 
  cells <- append(cells, c)
  Precip <- append(Precip, mean(temp$Precip_mm, na.rm = T))
  Precip_sd <- append(Precip_sd, sd(temp$Precip_mm, na.rm = T))
  Temp <- append(Temp, mean(temp$Temp_Degrees, na.rm = T))
  Temp_sd <- append(Temp_sd, sd(temp$Temp_Degrees, na.rm = T))
  GDP_K <- append(GDP_K, mean(temp$log_GDP_K, na.rm = T))
  GDP_K_sd <- append(GDP_K_sd, sd(temp$log_GDP_K, na.rm = T))
  GDP_O <- append(GDP_O, mean(temp$log_GDP_O, na.rm = T))
  GDP_O_sd <- append(GDP_O_sd, sd(temp$log_GDP_O, na.rm = T))
  Pop_Density <- append(Pop_Density, mean(temp$Pop_Density, na.rm = T))
  HDI <- append(HDI, mean(temp$HDI, na.rm = T))
  HDI_sd <- append(HDI_sd, sd(temp$HDI, na.rm = T))
}
sum <- data.frame(
  "Countries" = countries,
  "years" = years,
  "cells" = cells,
  "Precip" = Precip,
  "Precip_sd" = Precip_sd,
  "Temp" = Temp,
  "Temp_sd" = Temp_sd,
  "GDP_K" = GDP_K,
  "GDP_K_sd" = GDP_K_sd,
  "GDP_O" = GDP_O,
  "GDP_O_sd" = GDP_O_sd,
  "Pop_Density" = Pop_Density,
  "HDI" = HDI,
  "HDI_sd" = HDI_sd
)
write.csv(sum, "sum_stats.csv")

length(unique(year_data$Z))
mean(year_data$Precip_mm, na.rm = T)
sd(year_data$Precip_mm, na.rm = T)
mean(year_data$Temp_Degrees, na.rm = T)
sd(year_data$Temp_Degrees, na.rm = T)
mean(year_data$log_GDP_K, na.rm = T)
sd(year_data$log_GDP_K, na.rm = T)
mean(year_data$log_GDP_O, na.rm = T)
sd(year_data$log_GDP_O, na.rm = T)
mean(year_data$Pop_Density, na.rm = T)
mean(year_data$HDI, na.rm = T)
sd(year_data$HDI, na.rm = T)

write.csv(year_data, "year_data1.csv")

library(ggplot2)
data_s <- subset(year_data, log_GDP > 0 & Precip_mm < 500)
#01
ggplot(data_s, aes(x = Precip_mm, y = log_GDP)) +
  geom_point(alpha = 0.17, shape = '.', col = '#313131') +
  stat_smooth(method = "lm", col = "red", se = F) + 
  geom_smooth(se = FALSE, method = "gam", col = "blue") +
  labs(y = "log GDP", x = "Precipitation [mm]") +
  theme(text = element_text(size = 20, family = "Times"))
#02
ggplot(data_s, aes(x = Temp_Degrees, y = log_GDP)) +
  geom_point(alpha = 0.17, shape = '.', col = '#313131') +
  stat_smooth(method = "lm", col = "red", se = F,) + 
  geom_smooth(se = FALSE, method = "gam", col = "blue") +
  labs(y = "log GDP", x = "Temperature [Centigrate degrees]")+
  theme(text = element_text(size = 20, family = "Times"))
#03
ggplot(data_s, aes(x = SI_Precip_A, y = log_GDP)) +
  geom_point(alpha = 0.17, shape = '.', col = '#313131') +
  stat_smooth(method = "lm", col = "red", se = F) + 
  geom_smooth(se = FALSE, method = "gam", col = "blue") +
  labs(y = "log GDP", x = "Seasonality Index [0-100]") +
  theme(text = element_text(size = 20, family = "Times"))
#04
ggplot(data_s, aes(x = Precip_mm, y = HDI)) +
  geom_point(alpha = 0.17, shape = '.', col = '#313131') +
  stat_smooth(method = "lm", col = "red", se = F) + 
  geom_smooth(se = FALSE, method = "gam", col = "blue") +
  labs(y = "HDI [0-100]", x = "Precipitation [mm]") +
  theme(text = element_text(size = 20, family = "Times"))

#05
ggplot(year_data, aes(x = Temp_Degrees, y = HDI)) +
  geom_point(alpha = 0.17, shape = '.', col = '#313131') +
  stat_smooth(method = "lm", col = "red", se = F) + 
  geom_smooth(se = FALSE, method = "gam", col = "blue") +
  labs(y = "HDI [0-100]", x = "Temperature [Centigrate degrees]") +
  theme(text = element_text(size = 20, family = "Times"))

#06
ggplot(year_data, aes(x = SI_Precip_A, y = HDI)) +
  geom_point(alpha = 0.17, shape = '.', col = '#313131') +
  stat_smooth(method = "lm", col = "red", se = F) + 
  geom_smooth(se = FALSE, method = "gam", col = "blue") +
  labs(y = "HDI [0-100]", x = "Seasonality Index [0-100]") +
  theme(text = element_text(size = 20, family = "Times"))

#07
year_data$log_Pop_Density = log(year_data$Pop_Density)
ggplot(data_s, aes(x = Precip_mm, y = log_Pop_Density)) +
  geom_point(alpha = 0.17, shape = '.', col = '#313131') +
  stat_smooth(method = "lm", col = "red", se = F) + 
  geom_smooth(se = FALSE, method = "gam", col = "blue") +
  labs(y = "log Population Density", x = "Precipitation [mm]") +
  theme(text = element_text(size = 20, family = "Times"))

#08
ggplot(data_s, aes(x = Temp_Degrees, y = log_Pop_Density)) +
  geom_point(alpha = 0.17, shape = '.', col = '#313131') +
  stat_smooth(method = "lm", col = "red", se = F) + 
  geom_smooth(se = FALSE, method = "gam", col = "blue") +
  labs(y = "log Population Density", x = "Temperature [Centigrate degrees]") +
  theme(text = element_text(size = 20, family = "Times"))

#09
ggplot(data_s, aes(x = SI_Precip_A, y = log_Pop_Density)) +
  geom_point(alpha = 0.17, shape = '.', col = '#313131') +
  stat_smooth(method = "lm", col = "red", se = F) + 
  geom_smooth(se = FALSE, method = "gam", col = "blue") +
  labs(y = "log Population Density", x = "Seasonality Index [0-100]") +
  theme(text = element_text(size = 20, family = "Times"))





