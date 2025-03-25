# Linux
setwd("/home/adrian/Documents/Adrian/Universitario/Thesis/02 Computations/02 R/Regressions")
# Mac
# setwd("/Users/adrianyerovi/Library/CloudStorage/OneDrive-EscuelaPolitécnicaNacional/Documentos/Universitario/Thesis/02 Computations/02 R/Regressions")

library(tidyverse)

# Compute GDP growth ----------------------------------------------------------
gdp0 <- read.csv("/home/adrian/Documents/Adrian/Universitario/Thesis/02 Computations/01 Data/NatureData/GDP_PPP_1990_2015.csv", 
                row.names = 1)
# Mac:
# gdp <- read.csv("/Users/adrianyerovi/Library/CloudStorage/OneDrive-EscuelaPolitécnicaNacional/Documentos/Universitario/Thesis/02 Computations/01 Data/NatureData/GDP_PPP_1990_2015.csv", 
#                 row.names = 1)


# Compute log GDP
gdp <- gdp0[, c(1:3)]
for(i in 4:29){
  gdp[,i] = log(gdp0[, i] + 1)
}
colnames(gdp) <- colnames(gdp0)

# Compute growth as the geometric mean
temp <- gdp0[, c(1:3)]
for(i in 4:28){
  temp[, i]  = ifelse(gdp0[, i] == 0 & gdp0[, i+1] != 0, 100, 
                    ifelse(gdp0[, i] == 0 & gdp0[, i+1] == 0, 0,
                           ifelse(is.na(gdp0[, i]) | is.na(gdp0[, i+1]), NA,
                                  ((gdp0[, i+1]/(gdp0[, i])) - 1 )*100
                                  )
                           )
                    )
}
colnames(temp)[4:28] <- colnames(gdp)[5:29] %>% substr(5,8)
colnames(temp)[4:28] <- paste0("G_", colnames(temp)[4:28], sep="")
growth <- temp
rm(temp)
# write.csv(growth, "growth_1990_2015.csv")

# First we have to merge all the data ------------------------------------------
# Read GDP and convert to long format

gdp <- gather(gdp, "Year", "GDP", GDP_1990:GDP_2015)
gdp$Year <- substr(gdp$Year, 5, 8) %>% as.numeric()
saveRDS(gdp, "gdp_long.rds")
# gdp <- readRDS("gdp_long.rds")

growth <- gather(growth, "Year", "Growth", G_1991:G_2015)
growth$Year <- substr(growth$Year, 3, 6) %>% as.numeric()
saveRDS(growth, "growth_long.rds")
# growth <- readRDS("growth_long.rds")

gdp_growth <- left_join(gdp, growth)
colnames(gdp_growth) <- c("UN_country", "X", "Y", "Year", "logGDP", "Growth")
saveRDS(gdp_growth, "gdp_growth_long.rds")
# gdp_growth <- readRDS("gdp_growth_long.rds")

rm(gdp, gdp0, growth, k)

# Read weather variables and convert to long format ---------------------------
weather <- readRDS("weather_data.rds")

temp_cols <- colnames(weather) %in% grep("T", colnames(weather), value = T) %>% which()
temperature <- weather[, c(1,2,3,temp_cols)]
temperature <- gather(temperature, "Year", "T", MeanT_1990:MeanT_2015)
temperature$Year <- substr(temperature$Year, 7, 10) %>% as.numeric()

precip_cols <- colnames(weather) %in% grep("P", colnames(weather), value = T) %>% which()
precipitation <- weather[, c(1,2,3,precip_cols)]
precipitation <- gather(precipitation, "Year", "P", MeanP_1990:MeanP_2015)
precipitation$Year <- substr(precipitation$Year, 7, 10) %>% as.numeric()

entropy_cols <- colnames(weather) %in% grep("D", ignore.case = FALSE, 
                                            colnames(weather), value = T) %>% which()
entropy <- weather[, c(1,2,3,entropy_cols)]
entropy <- gather(entropy, "Year", "D", D_1990:D_2015)
entropy$Year <- substr(entropy$Year, 3, 6) %>% as.numeric()

weather <- inner_join(temperature, precipitation)
weather <- inner_join(weather, entropy)
rm(temperature, precipitation, entropy, entropy_cols, precip_cols, temp_cols)

saveRDS(weather, "weather_long.rds")
# weather <- readRDS("weather_long.rds")

weather$S = weather$D*(weather$P/max(weather$P, na.rm = T))

# Merge GDP and Weather --------------------------------------------------------
colnames(weather)
colnames(gdp_growth)
colnames(weather)[c(1,2,3)] <- c("UN_country", "X", "Y")
data1 <- inner_join(gdp_growth, weather)
# saveRDS(data, "data_W_GDP_long.rds")

# Merge with the elevation data ------------------------------------------------

# Merge with the cropland data -------------------------------------------------



# data_W_GDP <- readRDS("data_W_GDP_long.rds")
library(plm)


data1$GCell <- paste0(as.character(data1$X), " ", as.character(data1$Y), sep= "")
data1$Country_Year <-  paste0(as.character(data1$UN_country), "_", as.character(data1$Year), sep= "")
data1$T2 = (data1$`T`)^2 
data1$P = data1$P/1000
data1$P2 = (data1$P)^2 

data1$Growth = data1$Growth*100

p1 <- plm(Growth ~ P + P2 + `T` + T2, data = data1, index = c("GCell", "Year", "Country_Year"))
  # same as p1_felm <- felm(Growth ~ P + P2 + `T` + T2 | GCell, data = data1)
summary(p1)

library(lfe)
p1_felm <- felm(Growth ~ P + `T` | GCell | 0 | UN_country, data = data1)
summary(p1_felm)

p2_felm <- felm(Growth ~ P + P2 + `T` + T2 | GCell | 0 | UN_country, data = data1)
summary(p2_felm)

p3_felm <- felm(Growth ~ S + `T` + T2 | GCell | 0 | UN_country, data = data1)
summary(p3_felm)

library(stargazer)
stargazer(p1_felm, p2_felm, p3_felm, type = "text")

# library(sandwich)
# library(lmtest)
# library(lfe)
# library(multiwayvcov)
# coeftest(p1, vcov=vcovHC(p1,type="HC0", cluster="group"))
# G <- length(unique(data1$UN_country))
# c <- G/(G - 1)
# coeftest(p1, c * vcovHC(p1, cluster = "group"))

