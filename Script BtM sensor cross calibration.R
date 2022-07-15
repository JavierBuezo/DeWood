library(ggpubr)
library(plyr)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(readxl)
library(data.table)
library(dplyr)
library(reshape2)
library(drc)
library(ggplot2)
library(zoo)


finalfiltered <- fread("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/TerosAndNagoSensors_byHour.csv")

finalfiltered <- read.csv("C:/Users/NG.5027073/Downloads/TerosAndNagoSensors_byHour.csv")

# transform data. This is the standard procedure we use to transform the data. We usually smooth the data a little bit
# but I have removed this part for now (commented lines). The steps are:
# 1.- transform conductance to impedance (inverse function)
# 2.- scale betwen 0 and 1
# 3.- smooth the data (removed step) moving averages every 10 or so data.

prepare_scaled_conductance_data <- function(impedance_data, window_size) {
  # transform to Ohm
  avg_dat <- 1/impedance_data
  # range scale
  avg_dat <- (avg_dat-min(avg_dat, na.rm = T))/(max(avg_dat, na.rm = T)-min(avg_dat, na.rm = T))
  # # smooth 
  # avg_dat <- rollapplyr(avg_dat, window_size, mean, fill = NA)
  return(avg_dat)
}

split <- split(finalfiltered, f = as.factor(finalfiltered$sample_code))

scaled_list <- lapply(split, function(dt) {
  imp <- dt$avgWC
  prepare_scaled_conductance_data(imp, 10)})

finalfiltered$avgWC_scaled <- (unlist(scaled_list))

# plot data
ggplot(finalfiltered, aes(avgWC, avgTerosWC))+
  geom_point() +
  theme_bw()

# plot scaled data
ggplot(finalfiltered, aes(avgWC_scaled, avgTerosWC))+
         geom_point() +
  theme_bw()


# function to plot nicely the important parameters of a regression in a plot
# from here https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


# fit a regression line
lm <- lm(avgTerosWC ~ avgWC_scaled, data = finalfiltered)
# tranform the data to WC values. Note that now the units are the same as in Teros, I think it is m3/m3
ggplotRegression(lm)
summary(lm)

finalfiltered$avgWC_transformed <- predict(lm)

predict(lm)
coefficients(lm)
summary(lm)

# check result: now both sensors are in the same scale.

ggplot(finalfiltered, aes(avgWC_transformed, avgTerosWC)) +
  geom_point() +
  theme_bw()


#Use the calibration for the original file
nagosensors_all <- fread("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Respiration Campaing April 2022/April NagoSensors/Data/NagoreSensors_ALL YEAR.csv")

#Scale original data with the function above
scaled_list <- split(nagosensors_all, f = as.factor(nagosensors_all$Sensor_code))
split <- split(nagosensors_all, f = as.factor(nagosensors_all$Sensor_code))
scaled_list <- lapply(split, function(dt) {
  imp <- dt$impedance
  prepare_scaled_conductance_data(imp, 10)})

nagosensors_all$avgWC_scaled <- (unlist(scaled_list))

#Use predict with the lm above
nagosensors_all$avgWC_transformed <- predict(lm,newdata = nagosensors_all)


#Write the csv
write.csv(nagosensors_all,"Nagosensors_all_WithPredicted.csv",row.names = FALSE)
getwd()
#ANALISIS
#####################
#read the csv 
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

nagosensors_all <- fread("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV/Nagosensors_all_WithPredicted.csv")
nagosensors_all <- nagosensors_all[,c("Date.Time","sample_code","Code","DiamClass","Plot","SubPlot","Class","Species","Type","avgWC_transformed")]
nagosensors_all$Type <- replace_na(nagosensors_all$Type,replace = "Ground")
nagosensors_all$isBryo <- NA
nagosensors_all$isBryo <- substrRight(nagosensors_all$Code,1)
nagosensors_all <- nagosensors_all %>%mutate(Type2=case_when(isBryo=="b" ~ paste(Type,"b",sep=""),
                                          isBryo!="b" ~ Type))
nagosensors_all <- nagosensors_all %>% mutate(isBryo = case_when(isBryo=="b" ~ "b",
                                                                 isBryo!="b" ~ "C" ))

nagosensors_avg <- nagosensors_all %>% dplyr::group_by(Date.Time,Type2) %>%mutate(avgWCMean=mean(avgWC_transformed)) %>% ungroup
nagosensors_avg$avgWC_transformed <- NULL
#Preparar dataset. Media cada hora
nagosensors_avg$Date.Time <- mdy_hms(nagosensors_avg$Date.Time)
nagosensors_avg$Date.Time <- as.POSIXct(nagosensors_avg$Date.Time,origin= "1984-01-01")
nagosensors.day <-nagosensors_avg %>% 
  mutate(day = floor_date(Date.Time,"week"))

nagosensors.day <- nagosensors.day %>% dplyr::group_by(day,Type2) %>%mutate(avgWCMean=mean(avgWCMean)) %>% ungroup

nagosensors.day$Date.Time <- NULL
nagosensors.day <- unique(nagosensors.day)
nagosensors.day <- filter(nagosensors.day,nagosensors.day$DiamClass=="10")
abies <- filter(nagosensors.day,nagosensors.day$Species == "AA"&nagosensors.day$DiamClass=="10"|nagosensors.day$Type=="Ground")

library(tidyr)
# fagus <- filter(nagosensors_all,nagosensors_all$Species=="FS")
ggplot(data=nagosensors.day,aes(x=day,y=avgWCMean,group=Type2))+
  geom_line(aes(color=isBryo))+
  facet_wrap(~Class+Species)

nagosensors.day <- filter(nagosensors.day,nagosensors.day$DiamClass=="10"|nagosensors.day$Type=="Ground")
ggplot(data=nagosensors.day,aes(x=day,y=avgWCMean,group=Type))+
  geom_line(aes(color=Type))


