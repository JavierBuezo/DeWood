# cargar librerias
library(stringr)
library(dplyr)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)

# # establecer el directorio de trabajo
# path.to.wd <- "C:/Users/Fomare/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/EGM_1"
# setwd(path.to.wd)
# 
# # guardar la dirección a la carpeta en la que están los archivos
# path.to.data <- "C:/Users/Fomare/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/EGM_1"
# 
# # cargar todos los archivos de la carpeta
# files_path <- list.files(path.to.data, full.names = T, pattern = ".xlsx")
# 
# resultados <- read_xlsx("C:/Users/Fomare/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/EGM_1/EGM4 Measurements_27_07_21.xlsx") 
# #resultados$plot <- resultados$egmplotcode 
# #print(head(resultados))

#group_by(resultados,egmplotcode)
#regresioneslineales2 <- lm(resultados$`CO2 Ref`~ resultados$RecNo)
regresioneslineales <- lapply(resultados, function(x) lm(resultados$`CO2 Ref`~ x))
sapply(regresioneslineales, coef)
print(regresioneslineales)

# set.seed(1)
# 
# # number of columns in the Lung and Blood data.frames. 22,000 for you?
# n <- 20 
# 
# # dummy data
# obs <- 50 # observations
# Lung <- data.frame(matrix(rnorm(obs*n), ncol=n))
# Blood <- data.frame(matrix(rnorm(obs*n), ncol=n))
# Age <- sample(20:80, obs)
# Gender  <- factor(rbinom(obs, 1, .5))
# 
# # run n regressions
# my_lms <- lapply(1:n, function(x) lm(Lung[,x] ~ Blood[,x] + Age + Gender))
# 
# # extract just coefficients
# sapply(my_lms, coef)
# 
# # if you need more info, get full summary call. now you can get whatever, like:
# summaries <- lapply(my_lms, summary)
# # ...coefficents with p values:
# lapply(summaries, function(x) x$coefficients[, c(1,4)])
# # ...or r-squared values
# sapply(summaries, function(x) c(r_sq = x$r.squared, 
#                                 adj_r_sq = x$adj.r.squared))