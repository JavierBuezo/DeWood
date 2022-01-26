library(ggplot2)
library(dplyr)
library(readxl) # 1
library(tidyverse)
library(fs)
library(xlsx)
library(tidyr)
library(stringi)
library(readr)

path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Final results/"
path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign 09 September/"
#path.to.data <- "C:/Users/NG.5027073/Dropbox (MNCN CSIC)/práce/Nagore uam/INVESTIGACION/2020 - DeWood Romania/EGM 4/"
#Leemos el excell con los codigos de plot y muestra, separados por fecha
setwd(path.to.data)

setwd("D:/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/Results CSV")
path.to.data <- getwd()

#Cargar todos los archivos xlsx de la carpeta
files_path <- list.files(path.to.data, full.names = T, pattern = ".csv")
#Guardar el nombre del archivo para usarlo despues como titulo
files_nm <- list.files(path.to.data, pattern = ".csv")


AllData <- list.files(path.to.data, full.names = TRUE) %>% lapply(fread) %>% 
            bind_rows()




lapply(files_nm, function(x){

  if(!exists("AllData"))
     {
       AllData <- read.csv(x)
       }
     else
     {
       temp <- read.csv(x)
       AllData <- rbind(AllData, temp)
     }

})
write.csv(AllData, "Complete_Data.csv")
