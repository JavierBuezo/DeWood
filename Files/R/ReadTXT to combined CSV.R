# libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)
library(fs)
library(openxlsx)
library(tidyr)
library(dplyr)
library(openxlsx)
library(naniar)
library(data.table)
path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign December/EGM_2/11 12 2021/"
path.to.data <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign December/EGM_2/08 12 2021/"
#path.to.data <- "C:/Users/NG.5027073/Dropbox (MNCN CSIC)/práce/Nagore uam/INVESTIGACION/2020 - DeWood Romania/EGM 4/"
#Leemos el excell con los codigos de plot y muestra, separados por fecha
setwd(path.to.data)

#Cargar todos los archivos xlsx de la carpeta
files_path <- list.files(path.to.data, full.names = T, pattern = ".txt")
#Guardar el nombre del archivo para usarlo despues como titulo
files_nm <- list.files(path.to.data, pattern = ".TXT")


#Leer todos los archivos, hay que forzar el número de columnas porque el EGM-5 registra todo como la putisima mierda
txt_files_df <- lapply(files_nm, function(x) {read.table(file = x, sep =",",fill = TRUE,col.names = paste("Column",1:22))})
# Combinarlos
combined_df <- do.call("rbind", lapply(txt_files_df, as.data.frame)) 

#Escribir xlsx

write.xlsx(combined_df,"Lecturas.xlsx")
#write.table(combined_df,"Lecturas.csv",sep = ",")
