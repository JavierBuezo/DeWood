library(ggplot2)
library(dplyr)
library(readxl) # 1
library(tidyverse)
library(fs)
library(xlsx)
library(tidyr)
library(naniar)
library(data.table)

##################################### This file combines the measurements of the TMS with the Plot they are in#####################################



path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/"
path.to.data <- "D:/OneDrive - UPNA/DeWood Project/"
path.to.sensors <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/TMS4/"
path.to.sensors <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign October 2021/TMS4/Data/"
path.to.sensors <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign December/TMS4/Data/"
path.to.sensors <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign December/TMS4/Data/"

#path.to.data <- "C:/Users/NG.5027073/Dropbox (MNCN CSIC)/pr?ce/Nagore uam/INVESTIGACION/2020 - DeWood Romania/EGM 4/"
#Leemos el excell con los codigos de plot y muestra, separados por fecha
setwd(path.to.data)
Sensor_table <- read_xlsx("Tabla_sensores.xlsx",sheet = "TMS")

files_nm <- list.files(path.to.sensors, pattern = ".csv")

colnames(Sensor_table)[names(Sensor_table) == "Numero"] <- "sensornumber"
setwd(path.to.sensors)
lapply(files_nm, function(x){
  
  #Separar el nombre del archivo para recoger el nombre y n?mero del sensor 
  sensorcode <- str_split_fixed(as.character(x),"_",4)
  lectura <- fread(x,header = FALSE)
  lectura$Code_sens <- sensorcode[2]
  lectura$sensornumber <- substr(sensorcode[3],2,3)
  lectura$sensornumber <- as.numeric(as.character(lectura$sensornumber))
  final_df <- left_join(lectura,Sensor_table, by="sensornumber")
  
  write.csv(final_df,paste(x,"With_Plot.csv"))
})
  
