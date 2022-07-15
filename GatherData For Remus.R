library(ggplot2)
library(dplyr)
library(readxl)
library(openxlsx)
library(tidyverse)
library(fs)
library(xlsx)
library(tidyr)
library(lubridate)
library(naniar)
library(data.table)
library(bayesbio)
library(anytime)
library(plyr)
#path.to.data <- "C:/Users/NG.5027073/Dropbox (MNCN CSIC)/práce/Nagore uam/INVESTIGACION/2020 - DeWood Romania/EGM 4/"

######################### Important Readme  ##############################
#The EGMs are configured with the Rumanian Time Zone, while the TMS are configured by default with the time zone of the last computer used 
#to download de data. Normally, the Spanish Time Zone. Thus, this should be corrected everytime before Joining the data. Also take care
#with the winter/summer hourtime

TimeCorrectionTMS <- 1 * 60 * 60 #The Time correction must be in seconds

#Leemos el excell con los codigos de plot y muestra, separados por fecha
path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/"
path.to.data <- "D:/OneDrive - UPNA/DeWood Project"
setwd(path.to.data)

#Cargar todos los archivos xlsx de la carpeta donde estan las medidas
sensores_path <- paste(getwd(),"/TMS4/All days with plots/",sep = "") #TMS July
sensores_path <- paste(getwd(),"/Respiration Campaign October 2021/TMS4/All days with plots/",sep = "")
sensores_path <- paste(getwd(),"/Respiration Campaign December/TMS4/All days with plots/",sep = "")

setwd(sensores_path)
files_nm <- list.files(sensores_path, pattern = ".csv")
temp <- lapply(files_nm,function (x){
  print(paste("Leyendo",x))
  readsensor <- fread(x,dec = ",")
  
})
# lectura_sensores <- rbindlist(temp,fill = TRUE)
lectura_sensores <- ldply(temp,data.frame)
#Formatear el dataframe
colnames(lectura_sensores)
lectura_sensores$V1 <- NULL
lectura_sensores$V10 <- NULL
lectura_sensores$V11 <- NULL
colnames(lectura_sensores) <- c("ReadIndex", "DateTime", "TimeZone", "T1","T2","T3","Soil_moist","Shake","Err_flag","Code_sens", "Sensornumber","Serial","Plot","Subplot")
ncol(lectura_sensores)

tabla_datos <- read.xlsx("C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/Remus TFM July/July.xlsx")
tabla_datos <- read.xlsx("C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign 09 September/LM Results/September.xlsx")
tabla_datos <- read.xlsx("C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign October 2021/Remus TFM/October.xlsx")
tabla_datos <- read.xlsx("C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign December/Remus TFM/December.xlsx")


year <- "2021"

#Crear la columna con la fecha en formato correcto para la busqueda despues
tabla_datos$Date <- paste(year,tabla_datos$Month,tabla_datos$Day, sep = "-")
tabla_datos$Time <- paste(tabla_datos$Hour,tabla_datos$Min,"00",sep = ":")
tabla_datos$DateTime <- paste(tabla_datos$Date,tabla_datos$Time)
tabla_datos$DateTime <- ymd_hms(tabla_datos$DateTime)

tabla_datos$Loc <- "6_1"
lectura_sensores$Loc <- paste(lectura_sensores$Plot,lectura_sensores$Subplot,sep="_")

vct <- unique(tabla_datos$Loc)

# A veces los sensores se descargan con una columna distinta al resto, en tal caso buscarlo y extraerlo para tratarlo aparte
# sensordefect <- ymd_hms(lectura_sensores$DateTime[lectura_sensores$Loc == "1_2"])

lectura_sensores$DateTime <- ymd_hm(lectura_sensores$DateTime)
lectura_sensores$DateTime <- lectura_sensores$DateTime + TimeCorrectionTMS

# lectura_sensores$DateTime[lectura_sensores$Loc=="1_2"] <- sensordefect
# 
# 
# lectura_sensores$DateTime <- lectura_sensores$DateTime + 3 * 60 * 60 #RECORDEMOS QUE LOS TMS ESTÁN 3 HORAS POR DETRÁS DE LOS EGM's
# #######################   Representar valores de los TMS    ######################

# establecer la fecha de comienzo de toma de datos
startdate <- "2021-12-10 23:59:59"
startdate <-lubridate::ymd_hms(startdate)

enddate <- "2021-12-13 23:59:59"
enddate <- lubridate::ymd_hms(enddate)
# lectura_sensores$DateTime <- ymd_hm(lectura_sensores$DateTime)
lectura_sensores$DateTime <- lubridate::ymd_hms(lectura_sensores$DateTime) 

lectura_sensores <- filter(lectura_sensores, DateTime > startdate)

reg_mat <- lapply(vct, function (x){
  datos_filtered <- tabla_datos[tabla_datos$Loc == x,]
  lectura_filtered <- lectura_sensores[lectura_sensores$Loc == x,]
  print(x)
  save <- nearestTime(datos_filtered,lectura_filtered,"DateTime","DateTime")
  
})

resultadofinal <- bind_rows(reg_mat)
getwd()
write.xlsx(resultadofinal,"Remus Measurements December.xlsx")
