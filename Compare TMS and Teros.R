###################################################@
# CODIGO PARA EXTRAER Y LIMPIAR DATOS TEROS-11 ####
###################################################@

# cargar librerias
library(stringr)
library(dplyr)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(readxl)

path.to.wd <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files"
setwd(path.to.wd)

path.to.data <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Respiration Campaing April 2022/Teros"
files_path <- list.files(path.to.data, full.names = T, pattern = ".xlsx")
files_nm <- list.files(path.to.data, pattern = ".xlsx")
teros_nm <- str_sub(files_nm, 1, 8)
z <- lapply(files_path, function(x){ 
  tmp <- readxl::read_xlsx(x, skip = 2)
  nsens <- (dim(tmp)[2]-3)/2 # tenemos dataloggers con 2 y 6 sensores. Calculamos el numero de sensores a partir del numero de columnas del archivo
  # colnames <- readxl::read_xlsx(x),
  colnames(tmp) <- c("datetime", paste0(rep(c("WC", "T"),nsens),"_", rep(1:nsens, each = 2)), "Battery_perc","Battery_volt")
  tmp
}
)

names(z) <- teros_nm
teros_unique <- unique(teros_nm)
# x <- teros_unique[1]
sens_bind <- lapply(teros_unique, function(x) {
  which <- str_detect(teros_nm, x)
  tmp <- z[which]
  bind <- bind_rows(tmp)
  colnames(bind)[2:dim(bind)[2]] <- paste0(colnames(bind)[2:dim(bind)[2]])
  bind$logger_number <- rep(x, dim(bind)[1])
  return(bind)
})

all_sensors_byrow <- bind_rows(sens_bind)

#Añadir la localización de los TEROS
all_sensors_byrow$loc <- NA
all_sensors_byrow$loc[all_sensors_byrow$logger_number == "S1plot6z"] <- "6_4"
all_sensors_byrow$loc[all_sensors_byrow$logger_number == "S2plot1_"] <- "1_5"
all_sensors_byrow$loc[all_sensors_byrow$logger_number == "S3plo3z6"] <- "3_1"
unique(all_sensors_byrow$logger_number)

#Transformar las humedades


#######Leer TMS
TimeCorrectionTMS <- 1 * 60 * 60 #The Time correction must be in seconds

#Read the excel with the Plots vs TMS codes 
path.to.data <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files"
setwd(path.to.data)

#Load all the files with the TMS measurements and format the columns.
sensores_path <- paste(getwd(),"/Respiration Campaing April 2022/TMS4/All days with plots/",sep = "")
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
lectura_sensores$Soil_moist <- lectura_sensores$Soil_moist/1000

#Formatear y corregir la fecha
# lectura_sensores$DateTime <- anytime(lectura_sensores$DateTime)
lectura_sensores$Loc <- paste(lectura_sensores$Plot,lectura_sensores$Subplot,sep="_")
lectura_sensores$DateTime <- ymd_hm(lectura_sensores$DateTime)
lectura_sensores$DateTime <- lectura_sensores$DateTime + TimeCorrectionTMS

unique(all_sensors_byrow$loc)
teros6_4 <- filter(all_sensors_byrow,all_sensors_byrow$loc=="6_4")
tms6_4 <- filter(lectura_sensores,lectura_sensores$Loc=="6_4")
terosstart <- "2021-12-11 11:00:00"
tms6_4 <- filter(tms6_4,tms6_4$DateTime >= terosstart)
library(bayesbio)
teros6_4$datetime <- as.POSIXct(teros6_4$datetime)
tms6_4$DateTime <- as.POSIXct(tms6_4$DateTime)


Clima6_4 <- nearestTime(teros6_4,tms6_4,  "datetime", "DateTime")
