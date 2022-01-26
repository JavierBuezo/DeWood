##########################################################
#THIS CODE COMBINES THE DATA OF RESPIRATION OF THE EGM5 WITH THE SAMPLES#
################################################################


# libraries
library(ggplot2)
library(dplyr)
library(readxl) # 1
library(tidyverse)
library(fs)
library(openxlsx)
library(tidyr)
library(stringi)

path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign December/EGM_2/"
path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign 09 September/"
path.to.data <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign October 2021/EGM_2/"


#path.to.data <- "C:/Users/NG.5027073/Dropbox (MNCN CSIC)/pr?ce/Nagore uam/INVESTIGACION/2020 - DeWood Romania/EGM 4/"
#Leemos el excell con los codigos de plot y muestra, separados por fecha
setwd(path.to.data)

#Load all the .xlsx files in the folder
files_path <- list.files(path.to.data, full.names = T, pattern = ".xlsx")
#Guardar el nombre del archivo para usarlo despues como titulo
files_nm <- list.files(path.to.data, pattern = ".xlsx")
files_nm <- stringr::str_subset(files_nm, fixed('~$'), negate = TRUE)
x <- files_nm[1]
lapply(files_nm, function(x){
  
#La relaci?n entre c?digo de plot y muestra ha de estar en la carpeta Codes en el excel Codes. En una hoja de c?lculo con el mismo nombre de la fecha.

#El formato ha de ser DD_MM_YY
  # codigosplot <-read_excel("Codes/Codes_July_2021.xlsx",substr(x,1,8))
  codigosplot <-read_excel("Codes/Codes_December_2021.xlsx",substr(x,1,8))
colnames(codigosplot)[3] <-"egmplotcode"
colnames(codigosplot)[2] <-"sample_code"

#Leemos el excel donde se encuentran los datos y formateamos las columnas para que puedan entrar en nuestro programa de regresi?n lineal.
 measurements <- read_excel(x)
# measurements <- read.csv(x,sep = ",")
colnames(measurements)[1]<-"data_format"
colnames(measurements)[2]<-"date"
colnames(measurements)[3]<-"time"
colnames(measurements)[4]<-"egmplotcode"
colnames(measurements)[5]<-"RecNo"
colnames(measurements)[6]<-"CO2 Ref"
colnames(measurements)[7]<-"Atm_press"
colnames(measurements)[8]<-"Flow_rate"
colnames(measurements)[9]<-"H2O"
colnames(measurements)[10]<-"H2O_temp"
colnames(measurements)[11]<-"O2"
colnames(measurements)[12]<-"Cod_err"
colnames(measurements)[13]<-"Out_vltg"
colnames(measurements)[14]<-"PAR"
colnames(measurements)[15]<-"Tsoil"
colnames(measurements)[16]<-"Tair"
colnames(measurements)[17]<-"RH"
# nchar(measurements$date[1])
# nchar(measurements$date[1]) == 10
#Separamos dia, mes, hora y minuto en columnas nombradas para que puedan ser usadas en la funcion de regresi?n lineal 
#El EGM-5 a veces registra la fecha en formato dd/mm/YY o dd/mm/YYYY, hay que comprobarlo y actuar en consecuencia
# if(nchar(measurements$date[1])==10){ #Si el formato  de la fecha suma 10 caracteres tomamos dd/mm/YYYY
#   vecDay <- substring(measurements$date,9,10)
#   vecMonth <- substring(measurements$date,6,7)
# }
# else{ #si no dd/mm/yy
  vecDay <- substring(measurements$date,1,2)
  vecMonth <- substring(measurements$date,4,5)
# }

#Lo mismo pasa con el formato de la hora, a veces el EGM toma el dato como hh/mm/ss y en otras a?ade a la izquierda n?meros que no entiendo (19 caracteres totales)
# if(nchar(measurements$time[1])==11){
#   vecHour <- substring(measurements$time,12,13)
#   vecMin <- substring(measurements$time,15,16)
#   
# } else{
vecHour <- substring(measurements$time,1,2)
vecMin <- substring(measurements$time,4,5)
# }


measurements$Day <- vecDay
measurements$Month <- vecMonth
measurements$Hour <- vecHour
measurements$Min <- vecMin

#hacemos un LeftJoin para unir codigos con muestreo usando egmplotcode como ID.
Tablafinal <- left_join(measurements, codigosplot, by="egmplotcode")


#Guardamos el archivo como un CSV para poder usarlo en la funci?n. 

write.csv(Tablafinal,paste("Joined",substr(x,1,nchar(x)-5),".csv"))
})
