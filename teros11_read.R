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

# establecer el directorio de trabajo
path.to.wd <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign December/"
path.to.wd <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/"
path.to.wd <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign October 2021/"
path.to.wd <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign December/"

setwd(path.to.wd)

# guardar la dirección a la carpeta en la que están los archivos
path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign December/Teros"
path.to.data <- "D:/OneDrive - UPNA/DeWood Project/Theros 11"
path.to.data <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign October 2021/Teros/"
# cargar todos los archivos de la carpeta
files_path <- list.files(path.to.data, full.names = T, pattern = ".xlsx")

# extraer los nombres de los sensores del nombre del archivo
# primero guardar los nombres de los archivos sin el directorio
files_nm <- list.files(path.to.data, pattern = ".xlsx")

# extraer el código de los sensores del nombre
# cheatsheet para la manipulación de texto https://github.com/rstudio/cheatsheets/blob/master/strings.pdf
teros_nm <- str_sub(files_nm, 1, 8)

# leer todos los archivos de cada sensor, limpiar y pegar
# lo hacemos utilizando una función que realiza operaciones recurrentes. 
# Tutorial para entender aquí: https://www.datacamp.com/community/tutorials/r-tutorial-apply-family?utm_source=adwords_ppc&utm_campaignid=12492439802&utm_adgroupid=122563404161&utm_device=c&utm_keyword=r%20apply%20function&utm_matchtype=b&utm_network=g&utm_adpostion=&utm_creative=504158805004&utm_targetid=kwd-341048392010&utm_loc_interest_ms=&utm_loc_physical_ms=1011804&gclid=CjwKCAjwxo6IBhBKEiwAXSYBs6HCM0w5PfcqnwYdTJljOuK49AGKW-L3nxiOVx5g4NgzEWhaPIgFeRoCXK4QAvD_BwE

# leer los archivos y cambiar los encabezados
# x <- files_path[7]
z <- lapply(files_path, function(x){ 
  tmp <- readxl::read_xlsx(x, skip = 2)
  nsens <- (dim(tmp)[2]-3)/2 # tenemos dataloggers con 2 y 6 sensores. Calculamos el numero de sensores a partir del numero de columnas del archivo
  # colnames <- readxl::read_xlsx(x),
  colnames(tmp) <- c("datetime", paste0(rep(c("WC", "T"),nsens),"_", rep(1:nsens, each = 2)), "Battery_perc","Battery_volt")
  tmp
  }
  )

# View(z[[1]])
names(z) <- teros_nm
# pegar los archivos que corresponden al mismo sensor

# crear un vector con los nombres de los sensores
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

# juntar todos los sensores en un solo archivo (pegar por filas)
all_sensors_byrow <- bind_rows(sens_bind)

# establecer la fecha de comienzo de toma de datos
startdate <- "2021-12-01"
enddate <- "2021-12-12"

# filtrar por fecha
all_sensors_byrow <- filter(all_sensors_byrow, datetime > startdate, datetime < enddate)
min(all_sensors_byrow$Battery_perc)

# TEMPERATURA

# seleccionar temperatura
sens_tmp <- all_sensors_byrow %>% 
  select(.,datetime, logger_number, contains("T_"))
sens_tmp[sens_tmp==0]<-NA
# cambiar 


sens_temp_long <- gather(sens_tmp, sensor_number ,temp, -c(datetime, logger_number))
#Formatear los datos
sens_temp_long <- gather(sens_tmp, sensor_number ,temp, -c(datetime, logger_number))
sens_temp_long$logger_number <- substr(sens_temp_long$logger_number,1,2)
sens_temp_long$sensor_code <- paste(sens_temp_long$logger_number,sens_temp_long$sensor_number,sep="")
sens_temp_long$sensor_code <- gsub("T","WC",sens_temp_long$sensor_code)
#Leer el archivo que indica a qué esta conectado cada sensor, y formatear la tabla.
path.to.codification <- "D:/OneDrive - UPNA/DeWood Project"
path.to.codification <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/"
setwd(path.to.codification)
codification <- read_excel("Tabla_sensores.xlsx",sheet = "Teros11")

#Leer el archivo donde se resumen las características de cada muestra y formatear la tabla extrayendo del código los datos
Table_Field <- read_excel("Table field final.xlsx")
Table_Field$DiamClass <- substr(Table_Field$Code,1,2)
Table_Field$Plot <- substr(Table_Field$Code,5,5)
Table_Field$SubPlot <- substr(Table_Field$Code,7,7)
Table_Field$Class <- substr(Table_Field$Code,9,9)
Table_Field$Species <- substr(Table_Field$Code,10,11)

#Unificar las tablas usando el código
measurementmerged <- merge(sens_temp_long,codification,by="sensor_code")
#Unificar la tabla con los datos de la muestra
measurementmerged2 <-merge(measurementmerged,Table_Field, by="LABEL")

#Separar los datos por hora, para hacer medias después
measurementmerged2$type <- paste(measurementmerged2$Species,measurementmerged2$DiamClass,measurementmerged2$Class,sep="_")
measurementmerged2$datehour <- cut(as.POSIXct(measurementmerged2$datetime,format="%Y-%m-%d %H:%M:%S"), breaks = "hour") 
measurementmerged2$datehour <- ymd_hms(measurementmerged2$datehour)

#Esto es temporal, una muestra está mal apuntada. Se solucionará en el futuro. por el momento está de forma manual
measurementmerged2$type <- gsub("FS_10_2", "FS_25_4",measurementmerged2$type)

#Separar la tabla por tipo, que tiene en cuenta la especia, clase y diametros
list1 <- split(measurementmerged2, measurementmerged2$type)

#Hacer la media de los datos dentro de cada hora, y de cada tipo
averages <- lapply(list1, function(x) aggregate(temp ~ datehour+type,x,mean) )
averages2 <- bind_rows(averages)

#######################
#Estos datos provienen del programa TMS_read_andjoinwithdata
means$type <- "T. Air"
means <- dplyr::rename(means, "temp"="T3")
#######################

require (scales)
averages2 <- rbind(averages2,means)
ggplot(averages2, aes(x = datehour, y = temp)) +
  geom_line(aes(x=datehour,y=temp,color=type)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  # facet_wrap(~ logger_number) +
  labs(x = "Day", 
       y = "Temperature (Cº)")+
  ylim(-5,10)+
  # xlim(as.Date(c("2021-12-01","2021-12-20")))+
  scale_x_datetime(labels = date_format("%d"),date_breaks = "24 hours")+
  theme_bw()



############ WATER CONTENT #####################

# seleccionar WC
sens_WC <- all_sensors_byrow %>% 
  select(.,datetime, logger_number, contains("WC_"))
sens_WC[sens_WC==0]<-NA

#Formatear los datos
sens_WC_long <- gather(sens_WC, sensor_number ,temp, -c(datetime, logger_number))
sens_WC_long$logger_number <- substr(sens_WC_long$logger_number,1,2)
sens_WC_long$sensor_code <- paste(sens_WC_long$logger_number,sens_WC_long$sensor_number,sep="")

#Leer el archivo que indica a qué esta conectado cada sensor, y formatear la tabla.
path.to.codification <- "D:/OneDrive - UPNA/DeWood Project"
setwd(path.to.codification)
codification <- read_excel("Tabla_sensores.xlsx",sheet = "Teros11")
#Leer el archivo donde se resumen las características de cada muestra y formatear la tabla extrayendo del código los datos
Table_Field <- read_excel("Table field final.xlsx")
Table_Field$DiamClass <- substr(Table_Field$Code,1,2)
Table_Field$Plot <- substr(Table_Field$Code,5,5)
Table_Field$SubPlot <- substr(Table_Field$Code,7,7)
Table_Field$Class <- substr(Table_Field$Code,9,9)
Table_Field$Species <- substr(Table_Field$Code,10,11)

#Unificar las tablas usando el código
measurementmerged <- merge(sens_WC_long,codification,by="sensor_code")
#Unificar la tabla con los datos de la muestra
measurementmerged2 <-merge(measurementmerged,Table_Field, by="LABEL")

#Separar los datos por hora, para hacer medias después
measurementmerged2$type <- paste(measurementmerged2$Species,measurementmerged2$DiamClass,measurementmerged2$Class,sep="_")
measurementmerged2$datehour <- cut(as.POSIXct(measurementmerged2$datetime,format="%Y-%m-%d %H:%M:%S"), breaks = "hour") 
measurementmerged2$datehour <- ymd_hms(filteredmeans$datehour)

#Esto es temporal, una muestra está mal apuntada. Se solucionará en el futuro. por el momento está de forma manual
measurementmerged2$type <- gsub("FS_10_2", "FS_25_4",measurementmerged2$type)

#Separar la tabla por tipo, que tiene en cuenta la especia, clase y diametros
list1 <- split(measurementmerged2, measurementmerged2$type)

#Hacer la media de los datos dentro de cada hora, y de cada tipo
averages <- lapply(list1, function(x) aggregate(temp ~ datehour+type,x,mean) )
averages2 <- bind_rows(averages)
averages2$datehour <- ymd_hms(averages2$datehour)
#Representar
ggplot(averages2, aes(x = datehour, y = temp)) +
  geom_line(aes(x=datehour,y=temp,color=type)) +
  # facet_wrap(~ logger_number) +
  labs(x = "Day", 
       y = "Volumetric water content (M3/M3)")+
  ylim(0,0.5)+
  scale_x_datetime(date_labels = "%h")+
  scale_x_datetime(labels = date_format("%d"),date_breaks = "24 hours")+
  theme_bw()

plots <- lapply(averages,function(x) ggplot(x, aes(x = datehour, y = temp,group=1)) +
                  geom_line() +
                  # facet_wrap(~ logger_number) +
                  labs(x = "Month", 
                       y = "Volumetric water content (M3/M3)")+
                  ylim(0,0.5)+
                  # scale_x_datetime(date_labels = "%m")+
                  theme_bw())
plots





