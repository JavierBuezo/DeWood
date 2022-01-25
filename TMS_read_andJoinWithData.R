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





################################    This script has two parts. The one in which the air temperature during the measurement is paired with the measurement itself.#################################
###############################     and the one in which respirations, corrections, and volumes are calculated ################################




######################### Important Readme  ##############################
#The EGMs are configured with the Rumanian Time Zone, while the TMS are configured by default with the time zone of the last computer used 
#to download de data. Normally, the Spanish Time Zone. Thus, this should be corrected everytime before Joining the data. Also take care
#with the winter/summer hourtime

TimeCorrectionTMS <- 1 * 60 * 60 #The Time correction must be in seconds

#Read the excel with the Plots vs TMS codes 
path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/"
path.to.data <- "D:/OneDrive - UPNA/DeWood Project"
setwd(path.to.data)

#Load all the files with the TMS measurements and format the columns.
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
#Formatear y corregir la fecha
# lectura_sensores$DateTime <- anytime(lectura_sensores$DateTime)


##############   Read the file with the respiration measurements.
#############   This file is normally separated in two, the one coming from the EGM 4 and EGM 5. We combine them here.

setwd(path.to.data)
datos_path1 <- paste(getwd(),"/Respiration Campaign July 2021/EGM_1/LM Results/Joined With Field Table/",sep = "")
datos_path2 <- paste(getwd(),"/Respiration Campaign July 2021/EGM_2/LM Results/Joined With Field Table/",sep = "")

datos_path1 <- paste(getwd(),"/Respiration Campaign October 2021/EGM_1/LM Results/Joined With Field Table/",sep = "")
datos_path2 <- paste(getwd(),"/Respiration Campaign October 2021/EGM_2/LM Results/Joined With Field Table/",sep = "")

datos_path1 <- paste(getwd(),"/Respiration Campaign December/EGM_1/LM Results/Joined With Field Table/",sep = "")
datos_path2 <- paste(getwd(),"/Respiration Campaign December/EGM_2/LM Results/Joined With Field Table/",sep = "")

setwd(datos_path1)
files_nm1 <- list.files(datos_path1, pattern = ".csv")
datos1 <- lapply(files_nm1,fread)
tabladatos1 <- rbindlist(datos1, fill = TRUE)

setwd(datos_path2)

files_nm2 <- list.files(datos_path2, pattern = ".csv")
datos2 <- lapply(files_nm2,fread)
tabla_datos2 <- rbindlist(datos2, fill = TRUE)

#Combine both files
tabla_datos <- rbind(tabladatos1,tabla_datos2)

#Set the year
year <- "2021"

#Create Date Time column to be able to parse it in the future
tabla_datos$Date <- paste(year,tabla_datos$Month,tabla_datos$Day, sep = "-")
tabla_datos$Time <- paste(tabla_datos$Hour,tabla_datos$Min,"00",sep = ":")
tabla_datos$DateTime <- paste(tabla_datos$Date,tabla_datos$Time)
tabla_datos$DateTime <- ymd_hms(tabla_datos$DateTime)

#Create a column with the location, Plot_subplot, for further joining with TMS data
tabla_datos$Loc <- paste(tabla_datos$Plot,tabla_datos$SubPlot,sep = "_")
lectura_sensores$Loc <- paste(lectura_sensores$Plot,lectura_sensores$Subplot,sep="_")

#Extract the locations in a unique vector
vct <- unique(tabla_datos$Loc)
 



# A veces los sensores se descargan con una columna distinta al resto, en tal caso buscarlo y extraerlo para tratarlo aparte
# sensordefect <- ymd_hms(lectura_sensores$DateTime[lectura_sensores$Loc == "1_2"])




#Parse datetime and correct the time 
lectura_sensores$DateTime <- ymd_hm(lectura_sensores$DateTime)
lectura_sensores$DateTime <- lectura_sensores$DateTime + TimeCorrectionTMS

# lectura_sensores$DateTime[lectura_sensores$Loc=="1_2"] <- sensordefect
# 
# 
# lectura_sensores$DateTime <- lectura_sensores$DateTime + 3 * 60 * 60 #RECORDEMOS QUE LOS TMS ESTÁN 3 HORAS POR DETRÁS DE LOS EGM's
# #######################   Representar valores de los TMS    ######################


########################### The search for the closest time can take long with too many data. The measurements from the TMS must be filtered before##########################

# establecer la fecha de comienzo de toma de datos
startdate <- "2021-07-26 23:59:59"
startdate <-lubridate::ymd_hms(startdate)

enddate <- "2021-07-30 23:59:59"
enddate <- lubridate::ymd_hms(enddate)
# lectura_sensores$DateTime <- ymd_hm(lectura_sensores$DateTime)
lectura_sensores$DateTime <- lubridate::ymd_hms(lectura_sensores$DateTime) 

# filtrar por fecha
all_sensors_byrow <- filter(lectura_sensores, DateTime > startdate)

all_sensors_byrow$T3 <-as.numeric(all_sensors_byrow$T3)
all_sensors_byrow$Subplot <- as.character(all_sensors_byrow$Subplot)
# cambiar 
# representar los datos
# sens_temp_long <- gather(sens_tmp, Subplot ,T3, -c(DateTime, Plot,Subplot))
# 
# ggplot(sens_tmp,aes(x=DateTime,y=Soil_moist,color=Subplot))+
#   geom_line() +
#   facet_wrap(~ Plot) +
#   theme_bw()



############################ This part of the code is to represent the data from the TMS. Continue without this respiration measurements
all_sensors_byrow$datehour <- cut(as.POSIXct(all_sensors_byrow$DateTime,format="%Y-%m-%d %H:%M:%S"), breaks = "hour")
means <- aggregate(T3 ~ datehour, all_sensors_byrow, mean)
means$datehour <- ymd_hms(means$datehour)

all_sensors_byrow$Plot <- as.character(all_sensors_byrow$Plot)
sp <- ggplot(all_sensors_byrow, aes(DateTime, T3)) + 
  theme_bw()+
  geom_line(aes(x=DateTime, y=T3, color = Plot),size=0.1) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  labs(x = "Hour", 
       y = "Air temperature")+
  scale_x_datetime(date_labels = "%d %m")+
  ylim(-4,25)
sp
sp <- ggplot(means, aes(datehour, T3)) + 
  theme_bw()+
  geom_line(aes(x=datehour, y=T3, color=Subplot),size=0.1,) +
  labs(x = "Hour", 
       y = "Air temperature")+
  scale_x_datetime(date_labels = "%d %m")+
  ylim(0,25)
sp

sp <- ggplot(all_sensors_byrow, aes(DateTime, T3)) + 
theme_bw()+
  geom_line(aes(x=DateTime, y=T3, color = Subplot),size=0.1) +
  labs(x = "Hour", 
       y = "Air temperature")+
  scale_x_datetime(date_labels = "%H")+
  ylim(-4,25)
  
sp

#############################################################################################################################################

########################### Joining the data of respiration with the temperatures #########################################3

lectura_sensores <- filter(lectura_sensores, DateTime > startdate, DateTime < enddate)

reg_mat <- lapply(vct, function (x){
  datos_filtered <- tabla_datos[tabla_datos$Loc == x,]
  lectura_filtered <- lectura_sensores[lectura_sensores$Loc == x,]
  print(x)
  save <- nearestTime(datos_filtered,lectura_filtered,"DateTime","DateTime")
  
})

resultadofinal <- bind_rows(reg_mat)
########################################Set the variables of chamber volumes, discs, etc.... 

soil_chamberL <- 1.171 #Disponible en el manual del SRC-1 pp systems
tube_sectionsL <- 0.3927 #Tubo de 5 Cm de largo y 5 Cm radio
tube_section_areacm2 <- pi * (5^2)
big_chmberL <- 11.661397 # El volumen de esta cámara cambió de Julio a Octubre. CUIDADO Nagore: 8.256498L, DuckTapeBehemoth: 11.661397L  2915,39 Por medir
sml_chmberL <- 1.306706 
R_constant <- 0.08205746
resultadofinal$T3K <- resultadofinal$T3 + 274.15
################################################################################################################################################

#Assign the correct volume chamber to each sample                                 
resultadofinal <- resultadofinal %>% mutate(chambervolume =case_when(
  DiamClass==1 ~ sml_chmberL,
  DiamClass==10 ~ big_chmberL,
  DiamClass==25 ~ soil_chamberL + tube_sectionsL
  
))
######  Sometimes, atmospheric pressure comes divided by 10. This must be controlled.
resultadofinal <- resultadofinal %>% mutate(ATMP =case_when(
  ATMP< 0.1 ~ ATMP*10,
  ATMP>0.1 ~ ATMP
))

#Calculate respiration Resp= slope * (P*V)/(R*T)
resultadofinal$volcm3/1000
resultadofinal <- resultadofinal %>% mutate(respiration =case_when(
  DiamClass==1 ~ resultadofinal$slope *(resultadofinal$ATMP*(resultadofinal$chambervolume-(resultadofinal$Volcm3/1000)))/ (R_constant*resultadofinal$T3K),  #El volumen de los troncos está en cm3 = ml, lo paso a L para hacer el calculo
  DiamClass==10 ~ resultadofinal$slope *(resultadofinal$ATMP*(resultadofinal$chambervolume-(resultadofinal$Volcm3/1000)))/ (R_constant*resultadofinal$T3K),
  DiamClass==25 ~ resultadofinal$slope *(resultadofinal$ATMP*(resultadofinal$chambervolume))/ (R_constant*resultadofinal$T3K)
  
))

# resultadofinal$respiration <- resultadofinal$slope *(resultadofinal$ATMP*(resultadofinal$chambervolume-resultadofinal$Volcm3))/ (R_constant*resultadofinal$T3)

# resultadofinal$correctedArea <- resultadofinal$respiration / resultadofinal$Areacm2
resultadofinal <- resultadofinal %>% mutate(RespCorrectedArea =case_when(
  DiamClass==1 ~ resultadofinal$respiration / resultadofinal$Areacm2,
  DiamClass==10 ~ resultadofinal$respiration / resultadofinal$Areacm2,
  DiamClass==25 ~ resultadofinal$respiration / tube_section_areacm2  #El area medida en esta clase de diámetro es diferente
))

resultadofinal <- resultadofinal %>% mutate(RespCorrectedVolume =case_when(
  DiamClass==1 ~ resultadofinal$respiration / resultadofinal$Volcm3,
  DiamClass==10 ~ resultadofinal$respiration / resultadofinal$Volcm3,
  DiamClass==25 ~ resultadofinal$RespCorrectedArea * resultadofinal$Areacm2 / resultadofinal$Volcm3  #El area medida en esta clase de diámetro es diferente
))


#Check if everything is well
resultadofinal %>% group_by(DiamClass) %>%
  summarise_at(vars(respiration),
               list(name=mean))
resultadofinal %>% group_by(DiamClass) %>%
  summarise_at(vars(RespCorrectedArea),
               list(name=mean))
resultadofinal %>% group_by(DiamClass) %>%
  summarise_at(vars(RespCorrectedVolume),
               list(name=mean))

#Write the result. Watch the name, this script is for the measurement of each campaign. 
write.csv(resultadofinal,"RESULTADOFINALJulio.csv")
getwd()












########################################IGNORE THIS###########################################
# ##########################################REPRESENTACIÓN DE TEMPERATURAS(t1,t2,t3) POR FECHA#####################################################
# #Filtros de fecha
# startdate <- "2021-07-28"
# enddate <- "2021-07-29"
# 
# 
# # filtrar por fecha y representar las diferentes temperaturas
# all_sensors_byrow <- filter(lectura_sensores,DateTime > startdate, DateTime < enddate)
# 
# ggplot(all_sensors_byrow,aes(x=DateTime, y = T3, color=Subplot, group=interaction(Plot,Subplot)))+
# 
#   geom_line() +
#   facet_wrap(~ Plot) +
#   theme_bw()
# 
# ggplot(all_sensors_byrow,aes(x=DateTime, y = T2, color=Subplot, group=interaction(Plot,Subplot)))+
#   
#   geom_line() +
#   facet_wrap(~ Plot) +
#   theme_bw()
# ggplot(all_sensors_byrow,aes(x=DateTime, y = T1, color=Subplot, group=interaction(Plot,Subplot)))+
#   
#   geom_line() +
#   facet_wrap(~ Plot) +
#   theme_bw()
# 
# 
# #######################THEROS#####################
# # establecer el directorio de trabajo
# path.to.wd <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Theros 11"
# setwd(path.to.wd)
# 
# # guardar la dirección a la carpeta en la que están los archivos
# path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Theros 11"
# # cargar todos los archivos de la carpeta
# files_path <- list.files(path.to.data, full.names = T, pattern = ".xlsx")
# 
# # extraer los nombres de los sensores del nombre del archivo
# # primero guardar los nombres de los archivos sin el directorio
# files_nm <- list.files(path.to.data, pattern = ".xlsx")
# 
# # extraer el código de los sensores del nombre
# # cheatsheet para la manipulación de texto https://github.com/rstudio/cheatsheets/blob/master/strings.pdf
# teros_nm <- str_sub(files_nm, 1, 8)
# 
# # leer todos los archivos de cada sensor, limpiar y pegar
# # lo hacemos utilizando una función que realiza operaciones recurrentes. 
# # Tutorial para entender aquí: https://www.datacamp.com/community/tutorials/r-tutorial-apply-family?utm_source=adwords_ppc&utm_campaignid=12492439802&utm_adgroupid=122563404161&utm_device=c&utm_keyword=r%20apply%20function&utm_matchtype=b&utm_network=g&utm_adpostion=&utm_creative=504158805004&utm_targetid=kwd-341048392010&utm_loc_interest_ms=&utm_loc_physical_ms=1011804&gclid=CjwKCAjwxo6IBhBKEiwAXSYBs6HCM0w5PfcqnwYdTJljOuK49AGKW-L3nxiOVx5g4NgzEWhaPIgFeRoCXK4QAvD_BwE
# 
# # leer los archivos y cambiar los encabezados
# # x <- files_path[7]
# z <- lapply(files_path, function(x){ 
#   tmp <- readxl::read_xlsx(x, skip = 2)
#   nsens <- (dim(tmp)[2]-3)/2 # tenemos dataloggers con 2 y 6 sensores. Calculamos el numero de sensores a partir del numero de columnas del archivo
#   # colnames <- readxl::read_xlsx(x),
#   colnames(tmp) <- c("datetime", paste0(rep(c("WC", "T"),nsens),"_", rep(1:nsens, each = 2)), "Battery_perc","Battery_volt")
#   tmp
# }
# )
# 
# # View(z[[1]])
# names(z) <- teros_nm
# # pegar los archivos que corresponden al mismo sensor
# 
# # crear un vector con los nombres de los sensores
# teros_unique <- unique(teros_nm)
# # x <- teros_unique[1]
# sens_bind <- lapply(teros_unique, function(x) {
#   which <- str_detect(teros_nm, x)
#   tmp <- z[which]
#   bind <- bind_rows(tmp)
#   colnames(bind)[2:dim(bind)[2]] <- paste0(colnames(bind)[2:dim(bind)[2]])
#   bind$logger_number <- rep(x, dim(bind)[1])
#   return(bind)
# })
# 
# # juntar todos los sensores en un solo archivo (pegar por filas)
# all_sensors_byrow <- bind_rows(sens_bind)
# 
# # establecer la fecha de comienzo de toma de datos
# 
# 
# # filtrar por fecha
# all_sensors_byrow <- filter(all_sensors_byrow, datetime > startdate,datetime < enddate)
# 
# 
# # TEMPERATURA
# 
# # seleccionar temperatura
# sens_tmp <- all_sensors_byrow %>% 
#   select(.,datetime, logger_number, contains("T_"))
# 
# # cambiar 
# # representar los datos
# sens_temp_long <- gather(sens_tmp, sensor_number ,temp, -c(datetime, logger_number))
# 
# # formato panel
# ggplot(sens_temp_long, aes(x = datetime, y = temp, color = sensor_number)) +
#   geom_line() +
#   facet_wrap(~ logger_number) +
#   theme_bw()
# 
# # todo junto
# ggplot(sens_temp_long, aes(x = datetime, y = temp, color = logger_number,
#                            group=interaction(logger_number, sensor_number))) +
#   ylim(c(15, 25)) +
#   geom_line() +
#   theme_bw()

