# cargar librerias

library(ggpubr)
library(plyr)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(readxl)
library(data.table)
library(dplyr)

####READ NAGORE SENSORS########
################
#Prepare codification
path.to.codification <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files"
setwd(path.to.codification)
nagocodes <- read_excel("Tabla_sensores.xlsx",sheet = "NagoSensors")

nagosensors.join <- left_join(nagocodes,Table_Field,by = "sample_code")
colnames(nagosensors.join)[3] <- "Sensor"
colnames(nagosensors.join)[1] <- "Sensor_code"
nagosensors25 <- filter(nagosensors.join,nagosensors.join$DiamClass=="25")

#Run reading
path.to.data <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Respiration Campaing April 2022/April NagoSensors/Data"
files_path <- list.files(path.to.data, full.names = T, pattern = ".CSV")
files_nm <- list.files(path.to.data, pattern = ".CSV")
nagosensors_nm <- str_sub(files_nm, 1, 5)
z <- lapply(files_path, function(x){ 
  tmp <- fread(x)
}
)

names(z) <- nagosensors_nm
nagosensors_unique <- unique(nagosensors_nm)
# x <- teros_unique[1]
nagosensors_bind <- lapply(nagosensors_unique, function(x) {
  which <- str_detect(nagosensors_nm, x)
  tmp <- z[which]
  bind <- bind_rows(tmp)
  colnames(bind)[2:dim(bind)[2]] <- paste0(colnames(bind)[2:dim(bind)[2]])
  bind$logger_number <- rep(x, dim(bind)[1])
  return(bind)
})
nagosensors.read <- ldply(nagosensors_bind,data.frame)
colnames(nagosensors.read)[5:12] <- c("A0","A1","A2","A3","A4","A5","A6","A7")
library(reshape2)
nagosensors_long <- melt(nagosensors.read,id.vars = c("Date.Time","Temp.C.","RH...","Dew.Point.C.","logger_number"))
colnames(nagosensors_long)[6:7] <- c("sensor","impedance")
nagosensors_long$Sensor_code <- paste(nagosensors_long$logger_number,nagosensors_long$sensor,sep="-")
nagosensors_all <- left_join(nagosensors_long,nagosensors.join,by="Sensor_code")

write.csv(nagosensors_all,"NagoreSensors_ALL YEAR.csv",row.names = FALSE)
getwd()
##################

###############READ TEROS
########################
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
all_sensors_byrow$loc <- NA
all_sensors_byrow$loc[all_sensors_byrow$logger_number == "S1plot6z"] <- "6_4"
all_sensors_byrow$loc[all_sensors_byrow$logger_number == "S2plot1_"] <- "1_5"
all_sensors_byrow$loc[all_sensors_byrow$logger_number == "S3plo3z6"] <- "3_1"


sens_WC <- all_sensors_byrow %>% 
  select(.,datetime, loc,logger_number,contains("WC_"))
sens_WC[sens_WC==0]<-NA

sens_WC_long <- gather(sens_WC, sensor_number ,WC, -c(datetime, loc,logger_number))

sens_WC_long$logger_number <- substr(sens_WC_long$logger_number,1,2)
sens_WC_long$sensor_code <- paste(sens_WC_long$logger_number,sens_WC_long$sensor_number,sep="")
sens_WC_long$sensor_code <- gsub("T","WC",sens_WC_long$sensor_code)
###############
####################Read TMS
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
lectura_sensores$Soil_moist <- lectura_sensores$Soil_moist/10000
lectura_sensores$Loc <- paste(lectura_sensores$Plot,lectura_sensores$Subplot,sep="_")
lectura_sensores$DateTime <- ymd_hm(lectura_sensores$DateTime)
lectura_sensores$DateTime <- lectura_sensores$DateTime + TimeCorrectionTMS
lectura_sensores <- filter(lectura_sensores,lectura_sensores$DateTime > ymd_hms("2021-07-20 16:00:00")) 

lectura_sensores.day <- lectura_sensores %>%
  mutate(time=floor_date(DateTime, "hour"))
lectura_sensores.g <- group_by(lectura_sensores.day,time,Loc)
tmsavg <-   lectura_sensores.g %>% summarize_at(c("T1","T2","T3"),mean)
tmsavg <- ungroup(tmsavg)

tmsavg.melt <- melt(tmsavg,id.vars = c("time","Loc"))

daysfroze <- filter(tmsavg,tmsavg$Loc=="6_4"&tmsavg$T3<0)
daysfroze.list <- unique(daysfroze$time)

####READ THE CODES OF Sensors######
path.to.codification <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files"
setwd(path.to.codification)
codification <- read_excel("Tabla_sensores.xlsx",sheet = "Teros11")
#Leer el archivo donde se resumen las caracter?sticas de cada muestra y formatear la tabla extrayendo del c?digo los datos
Table_Field <- fread("Field_tableW_New_Densities.csv")
Table_Field$type<- paste(Table_Field$DiamClass,Table_Field$Species,Table_Field$Class,sep="")

codification.filtered <- codification[,c("LABEL","sensor_code")]
Table_Field.filtered <- Table_Field[,c("type","sample_code")]
colnames(Table_Field.filtered)[2] <- "LABEL"
sens_WC_long.labeled <- merge(sens_WC_long,codification.filtered,by="sensor_code")
sens_WC_long.labeled <- merge(sens_WC_long.labeled,Table_Field.filtered,by="LABEL")
sens_WC_long.labeled$type <- gsub("10FS2", "25FS4",sens_WC_long.labeled$type)   


####READ FIELD TABLE

path.to.data <- "C:/Users/Javie/Documents/DeWood GitHub/DeWood/Files"


#path.to.data <- "C:/Users/NG.5027073/Dropbox (MNCN CSIC)/pr?ce/Nagore uam/INVESTIGACION/2020 - DeWood Romania/EGM 4/"
#Leemos el excell con los codigos de plot y muestra, separados por fecha
setwd(path.to.data)

Table_Field <- read_xlsx("Table field final.xlsx")

Table_Field <- Table_Field[,c("LABEL","Code")]
Table_Field$Code <- str_remove_all(Table_Field$Code,pattern = " ")
Table_Field$Code <- str_replace_all(Table_Field$Code,pattern = "1-",replacement = "01-")

#Clase de Diametro
Table_Field$DiamClass <- substr(Table_Field$Code,1,2)
Table_Field$Plot <- substr(Table_Field$Code,5,5)
Table_Field$SubPlot <- substr(Table_Field$Code,7,7)
Table_Field$Class <- substr(Table_Field$Code,9,9)
Table_Field$Species <- substr(Table_Field$Code,10,11)
Table_Field$Type <- paste(Table_Field$DiamClass,Table_Field$Species,Table_Field$Class,sep = "")
colnames(Table_Field)[1] <-"sample_code"




########We can start to analyze
library(bayesbio)
library(data.table)
library(dplyr)

nagosensors_all <- fread("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Respiration Campaing April 2022/April NagoSensors/Data/NagoreSensors_ALL YEAR.csv")

nagosensors25 <- filter(nagosensors_all,nagosensors_all$DiamClass == "25"&nagosensors_all$logger_number=="A0033")

nagosensors25$Date.Time <- mdy_hms(nagosensors25$Date.Time)

nagosensors25$Date.Time <- as.POSIXct(nagosensors25$Date.Time,origin= "1984-01-01")
sens_WC_long.labeled$datetime <- as.POSIXct(sens_WC_long.labeled$datetime,origin= "1984-01-01")
sens_WC_long.labeled <- filter(sens_WC_long.labeled,sens_WC_long.labeled$datetime > as.POSIXct("2021-10-29 10:01:00"))
nagosensors_unique <- unique(nagosensors25$sample_code)

sens_WC_long.labeled$datetime <- sens_WC_long.labeled$datetime + (1*60*60)

#Preparar dataset de nagosensors. Para media cada hora
tmpnago.hour <-nagosensors25 %>% 
  mutate(time = floor_date(Date.Time,"hour"))
# tmpnagoavg <- na.omit(tmpnago.g) %>% summarize_at(c("impedance"),mean)

tmpnagoavg<- na.omit(tmpnago.hour) %>% dplyr::group_by(time,sample_code) %>% mutate (avgWC=mean(impedance)) %>% ungroup
tmpnagoavg$Date.Time <- NULL
tmpnagoavg$impedance <- NULL
tmpnagoavgfin <- tmpnagoavg[!duplicated(tmpnagoavg),]

#Preparar dataset teros. Media cada hora
tmpteros.hour <-sens_WC_long.labeled %>% 
  mutate(time = floor_date(datetime,"hour"))

tmpterosavg<- na.omit(tmpteros.hour) %>% group_by(time) %>% mutate (avgTerosWC=mean(WC)) %>% ungroup
tmpterosavg$datetime <- NULL
tmpterosavg$WC <- NULL
tmpterosavgfin <- tmpterosavg[!duplicated(tmpterosavg),]


########Ciclo para emparejar las humedades recorriendo cada sample
prueba <- lapply(nagosensors_unique,function(x){
  print(x)
  nago <- filter(tmpnagoavgfin,tmpnagoavgfin$sample_code==x)
  teros <- filter(tmpterosavgfin,tmpterosavgfin$LABEL == x)
  
  juntos <- left_join(nago,teros,by="time")
})
final <- bind_rows(prueba)
ncol(final)


final <- final %>% 
  mutate(day = floor_date(time,"day"))

finalnotfrozen <- final[final$day %in% daysfroze.list,]
# final[!final$time %in% daysfroze.list,]$avgWC <- NA
# final[!final$time %in% daysfroze.list,]$avgTerosWC <- NA  
  
finalfiltered <- finalnotfrozen[,c("sample_code","time","avgWC","avgTerosWC")]

write.csv(finalfiltered,"TerosAndNagoSensors_byHour.csv",row.names = FALSE)

###################


# library(reshape2)
# # finalfiltered$avgTerosWC = finalfiltered$avgTerosWC*1000
# 
# 
# 
# start_val_sigmoid <- function(x, y) {
#   fit <- lm(log(y[which.max(x)] - y + 1e-6) ~ x)
#   list(
#     a = y[which.max(x)],
#     r = unname(-coef(fit)[2]))
# }
# 
# lista <- start_val_sigmoid(finalfiltered$avgWC,finalfiltered$avgTerosWC)
# lista
# m <- NULL
# m <- nls(avgTerosWC ~ a*exp(r* avgWC), data = finalfiltered,start=lista,trace=TRUE)
# 
# m <- nls(avgWC ~ a*exp(r* avgTerosWC), data = finalfiltered,start=lista)
# 
# # m <- nls(avgTerosWC ~ a*exp(r* avgWC), data = finalfiltered,start= list(a=5842404.74, r=-66.72))
# 
# m
# 
# p <- coef(m)
# p
# 
# #########Trying to SCALE THE VARIABLES###########
# lista<- start_val_sigmoid(finalfiltered$avgTerosWCcentered,finalfiltered$avgWCcentered)
# lista
# m <- NULL
# m <- nls(avgTerosWCcentered ~ a*exp(r* avgWCcentered), data = finalfiltered,start=list(a=0.5,r=0.2),trace=TRUE)
# p <- coef(m)
# p
# dev.off()
# plot(finalfiltered$avgWCcentered,finalfiltered$avgTerosWCcentered)
# curve(p["a"]*exp(p["r"]*x),lwd=2,col="Red",add=TRUE)
# ####AQUI#####AYUDA CON EL SCRIPT! PORFA
# 
# dev.off()
# plot(finalfiltered$avgWC,finalfiltered$avgTerosWC)
# curve(p["a"]*exp(p["r"]*x),lwd=2,col="Red",add=TRUE)
# 
# plot(finalfiltered$avgTerosWC,finalfiltered$avgWC)
# curve(p["a"]*exp(p["r"]*x),lwd=2,col="Red",add=TRUE)

##############REPRESENTACIONES
#################
prueba2 <- melt(finalfiltered,id.vars = c("time","sample_code"))
  ggplot(prueba2,aes(x=time,y=value,color=variable))+
    geom_line()+
    facet_wrap(~sample_code)
  
  ggplot(finalfiltered,aes(x=avgTerosWC,y=avgWC),color=sample_code)+
    geom_point(aes(color=sample_code))
  
  ggplot(prueba2,aes(x=time,y=value,color=variable))+
    geom_point()+
    facet_wrap(~sample_code)
  

  # geom_smooth(method="lm", formula=y ~ log(x))+
  # stat_regline_equation(formula = y ~ log(x),aes(label=..rr.label..),position = "identity",label.y=0.32)
  # 
  
  ggplot(final,aes(x=time,y=avgWC))+
    ggtitle("Teros WC vs Nagosensors Impedance")+
    ylab("Nagosensors Impedance")+
    ylim(0,5000)+
    xlab("Teros WC")+
    geom_point(size=1)+
    facet_wrap(~LABEL)
 
   ggplot(final,aes(x=time,y=avgTerosWC))+
    ggtitle("Teros WC vs Nagosensors Impedance")+
    ylab("Nagosensors Impedance")+
    ylim(0,1)+
    xlab("Teros WC")+
    geom_point(size=1)+
    facet_wrap(~LABEL)
   