###################################################@
# CODIGO PARA EXTRAER Y LIMPIAR DATOS TEROS-11 ####
###################################################@

# cargar librerias
library(stringr)
library(ggpubr)
library(plyr)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(readxl)
library(data.table)
library(dplyr)
path.to.wd <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files"
setwd(path.to.wd)
#########READ SENSOR TABLE and FIELD TABLE
path.to.codification <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files"
setwd(path.to.codification)
codification <- read_excel("Tabla_sensores.xlsx",sheet = "Teros11")
#Leer el archivo donde se resumen las caracter?sticas de cada muestra y formatear la tabla extrayendo del c?digo los datos
Table_Field <- fread("Field_tableW_New_Densities.csv")
Table_Field$type<- paste(Table_Field$DiamClass,Table_Field$Species,Table_Field$Class,sep="")
#############################################
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

sens_tmp <- all_sensors_byrow %>% 
  select(.,datetime, loc,logger_number,contains("T_"))
sens_tmp[sens_tmp==0]<-NA

sens_temp_long <- gather(sens_tmp, sensor_number ,temp, -c(datetime, loc,logger_number))

sens_temp_long$logger_number <- substr(sens_temp_long$logger_number,1,2)
sens_temp_long$sensor_code <- paste(sens_temp_long$logger_number,sens_temp_long$sensor_number,sep="")
sens_temp_long$sensor_code <- gsub("T","WC",sens_temp_long$sensor_code)

codification.filtered <- codification[,c("LABEL","sensor_code")]
Table_Field.filtered <- Table_Field[,c("type","sample_code")]
colnames(Table_Field.filtered)[2] <- "LABEL"
sens_temp_long.labeled <- merge(sens_temp_long,codification.filtered,by="sensor_code")
sens_temp_long.labeled <- merge(sens_temp_long.labeled,Table_Field.filtered,by="LABEL")
sens_temp_long.labeled$type <- gsub("10FS2", "25FS4",sens_temp_long.labeled$type)
#######FINALLY WE HAVE ALL THE TEMPERATURES#############
#######REPEAT PROCCESS FOR WC##################
sens_WC <- all_sensors_byrow %>% 
  select(.,datetime, loc,logger_number,contains("WC_"))
sens_WC[sens_WC==0]<-NA

sens_WC_long <- gather(sens_WC, sensor_number ,WC, -c(datetime, loc,logger_number))

sens_WC_long$logger_number <- substr(sens_WC_long$logger_number,1,2)
sens_WC_long$sensor_code <- paste(sens_WC_long$logger_number,sens_WC_long$sensor_number,sep="")
sens_WC_long$sensor_code <- gsub("T","WC",sens_WC_long$sensor_code)

codification.filtered <- codification[,c("LABEL","sensor_code")]
Table_Field.filtered <- Table_Field[,c("type","sample_code")]
colnames(Table_Field.filtered)[2] <- "LABEL"
sens_WC_long.labeled <- merge(sens_WC_long,codification.filtered,by="sensor_code")
sens_WC_long.labeled <- merge(sens_WC_long.labeled,Table_Field.filtered,by="LABEL")
sens_WC_long.labeled$type <- gsub("10FS2", "25FS4",sens_WC_long.labeled$type)                                    
#######FINALLY WE HAVE ALL THE WATER CONTENTS#############                                    


#######We READ THE TMS################
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

#Formatear y corregir la fecha
# lectura_sensores$DateTime <- anytime(lectura_sensores$DateTime)
lectura_sensores$Loc <- paste(lectura_sensores$Plot,lectura_sensores$Subplot,sep="_")
lectura_sensores$DateTime <- ymd_hm(lectura_sensores$DateTime)
lectura_sensores$DateTime <- lectura_sensores$DateTime + TimeCorrectionTMS
lectura_sensores <- filter(lectura_sensores,lectura_sensores$DateTime > ymd_hms("2021-07-20 16:00:00")) 

######## Now we have both TEROS and TMS prepared to work

#########PREPARE TEMPERATURE###############
sens_temp_long.day <-sens_temp_long.labeled %>% 
    mutate(time = floor_date(datetime,"day"))
sens_temp_long.g <- group_by(sens_temp_long.day,time,type,loc)
lectura_sensores.day <- lectura_sensores %>%
    mutate(time=floor_date(DateTime, "day"))
lectura_sensores.g <- group_by(lectura_sensores.day,time,Loc)

terostempavg <- na.omit(sens_temp_long.g) %>% summarize_at(c("temp"),mean)
terostempavg <- ungroup(terostempavg)
tmsavg <-   lectura_sensores.g %>% summarize_at(c("T1","T2","T3"),mean)
tmsavg <- ungroup(tmsavg)

tmsavg.melt <- melt(tmsavg,id.vars = c("time","Loc"))
colnames(tmsavg.melt) <- c("time","loc","type","temp")


tempsunited <- rbind(terostempavg,tmsavg.melt)

temps6_4 <- filter(tempsunited,tempsunited$loc=="6_4")
ggplot(tms6_4.avg.day,aes(x=time,y=Soil_moist)) +
  geom_line()
tempplot <- ggplot(temps6_4,aes(x=time,y=temp)) +
  geom_line(aes(color=type)) # I want to save this
###########################################

#########Prepare WC######################


sens_WC_long.day <-sens_WC_long.labeled %>% 
  mutate(time = floor_date(datetime,"day"))
sens_WC_long.g <- group_by(sens_WC_long.day,time,type,loc)
lectura_sensores.day <- lectura_sensores %>%
  mutate(time=floor_date(DateTime, "day"))
lectura_sensores.g <- group_by(lectura_sensores.day,time,Loc)

terosWCavg <- na.omit(sens_WC_long.g) %>% summarize_at(c("WC"),mean)
terosWCavg <- ungroup(terosWCavg)
tmsWCavg <-   lectura_sensores.g %>% summarize_at(c("Soil_moist"),mean)
tmsWCavg <- ungroup(tmsWCavg)

tmsWCavg.melt <- melt(tmsWCavg,id.vars = c("time","Loc"))
colnames(tmsWCavg.melt) <- c("time","loc","type","WC")

WCunited <- rbind(terosWCavg,tmsWCavg.melt)
WCunited2 <- left_join(terosWCavg,tmsWCavg.melt)

#######PLOT 6###############
teros6_4 <- filter(terosWCavg,terosWCavg$loc=="6_4")
tms6_4 <- filter(tmsWCavg, tmsWCavg$Loc=="6_4")

WC6_4united <- left_join(teros6_4,tms6_4,by="time")
daysfreeze6_4 <- filter(tmsavg,tmsavg$T3 < 0 & tmsavg$Loc =="6_4")
daysfreeze6_4.list <- unique(daysfreeze$time)

WCnotfrozen6_4 <- WC6_4united[WC6_4united$time %in% daysfreeze.list,]

terostemp6_4 <- filter(terostempavg,terostempavg$loc=="6_4")
tmstemp6_4 <- filter(tmsavg,tmsavg$Loc=="6_4")
temp6_4united <- left_join(terostemp6_4,tmstemp6_4,by="time")

###########################REPRESENTATION###############
#Logaritmic ADJUSTMENT# THIS IS THE MOST RECOMENDED
WC6_4 <- ggplot(na.omit(WCnotfrozen6_4),aes(x=WC,y=Soil_moist,color=type))+
  ggtitle("Plot 6_4")+
  geom_point(size=1)+
  facet_wrap(~type)+
  geom_smooth(method="lm", formula=y ~ log(x))+
  stat_regline_equation(formula = y ~ log(x),aes(label=..rr.label..),position = "identity",label.y=0.32)
WC6_4

tmp6_4 <- ggplot(na.omit(temp6_4united),aes(x=T3,y=temp,color=type))+
  ggtitle("Plot 6_4")+
  geom_point(size=1)+
  facet_wrap(~type)+
  geom_smooth(method="lm", formula=y ~ x)+
  stat_regline_equation(formula = y ~ x,aes(label=..rr.label..),position = "identity",label.y=20)
tmp6_4

######################################PLOT 1_5################################
teros1_5 <- filter(terosWCavg,terosWCavg$loc=="1_5")
tms1_5 <- filter(tmsWCavg, tmsWCavg$Loc=="1_5")

WC1_5united <- left_join(teros1_5,tms1_5,by="time")
daysfreeze1_5 <- filter(tmsavg,tmsavg$T3 < 0 & tmsavg$Loc =="1_5")
daysfreeze1_5.list <- unique(daysfreeze$time)

WCnotfrozen1_5 <- WC1_5united[WC1_5united$time %in% daysfreeze.list,]

terostemp1_5 <- filter(terostempavg,terostempavg$loc=="1_5")
tmstemp1_5 <- filter(tmsavg,tmsavg$Loc=="1_5")
temp1_5united <- left_join(terostemp1_5,tmstemp1_5,by="time")

###########################REPRESENTATION###############
#Logaritmic ADJUSTMENT# THIS IS THE MOST RECOMENDED
WC1_5 <- ggplot(na.omit(WCnotfrozen1_5),aes(x=WC,y=Soil_moist,color=type))+
  ggtitle("Plot 1_5")+
  geom_point(size=1)+
  facet_wrap(~type)+
  geom_smooth(method="lm", formula=y ~ log(x))+
  stat_regline_equation(formula = y ~ log(x),aes(label=..rr.label..),position = "identity",label.y=0.32)
WC1_5

tmp1_5 <- ggplot(na.omit(temp1_5united),aes(x=T3,y=temp,color=type))+
  ggtitle("Plot 1_5")+
  geom_point(size=1)+
  facet_wrap(~type)+
  geom_smooth(method="lm", formula=y ~ x)+
  stat_regline_equation(formula = y ~ x,aes(label=..rr.label..),position = "identity",label.y=20)
tmp1_5


######################################PLOT 3_1 ################################
teros3_1 <- filter(terosWCavg,terosWCavg$loc=="3_1")
tms3_1 <- filter(tmsWCavg, tmsWCavg$Loc=="3_1")

WC3_1united <- left_join(teros3_1,tms3_1,by="time")
daysfreeze3_1 <- filter(tmsavg,tmsavg$T3 < 0 & tmsavg$Loc =="3_1")
daysfreeze3_1.list <- unique(daysfreeze$time)

WCnotfrozen3_1 <- WC3_1united[WC3_1united$time %in% daysfreeze.list,]

terostemp3_1 <- filter(terostempavg,terostempavg$loc=="3_1")
tmstemp3_1 <- filter(tmsavg,tmsavg$Loc=="3_1")
temp3_1united <- left_join(terostemp3_1,tmstemp3_1,by="time")

###########################REPRESENTATION###############
#Logaritmic ADJUSTMENT# THIS IS THE MOST RECOMENDED
WC3_1 <- ggplot(na.omit(WCnotfrozen3_1),aes(x=WC,y=Soil_moist,color=type))+
  ggtitle("Plot 3_1")+
  geom_point(size=1)+
  facet_wrap(~type)+
  geom_smooth(method="lm", formula=y ~ log(x))+
  stat_regline_equation(formula = y ~ log(x),aes(label=..rr.label..),position = "identity",label.y=0.32)
WC3_1

tmp3_1 <- ggplot(na.omit(temp3_1united),aes(x=T3,y=temp,color=type))+
  ggtitle("Plot 3_1")+
  geom_point(size=1)+
  facet_wrap(~type)+
  geom_smooth(method="lm", formula=y ~ x)+
  stat_regline_equation(formula = y ~ x,aes(label=..rr.label..),position = "identity",label.y=20)
tmp3_1


#SAVE ALL THE GRAPHS
path.to.wd <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files"
setwd(path.to.wd)

pdf("Temperatures TMS vs Teros.pdf")
tmp1_5
tmp3_1
tmp6_4
dev.off ()

pdf("Moisture TMS vs Teros.pdf")
WC1_5
WC3_1
WC6_4
dev.off ()


#Lineal ADJUSTMENT
ggplot(na.omit(WCnotfrozen),aes(x=WC,y=Soil_moist,color=type))+
  geom_point()+
  facet_wrap(~type)+
  geom_smooth(method="lm", formula=y~x)+
  stat_regline_equation(aes(label=..rr.label..))
#Exponential ADJUSTMENT
ggplot(na.omit(WCnotfrozen),aes(x=WC,y=Soil_moist))+
  geom_point()+
  facet_wrap(~type)+
  geom_smooth(method="lm", formula=y ~ exp(x))+
  stat_regline_equation(formula=y ~ exp(x),aes(label=..rr.label..))

