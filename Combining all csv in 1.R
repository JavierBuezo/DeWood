library(ggplot2)
library(dplyr)
library(readxl) # 1
library(tidyverse)
library(fs)
library(xlsx)
library(tidyr)
library(stringi)
library(readr)

path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Final results/CSV/Old/"
path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign 09 September/"
path.to.data <- "C:/Users/Javier/Documents/DeWood Git/DeWood/Files/Final results/CSV/Old/"
path.to.data  <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV/Old"
#path.to.data <- "C:/Users/NG.5027073/Dropbox (MNCN CSIC)/pr?ce/Nagore uam/INVESTIGACION/2020 - DeWood Romania/EGM 4/"
#Leemos el excell con los codigos de plot y muestra, separados por fecha
setwd(path.to.data)


path.to.data <- getwd()

#Cargar todos los archivos xlsx de la carpeta
files_path <- list.files(path.to.data, full.names = T, pattern = ".csv")
#Guardar el nombre del archivo para usarlo despues como titulo
files_nm <- list.files(path.to.data, pattern = ".csv")

#Esta parte de código es para comprobar manualmente los datasets y encontrar posibles problemas.
Julio <- fread("RESULTADOFINALJulio.csv")
Diciembre <- fread("RESULTADOFINALDiciembre.csv")
Octubre <- fread("RESULTADOFINALOctubre.csv")
Abril <- fread("RESULTADOFINALAbril_3.csv")
colnames(Abril)[which(names(Abril)=="Subplot2")] <- "Subplot"
Abril$V1 <- NULL


allmeasurements <- do.call("rbind",list(Julio,Octubre,Diciembre,Abril))
allmeasurements$V1 <- NULL
write.csv(allmeasurements, "Complete_Data.csv",row.names = FALSE)

#Esto automatiza y une todos los datasets de una carpeta
AllData <- list.files(path.to.data, full.names = TRUE) %>% lapply(fread) %>% 
            bind_rows()


write.csv(AllData, "Complete_Data.csv")
