library(tidyverse)
#establecer el directorio de trabajo
path.to.wd <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/EGM_1"
setwd(path.to.wd)

# guardar la dirección a la carpeta en la que están los archivos
path.to.data <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/EGM_1"

# cargar todos los archivos de la carpeta
files_path <- list.files(path.to.data, full.names = T, pattern = ".xlsx")

#Crear regresion lineal
lapply(files_path, function(x){ tmp <- readxl::read_xlsx(x)
lm(tmp$CO2 Ref, tmp$Sample Code)}

