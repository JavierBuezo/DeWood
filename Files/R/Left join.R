library(tidyverse)

path.to.data <- "C:/Users/Fomare/OneDrive - UPNA/DeWood Project/R/Pruebas/"
setwd(path.to.data)

tablaorigen <- read_xlsx("Tabla Origen leftjoin.xlsx")
tablanombres <- read_xlsx("Tabla nombres.xlsx")

resultado <- left_join(tablaorigen, tablanombres, by="COD")
write.csv(resultado,"Resultadotrasjoin.csv")
