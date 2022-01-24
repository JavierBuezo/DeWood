library(data.table)

library(readxl)
library(openxlsx)
library(tidyverse)
path.to.data <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign December/EGM_1/08_12_2021/"
path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign December/EGM_1/10_12_2021/"
setwd(path.to.data)
dir()
lecturas <- fread("10_12_2021.dat")
codigos <- read_xlsx("Codes 10_12_2021.xlsx")

result <- left_join(lecturas,codigos,";Plot")

write.xlsx(result,"Measurements EGM4 10_12_2021.xlsx")
