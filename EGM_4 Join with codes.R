library(data.table)

library(readxl)
library(openxlsx)
library(tidyverse)
path.to.data <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign December/EGM_1/08_12_2021/"
path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign December/EGM_1/10_12_2021/"
path.to.data <- "C:/Users/Javier/Documents/DeWood Git/DeWood/Files/Respiration Campaing April 2022/EGM_1/27_04_2022"
setwd(path.to.data)
dir()
enterdata<- "29_04_2022"
path.to.data <- paste("C:/Users/Javier/Documents/DeWood Git/DeWood/Files/Respiration Campaing April 2022/EGM_1/",enterdata,sep="")
setwd(path.to.data)
#################
#USE THIS FUNCTION IN CASE THERE'S MORE THAN 1 .dat
data_all <- list.files(path=path.to.data,pattern = "*.dat", full.names = TRUE) %>%
  lapply(fread) %>%
  bind_rows

write.table(data_all,paste(enterdata,".dat",sep = ""),append = TRUE, sep = "\t", row.names=FALSE, col.names=TRUE, quote=FALSE)
#######################################################3

lecturas <- fread(paste(enterdata,".dat",sep = ""))
codigos <- read_xlsx(paste("Codes ",enterdata,".xlsx",sep = ""))

result <- left_join(lecturas,codigos,";Plot")

write.xlsx(result,paste("Measurements EGM4 ", enterdata,".xlsx",sep = ""))
