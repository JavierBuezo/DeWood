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
library(stringr)


path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign December/"
setwd(path.to.data)
respiraciones <-fread("RESULTADOFINALDiciembre.csv")

listresp <- split(respiraciones, respiraciones$sample_code)

Q10_all <- lapply(listresp, function(x){
  
  toptemp <- x[which.max(x$T3),]
  bottemp <- x[which.min(x$T3),]
  
  q10value <- (toptemp$RespCorrectedArea/bottemp$RespCorrectedArea) ^ (10/(toptemp$T3-bottemp$T3))
  
})
