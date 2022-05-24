library(openxlsx)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plyr)
path.to.data <- "C:/Users/NG.5027073/Dropbox (SCENIC MNCN CSIC)/eclipseworkspace/DeWood/DeWood/Files/Final results/CSV"
path.to.data  <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/CEBAS Ionomics/CEBAS IONOMICS/"
path.to.data  <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV"
setwd(path.to.data)

CNdata <- read.xlsx("Full Ionomics.xlsx",1)
CNdata$CNratio <- CNdata$`Ctotal.(g/100g)`/CNdata$`Ntotal.(g/100g)`
na.omit(CNdata) %>% 
  ggplot(aes(Class,`Ctotal.(g/100g)`))+
  stat_summary(fun="mean",geom="bar", alpha=.7)+
  stat_summary(fun.data = "mean_cl_normal",
               geom="errorbar",
               width=.2)+
  coord_cartesian(ylim=c(45,50))+
  # geom_bar(stat = "summary",fun="mean")+
  # geom_errorbar(stat= "summary", fun="mean_se")+
  facet_wrap(~Diameter+Species,ncol=2)
scales="free"