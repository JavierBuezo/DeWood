library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(EnvStats)


path.to.data  <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files"
setwd(path.to.data)
all <- fread("Field_tableW_New_Densities.csv")
all$type <- paste(all$DiamClass,all$Species,all$Class,sep="_")
all$Class <- as.character(all$Class)
ggplot(all,aes(x=Class,y=sample_density,color=Class))+
  geom_boxplot()+
  labs(X="Class",y=expression("Kg M"^-3))+
  facet_wrap(~Species+DiamClass,scales="free")

