library(data.table)
library(rstatix)
library(dplyr)
library(zoo)
library(multcompView)
library(ggbiplot)
library(ggpubr)
library(corrplot)
library(quantreg)
library(openxlsx)

cleaningup <- function(y)
{
  y <- anti_join(y,malditos,by="ID")
  y$respiration[y$respiration <0]<-0
  y$RespCorrectedArea[y$RespCorrectedArea<0] <- 0
  y$RespCorrectedVolume[y$RespCorrectedVolume<0] <-0
  y$`Bark(%)` <- na.aggregate(y$`Bark(%)`)
  y$Waterpercentage <-na.aggregate((y$Waterpercentage))
  y <- filter(y,rsq > 0.8)
  return(y)
}
tri.to.squ<-function(x)
{
  rn<-row.names(x)
  cn<-colnames(x)
  an<-unique(c(cn,rn))
  myval<-x[!is.na(x)]
  mymat<-matrix(1,nrow=length(an),ncol=length(an),dimnames=list(an,an))
  for(ext in 1:length(cn))
  {
    for(int in 1:length(rn))
    {
      if(is.na(x[row.names(x)==rn[int],colnames(x)==cn[ext]])) next
      mymat[row.names(mymat)==rn[int],colnames(mymat)==cn[ext]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
      mymat[row.names(mymat)==cn[ext],colnames(mymat)==rn[int]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
    }
  }
  return(mymat)
}

ID <- c("F69_102+109_7_29","88_369_7_28","37_140_7_27")

malditos <- data.frame(ID)


  path.to.data1 <- path.to.wd <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/"
path.to.data2 <- path.to.data <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign October 2021/"
setwd(path.to.data1)
data1 <- fread("resultadofinalJulio.csv")
setwd(path.to.data2)
data2 <- fread("RESULTADOFINALOctubre.csv")
gathered_data <- rbind.fill(data1,data2)

gathered_data$Type <- paste(gathered_data$Species,gathered_data$DiamClass,gathered_data$Class,sep="_")
j <- filter(gathered_data,Species=="FS",DiamClass=="1")
j <- cleaningup(j)
j$Class <- as.character(j$Class)
temascatter <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                     legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                     legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                     axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                     axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))
sp <- ggplot(j, aes(T3, RespCorrectedArea,color = Class)) + 
  theme_bw() +
  geom_point(aes(color = Class),size=2) +
  labs(y="Respiration µm CO2/s*Area (CM^2)",x="Temperature ºC")+
  facet_wrap(~Class)+
  ylim(0,7)

sp
