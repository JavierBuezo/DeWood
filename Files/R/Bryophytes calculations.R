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

#########Leer los resultados
path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Final results/CSV/"
setwd(path.to.data)
respiraciones <-fread("FinalResults_WDensities.csv")
respiraciones$V1 <- NULL
#Con este calculo pasamos los uM CO2, Cm2 ,S a: gr CO2, M2, minuto
respiraciones$RespCorrectedArea <- respiraciones$RespCorrectedArea *10000*0.000044 * 60

#Con este calculo pasamos los uM CO2, Cm3 ,S a: gr CO2, M3, minuto
respiraciones$RespCorrectedVolume <- respiraciones$RespCorrectedVolume *1000000*0.000044*60


#Con este calculo pasamos los uM CO2, gr ,S a: ug CO2, gr DW, minuto
respiraciones$RespCorrectedgrams <- respiraciones$RespCorrectedgrams *0.000044*60 * 1000000



respiraciones <- filter(respiraciones,respiraciones$DiamClass == 10)

respiraciones$type <- paste(respiraciones$Species,respiraciones$Class,sep = "_") 
respiraciones <- filter(respiraciones, respiraciones$Month == 12)


respiraciones <- respiraciones %>% drop_na(RespCorrectedgrams)

  
brioresp <- respiraciones[str_sub(respiraciones$Code,start = -1) == "b",]
restresp <- respiraciones[!str_sub(respiraciones$Code,start = -1) == "b",]


respiraciones[str_sub(respiraciones$Code,start = -1) == "b"]$type <- paste(respiraciones[str_sub(respiraciones$Code,start = -1) == "b"]$type,"_b",sep = "")

briolist <- split(brioresp,brioresp$type)
restlist <- split(restresp, restresp$type)

briomeans <- lapply(briolist, function (x) mean(x$RespCorrectedgrams))
briomeansdf <- rbind.data.frame(briomeans)

restmeans <- lapply(restlist, function (x) mean(x$RespCorrectedgrams))
restmeansdf <- rbind.data.frame (restmeans)

briosd <- lapply(briolist, function (x) sd(na.omit(x$RespCorrectedgrams))/sqrt(length(x)))
briosddf <- rbind.data.frame(briosd)

restsd <- lapply(restlist, function (x) sd(na.omit(x$RespCorrectedgrams))/sqrt(length(x)))
restsddf <- rbind.data.frame(restsd)


briomeanssd <- merge(briomeansdf,briosddf)

writeClipboard(as.character(briomeansdf))
writeClipboard(as.character(briosddf))
writeClipboard(as.character(restmeansdf))
writeClipboard(as.character(restsddf))

################# Para la estadística ######################

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

respiraciones[str_sub(respiraciones$Code,start = -1) == "b"]$type <- paste(respiraciones[str_sub(respiraciones$Code,start = -1) == "b"]$type,"_b",sep = "")

resultado <- pairwise.wilcox.test(respiraciones$RespCorrectedgrams, respiraciones$type,p.adjust.method = "BH")
resultado[["p.value"]]

mymat <- tri.to.squ(resultado$p.value)
multcompLetters(mymat)
