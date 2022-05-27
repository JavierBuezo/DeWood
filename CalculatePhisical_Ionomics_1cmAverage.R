library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(readxl)
library(tidyr)
fieldtable <- fread("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Field_tableW_New_Densities.csv")
fieldtableSelect <- fieldtable[,c("sample_code","Code","FW..g." ,"DW..g.","Bark....","Water.content..g.","Water.percentage","Plot","SubPlot","Class","Species","DiamClass","sample_density")]
fieldtable10.25 <- fieldtableSelect %>% filter(!DiamClass == "1")
fieldtable1 <- fieldtableSelect %>% filter(DiamClass =="1")

onecmeq <- read_excel("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/1cm_equivalent_table.xlsx")

fieldtable1$Code <- paste(fieldtable1$Plot,fieldtable1$SubPlot,fieldtable1$Species,fieldtable1$Class,sep = "")
fieldtable10.25$Code <- paste(fieldtable10.25$Plot,fieldtable10.25$SubPlot,fieldtable10.25$Species,fieldtable10.25$Class,sep = "")

aux <- onecmeq[,c("SampleEq","Code")]

fieldtable1join <- left_join(fieldtable1,aux,by = "Code") 
fieldtable1join[is.na(fieldtable1join)] <- 0

#Calculate the mean of the 4 samples to give a number to the combination. This function ignores the 0s. Solution to solve the problem with NA's found in one lost subplot 
medias <- aggregate(cbind(FW..g. ,DW..g.,Bark....,Water.content..g.,Water.percentage,sample_density,SampleEq)~Code,data = fieldtable1join,FUN=(function(x){ifelse(sum(x==0)>0 & sum(x !=0) >0, mean(x[x>0]), mean(x))}))
medias$DiamClass <- 1
medias$Plot <- substr(medias$Code,1,1)
medias$SubPlot <- substr(medias$Code,2,2)
medias$Species <- substr(medias$Code,3,4)
medias$Class <- substr(medias$Code,5,5)
medias$sample_code <- medias$SampleEq

fieldtable10.25$SampleEq <- NA
#Combine the new dataframes of 25, 10 cm with the average table of 1cm 
medias$sample_density <- NULL
fieldtable10.25$sample_code <- as.character(fieldtable10.25$sample_code)
fieldtable10.25$Plot <- as.character(fieldtable10.25$Plot)
fieldtable10.25$SubPlot <- as.character(fieldtable10.25$SubPlot)
fieldtable10.25$Class <- as.character(fieldtable10.25$Class)
total <- rbind(fieldtable10.25,medias)
total$SampleEq <- NULL
colnames(CarbonNitrogen)
CarbonNitrogen <- read_excel("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/CEBAS Ionomics/CEBAS IONOMICS/Full Ionomics.xlsx",sheet = 1)
CarbonNitrogen <- CarbonNitrogen[,c("Ntotal (g/100g)","Ctotal (g/100g)","DeWood code")]
colnames(CarbonNitrogen)[3]<-"sample_code"
CarbonNitrogen$sample_code <- as.character(CarbonNitrogen$sample_code)
total2 <- left_join(total,CarbonNitrogen,by="sample_code")

total3 <- filter(total2,total2$Plot == 1 | total2$Plot == 3 | total2$Plot == 6)

Ionomics <- read_excel("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/CEBAS Ionomics/CEBAS IONOMICS/Full Ionomics.xlsx",sheet = 2)

Ionomics$Diameter <- NULL
colnames(Ionomics)[1] <- "sample_code"
Ionomics$sample_code <- as.character(Ionomics$sample_code)
total4 <- left_join(total3,Ionomics,by = "sample_code")
write.csv(total4,"All_PhysicalTraits_CN_Ionomics.csv",row.names = FALSE)
getwd()




