library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
path.to.data  <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV"
setwd(path.to.data)
alldata <- fread("Complete_data.csv")

path.to.data  <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files"
setwd(path.to.data)
densities <- fread("Field_tableW_New_Densities.csv")

onlydensities <- densities[,c("sample_code","sample_density")]
# 
# duplicated_names <- duplicated(colnames(alldata))
# uniquenames <- unique(colnames(alldata))
# alldata[uniquenames]
# alldata[!duplicated_names]

all <- left_join(alldata,onlydensities,"sample_code")

colnames(all)[which(names(all)=="sample_density")] <- "DensityKgM3"
colnames(all)[which(names(all)=="RespCorrectedArea")] <- "RespCorrectedAreauMCm2"
colnames(all)[which(names(all)=="RespCorrectedVolume")] <- "RespCorrectedVolumeuMCm3"
all$DensitygmCm3 <- all$DensityKgM3/1000
all$RespCorrectedWeightuMgr <- all$RespCorrectedVolumeuMCm3/all$DensitygmCm3

#Con este calculo pasamos los uM CO2, Cm2 ,S a: gr CO2, M2, minuto
all$RespCorrectedArea_grCO2_M2_min <- all$RespCorrectedAreauMCm2 *10000*0.000044 * 60


#Con este calculo pasamos los uM CO2, Cm3 ,S a: gr CO2, M3, minuto
all$RespCorrectedVolume_grCO2_M3_min <- all$RespCorrectedVolumeuMCm3 * 1000000*0.000044*60


#Con este calculo pasamos los uM CO2, gr ,S a: uM CO2, gr, minuto
all$RespCorrectedWeight_GrCO2_Gr_min <- all$RespCorrectedWeightuMgr*0.000044*60 * 1000000
all$Class <- as.character(all$Class)

filteredR <- filter(all,rsq > 0.8)
ggplot(filteredR,aes(x=T3, y=RespCorrectedWeight_GrCO2_Gr_min,color=Class))+
  geom_point(size=1)+
  geom_smooth(method = "lm")+
  stat_regline_equation(aes(label=..rr.label..))+
  labs(y=expression("g CO"[2]*" min"^-1*"g DW"^-1),x="Temperature ºC")+
  facet_wrap(~Species+DiamClass,scales="free")

ggplot(filteredR,aes(x=Class,y=RespCorrectedWeight_GrCO2_Gr_min,color=Class))+
  geom_boxplot()+
  labs(X="Class",y=expression("g CO"[2]*" min"^-1*"g DW"^-1))+
  facet_wrap(~Species+DiamClass,scales="free")


all <- transform(all,auxmin = pmin(Sample_code1,Sample_code2))
all <- transform(all,auxmax = pmax(Sample_code1,Sample_code2))
all$sample_code_combined <- paste(all$auxmin,"+",all$auxmax,sep = "")
measurementcode <- unique(all$sample_code_combined)
all$auxmin <- NULL

all[all$sample_code_combined=="90+91"]
write.csv(all,"AllYearMeasurementsWDensity.csv",row.names = FALSE)
getwd()
