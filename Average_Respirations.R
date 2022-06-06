library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(EnvStats)

##############How the averages were calculated###########
path.to.data  <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV"
setwd(path.to.data)
all <- fread("AllYearMeasurementsWDensity.csv")
all$Class <- as.character(all$Class)
filteredR <- filter(all,rsq > 0.8)
filteredR$Type <- paste(filteredR$Month,filteredR$DiamClass,filteredR$Species,filteredR$Class,sep = "_") 
library(doBy)
cdata <- summaryBy(RespCorrectedWeight_GrCO2_KGr_Year ~ Type+Month, data=filteredR, FUN=c(length,mean,sd))
cdata$se <-cdata$RespCorrectedWeight_GrCO2_KGr_Year.sd/sqrt(cdata$RespCorrectedWeight_GrCO2_KGr_Year.length) 
write.csv(cdata,"MeanResp_AllYear_AllGroups.csv",row.names = FALSE)
################################
###########Work with the data
path.to.data  <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV"
setwd(path.to.data)
cdata <- fread("MeanResp_AllYear_AllGroups.csv")

ggplot(cdata, aes(x=Type, y=RespCorrectedWeight_GrCO2_KGr_Year.mean, fill=Type)) + 
  geom_bar(stat="identity", color="black",
           position=position_dodge()) +
  geom_errorbar(aes(ymin=RespCorrectedWeight_GrCO2_KGr_Year.mean-se, ymax=RespCorrectedWeight_GrCO2_KGr_Year.mean+se), width=.2,
                position=position_dodge(.9))
#############################