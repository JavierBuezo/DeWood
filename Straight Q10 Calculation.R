library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(EnvStats)

path.to.data  <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV"
setwd(path.to.data)
all <- fread("AllYearMeasurementsWDensity.csv")
all$Class <- as.character(all$Class)
filteredR <- filter(all,rsq > 0.8)
#############
#########Representation in boxplot and scatterplot of the respirations

ggplot(filteredR,aes(x=T3, y=RespCorrectedWeight_GrCO2_KGr_Year,color=Class))+
  geom_point(size=1)+
  geom_smooth(method = "lm")+
  stat_regline_equation(aes(label=..rr.label..))+
  labs(y=expression("g CO"[2]*" min"^-1*"g DW"^-1),x="Temperature ºC")+
  facet_wrap(~Species+DiamClass,scales="free")

ggplot(filteredR,aes(x=Class,y=RespCorrectedWeight_GrCO2_KGr_Year,color=Class))+
  geom_boxplot()+
  labs(X="Class",y=expression("g CO"[2]*" min"^-1*"g DW"^-1))+
  facet_wrap(~Species+DiamClass,scales="free")

##############Drawing of all the Q10

allJuly <- filter(all,all$Month=="7")
allOctober <- filter(all,all$Month=="10")
allDecember <- filter(all,all$Month=="12")
allApril <- filter(all,all$Month=="4")

prueba <- subset(all,duplicated(sample_code_combined)|duplicated(sample_code_combined,fromLast=TRUE))
listresp <- split(prueba, prueba$sample_code_combined)



Q10_all <- lapply(listresp, function(x){
  
  toptemp <- x[which.max(x$T3),]
  bottemp <- x[which.min(x$T3),]
  q10value <- (toptemp$RespCorrectedWeight_GrCO2_KGr_Year/bottemp$RespCorrectedWeight_GrCO2_KGr_Year) ^ (10/(toptemp$T3-bottemp$T3))

})

colnames(all)
q10 <- do.call(rbind.data.frame,Q10_all)
q10 <- bind_rows(Q10_all)
q10.t <- t(q10)
ncol(q10.t)
q10.t.df <- melt(q10.t)
q10.t.df$Var2 <- NULL
colnames(q10.t.df) <- c("sample_code_combined","Q10")
datafiltered <- all[,c("sample_code","Code","Class","Species","DiamClass","sample_code_combined")]

q10join <- inner_join(q10.t.df,datafiltered,by="sample_code_combined")
q10distinct <- distinct(q10join)

####The Q10 used are filtered. Anything above 10 is considered outlayer, including INF (errors in calculation) and NA
q10distinct2 <- q10distinct[!is.infinite(q10distinct$Q10),]
q10distinct3 <- filter(q10distinct2,q10distinct2$Q10 < 10)
q10distinct3 <- filter(q10distinct3,q10distinct3$Q10 != 0)
###Also delete the replicated rows the 1 cm have
q10distinct4 <- q10distinct3[!duplicated(q10distinct3$sample_code_combined),]


q10year <- ggplot(na.omit(q10distinct4),aes(x=Class,y=Q10,color=Class))+
  geom_boxplot()+
  labs(X="Class",y=expression("Q10 average all year"))+
  stat_n_text()+
  facet_wrap(~Species+DiamClass)
q10year
write.csv(q10distinct4,"q10straightAvg_filtered.csv",row.names = FALSE)
getwd()
###########################################
#############Q10 July


prueba <- subset(allJuly,duplicated(sample_code_combined)|duplicated(sample_code_combined,fromLast=TRUE))
listresp <- split(prueba, prueba$sample_code_combined)
Q10_all <- lapply(listresp, function(x){
  
  toptemp <- x[which.max(x$T3),]
  bottemp <- x[which.min(x$T3),]
  q10value <- (toptemp$RespCorrectedWeight_GrCO2_KGr_Year/bottemp$RespCorrectedWeight_GrCO2_KGr_Year) ^ (10/(toptemp$T3-bottemp$T3))
  
})

colnames(all)
q10 <- do.call(rbind.data.frame,Q10_all)
q10 <- bind_rows(Q10_all)
q10.t <- t(q10)
ncol(q10.t)
q10.t.df <- melt(q10.t)
q10.t.df$Var2 <- NULL
colnames(q10.t.df) <- c("sample_code_combined","Q10")
datafiltered <- all[,c("sample_code","Code","Class","Species","DiamClass","sample_code_combined")]

q10join <- inner_join(q10.t.df,datafiltered,by="sample_code_combined")
q10distinct <- distinct(q10join)

####The Q10 used are filtered. Anything above 10 is considered outlayer, including INF (errors in calculation) and NA
q10distinct2 <- q10distinct[!is.infinite(q10distinct$Q10),]
q10distinct3 <- filter(q10distinct2,q10distinct2$Q10 < 10)
q10distinct3 <- filter(q10distinct3,q10distinct3$Q10 != 0)
###Also delete the replicated rows the 1 cm have
q10distinct4 <- q10distinct3[!duplicated(q10distinct3$sample_code_combined),]


plotJuly<- ggplot(na.omit(q10distinct4),aes(x=Class,y=Q10,color=Class))+
  geom_boxplot()+
  labs(X="Class",y=expression("Q10 average July"))+
  stat_n_text()+
  facet_wrap(~Species+DiamClass)


plotJuly
##########################
########Q10 October
prueba <- subset(allOctober,duplicated(sample_code_combined)|duplicated(sample_code_combined,fromLast=TRUE))
listresp <- split(prueba, prueba$sample_code_combined)
Q10_all <- lapply(listresp, function(x){
  
  toptemp <- x[which.max(x$T3),]
  bottemp <- x[which.min(x$T3),]
  q10value <- (toptemp$RespCorrectedWeight_GrCO2_KGr_Year/bottemp$RespCorrectedWeight_GrCO2_KGr_Year) ^ (10/(toptemp$T3-bottemp$T3))
  
})

colnames(all)
q10 <- do.call(rbind.data.frame,Q10_all)
q10 <- bind_rows(Q10_all)
q10.t <- t(q10)
ncol(q10.t)
q10.t.df <- melt(q10.t)
q10.t.df$Var2 <- NULL
colnames(q10.t.df) <- c("sample_code_combined","Q10")
datafiltered <- all[,c("sample_code","Code","Class","Species","DiamClass","sample_code_combined")]

q10join <- inner_join(q10.t.df,datafiltered,by="sample_code_combined")
q10distinct <- distinct(q10join)

####The Q10 used are filtered. Anything above 10 is considered outlayer, including INF (errors in calculation) and NA
q10distinct2 <- q10distinct[!is.infinite(q10distinct$Q10),]
q10distinct3 <- filter(q10distinct2,q10distinct2$Q10 < 10)
q10distinct3 <- filter(q10distinct3,q10distinct3$Q10 != 0)
###Also delete the replicated rows the 1 cm have
q10distinct4 <- q10distinct3[!duplicated(q10distinct3$sample_code_combined),]


plotOctober<- ggplot(na.omit(q10distinct4),aes(x=Class,y=Q10,color=Class))+
  geom_boxplot()+
  labs(X="Class",y=expression("Q10 average October"))+
  stat_n_text()+
  facet_wrap(~Species+DiamClass)


plotOctober

##########################
########Q10 December
prueba <- subset(allDecember,duplicated(sample_code_combined)|duplicated(sample_code_combined,fromLast=TRUE))
listresp <- split(prueba, prueba$sample_code_combined)
Q10_all <- lapply(listresp, function(x){
  
  toptemp <- x[which.max(x$T3),]
  bottemp <- x[which.min(x$T3),]
  q10value <- (toptemp$RespCorrectedWeight_GrCO2_KGr_Year/bottemp$RespCorrectedWeight_GrCO2_KGr_Year) ^ (10/(toptemp$T3-bottemp$T3))
  
})

colnames(all)
q10 <- do.call(rbind.data.frame,Q10_all)
q10 <- bind_rows(Q10_all)
q10.t <- t(q10)
ncol(q10.t)
q10.t.df <- melt(q10.t)
q10.t.df$Var2 <- NULL
colnames(q10.t.df) <- c("sample_code_combined","Q10")
datafiltered <- all[,c("sample_code","Code","Class","Species","DiamClass","sample_code_combined")]

q10join <- inner_join(q10.t.df,datafiltered,by="sample_code_combined")
q10distinct <- distinct(q10join)

####The Q10 used are filtered. Anything above 10 is considered outlayer, including INF (errors in calculation) and NA
q10distinct2 <- q10distinct[!is.infinite(q10distinct$Q10),]
q10distinct3 <- filter(q10distinct2,q10distinct2$Q10 < 10)
q10distinct3 <- filter(q10distinct3,q10distinct3$Q10 != 0)
###Also delete the replicated rows the 1 cm have
q10distinct4 <- q10distinct3[!duplicated(q10distinct3$sample_code_combined),]


plotDecember<- ggplot(na.omit(q10distinct4),aes(x=Class,y=Q10,color=Class))+
  geom_boxplot()+
  labs(X="Class",y=expression("Q10 average December"))+
  stat_n_text()+
  facet_wrap(~Species+DiamClass)


plotDecember
###############################
########Q10 April
prueba <- subset(allApril,duplicated(sample_code_combined)|duplicated(sample_code_combined,fromLast=TRUE))
listresp <- split(prueba, prueba$sample_code_combined)
Q10_all <- lapply(listresp, function(x){
  
  toptemp <- x[which.max(x$T3),]
  bottemp <- x[which.min(x$T3),]
  q10value <- (toptemp$RespCorrectedWeight_GrCO2_KGr_Year/bottemp$RespCorrectedWeight_GrCO2_KGr_Year) ^ (10/(toptemp$T3-bottemp$T3))
  
})

colnames(all)
q10 <- do.call(rbind.data.frame,Q10_all)
q10 <- bind_rows(Q10_all)
q10.t <- t(q10)
ncol(q10.t)
q10.t.df <- melt(q10.t)
q10.t.df$Var2 <- NULL
colnames(q10.t.df) <- c("sample_code_combined","Q10")
datafiltered <- all[,c("sample_code","Code","Class","Species","DiamClass","sample_code_combined")]

q10join <- inner_join(q10.t.df,datafiltered,by="sample_code_combined")
q10distinct <- distinct(q10join)

####The Q10 used are filtered. Anything above 10 is considered outlayer, including INF (errors in calculation) and NA
q10distinct2 <- q10distinct[!is.infinite(q10distinct$Q10),]
q10distinct3 <- filter(q10distinct2,q10distinct2$Q10 < 10)
q10distinct3 <- filter(q10distinct3,q10distinct3$Q10 != 0)
###Also delete the replicated rows the 1 cm have
q10distinct4 <- q10distinct3[!duplicated(q10distinct3$sample_code_combined),]


plotApril<- ggplot(na.omit(q10distinct4),aes(x=Class,y=Q10,color=Class))+
  geom_boxplot()+
  labs(X="Class",y=expression("Q10 average April"))+
  stat_n_text()+
  facet_wrap(~Species+DiamClass)


plotApril

getwd()
