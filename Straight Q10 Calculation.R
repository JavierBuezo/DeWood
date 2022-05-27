library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)

path.to.data  <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV"
setwd(path.to.data)
all <- fread("AllYearMeasurementsWDensity.csv")
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

prueba <- subset(all,duplicated(sample_code_combined)|duplicated(sample_code_combined,fromLast=TRUE))
listresp <- split(prueba, prueba$sample_code_combined)



Q10_all <- lapply(listresp, function(x){
  
  toptemp <- x[which.max(x$T3),]
  bottemp <- x[which.min(x$T3),]
  q10value <- (toptemp$RespCorrectedWeight_GrCO2_Gr_min/bottemp$RespCorrectedWeight_GrCO2_Gr_min) ^ (10/(toptemp$T3-bottemp$T3))

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

q10distinct4 <- q10distinct4[!q10distinct4$sample_code_combined=="595+598",]
install.packages("EnvStats")
library("EnvStats")
ggplot(na.omit(q10distinct4),aes(x=Class,y=Q10,color=Class))+
  geom_boxplot()+
  labs(X="Class",y=expression("Q10 average"))+
  stat_n_text()+
  facet_wrap(~Species+DiamClass)

write.csv(q10distinct3,"q10straightAvg_filtered.csv",row.names = FALSE)
getwd()



uniqueprueba <- unique(q10distinct4$sample_code_combined)
n_occur <- data.frame(table(q10distinct4$sample_code_combined))

prueba <- filter(q10distinct,q10distinct =="102+109")
prueba <- filter(all,all$sample_code_combined=="284+286")
#Intento de contour plot
install.packages("plotly")
library(reshape2)
install.packages("metR")
library(metR)
q10distinct3$Class <- as.integer(q10distinct3$Class)

ggplot(q10distinct3,aes(x=Class,Y=DiamClass))+
  geom_raster(aes(fill = Q10)) + 
  geom_contour(aes(z = Q10), colour = "white", size = 0.2, alpha = 0.5) + 
  geom_text_contour(aes(z = Q10),  colour = "white" ) +
  labs(x = "Decay Class", 
       y = "Diameter", 
       fill = "Q10") + 
  theme(legend.title = element_text(size = 10, face = "bold"), 
        legend.position = "top", panel.background = element_blank(), 
        axis.text = element_text(colour = "black", size = 10, face = "bold"), 
        axis.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 11), legend.key = element_blank()) + 
  scale_fill_continuous(low = "#BFE1B0", high = "#137177") + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) 

# q10join <- NULL

# vct <- unique(all$sample_code_combined)
# q10 <- lapply(vct,function(x){
#   print(x)
#   allfiltered <- all[all$sample_code_combined == x]
#   auxmax <- allfiltered[which.max(all$T3),]
#   auxmin <- allfiltered[which.min(all$T3),]
#   
# })
