library(ggplot2)
library(dplyr)
library(readxl) # 1
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
library(agricolae)
library(Matrix)
library(lme4)
library(GGally)
library(carData)
library(car)
library(PMCMRplus)
library(onewaytests)
library(corrplot)
library(DunnettTests)
library(ggpubr)
library(rstatix)
library(factoextra)
library(ggforce)
library(zoo)
#path.to.data <- "C:/Users/NG.5027073/Dropbox (MNCN CSIC)/práce/Nagore uam/INVESTIGACION/2020 - DeWood Romania/EGM 4/"



#Leemos el excell con los codigos de plot y muestra, separados por fecha
path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/"
path.to.data <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/"
setwd(path.to.data)
dir()
full_df <- fread("PREVIORESULTADOFINAL.CSV")
full_df$V1 <- NULL
full_prepared <- full_df[ , c("Bark(%)","ID","Waterpercentage","Class", "DiamClass","Species","T3","Soil_moist","RespCorrectedArea","Plot...16","SubPlot","rsq","RespCorrectedVolume")]  
full_prepared <- full_df

full_prepared %>% group_by(DiamClass) %>%
        summarise_at(vars(respiration),
                     list(name=mean))
full_prepared %>% group_by(DiamClass) %>%
        summarise_at(vars(RespCorrectedArea),
                     list(name=mean))
full_prepared %>% group_by(DiamClass) %>%
        summarise_at(vars(RespCorrectedVolume),
                     list(name=mean))

colorsclass <- c("#FDAE61", # Orange
                 "#D9EF8B", # Light green
                 "#66BD63") #Dark Green


#####################SEPARAR ENTRE ESPECIES TODO EL ARCHIVO#####################

                j <- filter(full_prepared, Species =="AA",DiamClass =="1")

#Lista de outliers para eliminar#
ID <- c("F69_102+109_7_29","88_369_7_28","37_140_7_27")

malditos <- data.frame(ID)


j <- anti_join(j,malditos,by="ID")
j$respiration[j$respiration <0]<-0
j$RespCorrectedArea[j$RespCorrectedArea<0] <- 0
j$RespCorrectedVolume[j$RespCorrectedVolume<0] <-0
j$`Bark(%)` <- na.aggregate(j$`Bark(%)`)
j$Waterpercentage <-na.aggregate((j$Waterpercentage))
###############################################################################


#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables

data_summary <- function(data, varname, groupnames){
        require(plyr)
        summary_func <- function(x, col){
                c(mean = mean(x[[col]], na.rm=TRUE),
                  sd = sd(x[[col]], na.rm=TRUE))
        }
        data_sum<-ddply(data, groupnames, .fun=summary_func,
                        varname)
        data_sum <- rename(data_sum, c("mean" = varname))
        return(data_sum)
}

prueba <- data_summary(j,"RespCorrectedArea","Class")

ca <- j %>%
        group_by(Class) %>%
        summarise(
                mean = mean(RespCorrectedArea),
                sd = sd(RespCorrectedArea),
                n = n(),
                se = sd / sqrt(n)
        )


############    ScatterPlot  Resp Vs Temp   ################


temascatter <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                                     legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                                     legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                                     axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                                     axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))
sp <- ggplot(j, aes(T3, RespCorrectedArea)) + 
        temascatter +
        geom_point(aes(color = Class)) +
        labs(y="Respiration µm CO2/s*Area (CM^2)",x="Temperature ºC")
        
png("resultado.png", units="in", width=5, height=5, res=300)
sp
dev.off()

###################### Export averages to csv   ##################
j <- filter(full_prepared, Species =="FS",DiamClass =="25")
# j <- filter(j,Waterpercentage != 100,Waterpercentage >0) #Limpiar datos vacíos o que faltan
ID <- c("F69_102+109_7_29","88_369_7_28","37_140_7_27")
malditos <- data.frame(ID)
j <- anti_join(j,malditos,by="ID")
j$respiration[j$respiration <0]<-0
j$RespCorrectedArea[j$RespCorrectedArea<0] <- 0
j$RespCorrectedVolume[j$RespCorrectedVolume<0] <-0

# medias_sd <- j %>% group_by(Class) %>% 
#         dplyr::summarise_at(vars(T3,RespCorrectedArea,Waterpercentage,`Bark(%)`),list(mean=mean,sd=sd,n=n(),se=sd/sqrt(n)))
# j %>% group_by(Class) %>%
#         summarise_at(c("T3","RespCorrectedArea"),mean=mean,sd=sd)


j <- j[ , c("Class","Bark(%)","Waterpercentage","T3","Soil_moist","RespCorrectedArea")]           
j <- filter(j,Waterpercentage != 100,Waterpercentage >0)
prueba <- na.omit(j) %>% group_by(Class) %>% 
        summarise_each(funs(mean,sd,n=n()))
prueba
setwd("D:/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/Results CSV")
write.csv(prueba,"Means and sd FS 25cm.csv")



###################     Bar plot of averages ########

#################       Resumen de los datos    ###################
setwd("D:/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/Figures second round")

j <- filter(full_prepared, Species =="AA",DiamClass =="1")
# j <- filter(j,Waterpercentage != 100,Waterpercentage >0) #Limpiar datos vacíos o que faltan
ID <- c("F69_102+109_7_29","88_369_7_28","37_140_7_27")
malditos <- data.frame(ID)
j <- anti_join(j,malditos,by="ID")
j$respiration[j$respiration <0]<-0
j$RespCorrectedArea[j$RespCorrectedArea<0] <- 0
j$RespCorrectedVolume[j$RespCorrectedVolume<0] <-0
j$Class <- as.character(j$Class)

resumen <- data_summary(j,varname="RespCorrectedArea",groupnames="Class")


sp <- ggplot(resumen,aes(x=Class,y=Waterpercentage))+
        geom_errorbar(aes(ymin=Waterpercentage, ymax=Waterpercentage+sd), width=.1,
                      position=position_dodge(.9))+ 
        geom_bar(stat = "identity",width=0.5,fill = c("red4","springgreen4","steelblue"),color="black")+
        # geom_bar(position_dodge, stat = "identity", 
        #          color="black", show.legend = FALSE)+
        theme_minimal()
sp 
sp.labs <- sp + labs( x = "Class", y = "Water (%)")
png("FS 25cm Waterpercentage.png", units="in", width=5, height=5, res=300)
sp.labs
dev.off()

#################################################

jmeansd <- j
getwd()


################Outlier detection and dirty elimination##############################################
# 
# outliers <- jmeansd %>% group_by(Class) %>% identify_outliers(RespCorrectedArea)
# ?identify_outliers
# 
# jmeansd <- anti_join(jmeansd,outliers,by="correctedArea")

##############################################################





par(mfrow=c(2,2))
ggplot(medias_sd, aes(x=Plot...16,y="T3_mean")) +
                geom_errorbar(aes(ymin=T3_mean-T3_sd,ymax=T3_mean+T3_sd),width=.2)+
                geom_line()+
                geom_point()

ncol(jmeansd)
ncol(medias_sd)
medias_sd <- (jmeansd[,sapply(jmeansd, function(x) list(mean=mean(x),sd=sd(x))),by="Plot...16"])

medias_sd <- aggregate(. ~ DiamClass,jmeansd,function(x) c(mean=mean(x), sd=sd(x)))
warnings()
resultado
png("resultado.png", units="in", width=5, height=5, res=300)
plot(j$T3,j$correctedArea,main="Respiration vs Temperature 10cm",xlab = "Temperature",ylab = "Respiration Corrected by Area")
dev.off()
j$Species <- NULL


###########################     PCA     #################

j <- filter(full_prepared, Species =="AA",DiamClass =="1")

#Lista de outliers para eliminar#
ID <- c("F69_102+109_7_29","88_369_7_28","37_140_7_27")


malditos <- data.frame(ID)


j <- anti_join(j,malditos,by="ID")
j$respiration[j$respiration <0]<-0
j$RespCorrectedArea[j$RespCorrectedArea<0] <- 0
j$RespCorrectedVolume[j$RespCorrectedVolume<0] <-0
j$`Bark(%)` <- na.aggregate(j$`Bark(%)`)
j$Waterpercentage <-na.aggregate((j$Waterpercentage))

j <- j[ , c("Bark(%)","Waterpercentage","T3","Soil_moist","RespCorrectedArea")]
pca <- prcomp(na.omit(j), center = TRUE, scale. = TRUE)
print(pca)
plot(pca, type = "l")
summary(pca)
biplot(pca, scale = 0)

pc1 <- apply(pca$rotation[,1]*j,1, sum)
pc2 <- apply(pca$rotation[,2]*j,1, sum)
j$pc1 <- pc1
j$pc2 <- pc2

fviz_pca_ind(pca, geom.ind = "point", 
             col.ind = "#FC4E07", 
             axes = c(1, 2), 
             pointsize = 1.5) 
fviz_pca_var(pca, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = FALSE)
var <- get_pca_var(pca)
var
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 100))
png("resultado.png", units="in", width=5, height=5, res=300)
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
dev.off()
par(mfrow = c(1,2))

PVE <- 100*pca$sdev^2/sum(pca$sdev^2)
PVE
plot(PVE, type = "o", 
     ylab = "PVE", 
     xlab = "Componente principal", 
     col = "blue")
plot(cumsum(PVE), type = "o", 
     ylab = "PVE acumulada", 
     xlab = "Componente principal", 
     col = "brown3")



#####################################################Correalacion de Pearson

j <- filter(full_prepared, Species =="FS",DiamClass =="1")

#Lista de outliers para eliminar#
ID <- c("F69_102+109_7_29","88_369_7_28","37_140_7_27")


malditos <- data.frame(ID)


j <- anti_join(j,malditos,by="ID")
j$respiration[j$respiration <0]<-0
j$RespCorrectedArea[j$RespCorrectedArea<0] <- 0
j$RespCorrectedVolume[j$RespCorrectedVolume<0] <-0
j$`Bark(%)` <- na.aggregate(j$`Bark(%)`)
j$Waterpercentage <-na.aggregate((j$Waterpercentage))

j <- j[ , c("Bark(%)","Waterpercentage","T3","Soil_moist","RespCorrectedArea")]

# install.packages("zoo")
# library(zoo)
# j$DiamClass <- NULL
j$`Bark(%)` <- na.aggregate(j$`Bark(%)`)
j$Waterpercentage <-na.aggregate((j$Waterpercentage))


j <- scale(j, center = TRUE, scale = TRUE)
j.cor <- cor(j, method = "pearson")
round(j.cor, digits = 2)
corrplot(j.cor)
mres1 <- cor.mtest(j, conf.level = 0.99)



png("resultado.png", units="in", width=5, height=5, res=300)
corrplot(na.omit(j.cor), method = "square", shade.col = NA, tl.col = "black", tl.cex = 0.5, tl.srt = 45, order = "FPC", type = "upper", diag = F, p.mat = mres1$p, sig.level = 0.05, insig = "label_sig", pch.col = "white", pch.cex = 2)
dev.off()
###########################

