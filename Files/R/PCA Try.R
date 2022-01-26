
library(factoextra)
library(ggplot2)
library(dplyr)

path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project"
setwd(path.to.data)
archivos <- paste(getwd(),"/Respiration Campaign July 2021/EGM_2/LM Results/Joined With Field Table/",sep = "")
files_nm <- list.files(archivos, pattern =  ".csv")
archivos
Resultados <- do.call(rbind,lapply(paste(archivos,files_nm,sep = ""),read.csv))

#Transformar todo a numero, R lo coge como string
Resultados <- mutate_all(Resultados,function(x) as.numeric(as.character(x)))

names(Resultados) <- gsub("\\.", "", names(Resultados)) #eliminar los puñeteros puntos que mete R en los espacios
mediaD1 <- rowMeans(Resultados[,c("D1cm","D2cm")], na.rm = TRUE)
mediaD2 <-rowMeans(Resultados[,c("D3cm","D4cm")], na.rm = TRUE)

#Cálculos de area y volumen de un frustrum (Patterson et al 1993)
Resultados$Areacm2 <- pi*(mediaD1+mediaD2)*sqrt((mediaD1-mediaD2)^2 + Resultados$Lengthcm) 
Resultados$Volcm2 <- (pi/3)*Resultados$Lengthcm*(mediaD1^2+mediaD2^2+(mediaD1*mediaD2))


#/////////////////PARTE CORRESPONDIENTE AL PCA////////////////////
# Resultados[paste("...",14:29,sep = "")] <- NULL # De los excells quedan columnas vacías. Eliminarlas
# # Resultados$mediadiam <- sum(Resultados$[,D1:D4])
# 
# tipos <- unique(Resultados$DiamClass)
# 
# df1 <- Resultados[Resultados$DiamClass==1,]
# df10 <- Resultados[Resultados$DiamClass==10,]
# df25 <- Resultados[Resultados$DiamClass==25,]
# 
# df1temp <- mutate_all(df1,function(x) as.numeric(as.character(x)))
# df2temp <- subset(df1temp,select = -c(X.1,sample_code,Code,Notes,DiamClass,Plot,SubPlot,Class,Species,X,ID,rsq,Day,Hour,Min,Sample_code1, Sample_code2,FW..g.,DW..g.))
# all(is.finite(df2temp))
# pca <- prcomp(na.omit(df2temp), center = TRUE, scale. = TRUE)
# print(pca)
# plot(pca, type = "l")
# summary(pca)
# # ggplot2::autoplot(stats::prcomp(pca, scale=TRUE), label = FALSE, loadings.label = TRUE)
# biplot(pca, scale = 0)
# 
# pc1 <- apply(pca$rotation[,1]*df2temp,1, sum)
# pc2 <- apply(pca$rotation[,2]*df2temp,1, sum)
# df1temp$pc1 <- pc1
# df1temp$pc2 <- pc2
# 
# fviz_pca_ind(pca, geom.ind = "point", 
#              col.ind = "#FC4E07", 
#              axes = c(1, 2), 
#              pointsize = 1.5) 
# fviz_pca_var(pca, col.var = "cos2", 
#              geom.var = "arrow", 
#              labelsize = 2, 
#              repel = FALSE)
# var <- get_pca_var(pca)
# fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 100))
# fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# par(mfrow = c(1,2))
# 
# PVE <- 100*pca$sdev^2/sum(pca$sdev^2)
# PVE
# plot(PVE, type = "o", 
#      ylab = "PVE", 
#      xlab = "Componente principal", 
#      col = "blue")
# plot(cumsum(PVE), type = "o", 
#      ylab = "PVE acumulada", 
#      xlab = "Componente principal", 
#      col = "brown3")

#/////////////////PARTE CORRESPONDIENTE AL PCA////////////////////
