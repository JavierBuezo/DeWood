library(nlme)
library(MuMIn)
library(predictmeans)
library(data.table)
library(nortest)
library(dplyr)
library(predictmeans)
data <- fread("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV/AllYearMeasurementsWDensity.csv")
####NA management following https://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti_and_mumin"
###We are going to use Species, DiamClass, Class,T3,Soil_moist,DensityKgM3 and RespCorrectedWeight_GrCO2_KGr_Year
data <- data[!apply(data[,c("Species", "DiamClass", "Class", "T3", "Soil_moist", "DensityKgM3", "RespCorrectedWeight_GrCO2_KGr_Year","Plot...16")], 1, anyNA),]
data$diameter <- (data$`D1(cm)`+data$`D2(cm)`+data$`D3(cm)`+data$`D4(cm)`)/4

data$Plot...16 <- as.factor(data$Plot...16)
data$Species <- as.factor(data$Species)
data$SubPlot <- as.factor(data$SubPlot)
data$Class <- as.factor(data$Class)
# data <- filter(data, data$Species=="FS")
# #Modelo
# colnames(data)
# str(data)

a=lme(log1p(RespCorrectedWeight_GrCO2_KGr_Year)~  1 + Species + diameter + Soil_moist + T3 + Class + DensityKgM3, method="REML",na.action = na.omit, data=data, random=~1|Plot...16/SubPlot) #En este caso SubPlot está anidado 
#El de debajo es el mejor modelo que he encontrado. Subplot y plot como factores aleatorios y densidad anidado dentro de Class
a=lme(log1p(RespCorrectedWeight_GrCO2_KGr_Year)~  1 + Species + diameter + Soil_moist + T3 + Class*DensityKgM3, method="REML",na.action = na.omit, data=data, random=~1|Plot...16/SubPlot) 

# # b=lme(log1p(RespCorrectedWeight_GrCO2_KGr_Year)~  1 + Species + DiamClass + Soil_moist + T3 + DensityKgM3, method="REML",na.action = na.omit, data=data, random=~1|Plot...16) 
# 
# #EN CASO DE HABER SEPARADO ESPECIE
# a=lme(log1p(RespCorrectedWeight_GrCO2_KGr_Year)~  1 + DiamClass + Soil_moist + T3 + DensityKgM3, method="REML",na.action = na.omit, data=data, random=~1|Plot...16/SubPlot) #En este caso SubPlot está anidado 
AIC(a,b)
# 
anova(a,b)

#Sin especie
# a=lme(RespCorrectedWeight_GrCO2_KGr_Year~ 1 + DiamClass + T3 + Soil_moist + DensityKgM3, method="REML",na.action = na.omit, data=data, random=~1|Plot...16) 


#Supuestos
Res <- residuals(a, type="normalize")
lillie.test(Res) #Normalidad según Kolmogorov Smirnov. no debe ser menor a 0.05 aunque es mejor graficarlo.

Fit <- fitted(a)
par(mfrow=c(2,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0) #Residuos vs valores ajustados (deben estar distribuidos arriba y abajo a ambos lados de la linea del 0)
plot(Res ~ data$RespCorrectedWeight_GrCO2_KGr_Year, xlab="V explicativa", ylab="Residuals")
abline(h=0)# Los residuos deberían distribuirse homogeneamente a ambos lados d ela línea
hist(Res, main="Histogram of residuals", xlab="Residuals")
qqnorm(Res) #El histograma debería tener forma de campana de Gauss
qqline(Res) #Los residuos o puntos debería hcaer una línea (no forma de S)
# b=data.frame(CookD(a,idn=5,plot=T,newwd=FALSE))#Tb me gusta chequear puntos influyentes

r.squaredLR(a)#R squared del modelo
anova(a)#anova del modelo
summary(a)
# install.packages("effects")
library(effects)
plot(predictorEffects(a))
library(sjPlot)
library(sjmisc)
library(ggplot2)
# install.packages("see", dependencies = TRUE)
# data(efc)
theme_set(theme_sjplot())
fit <- lme(log1p(RespCorrectedWeight_GrCO2_KGr_Year)~  1 + Species + diameter + Soil_moist + T3 + Class*DensityKgM3, method="REML",na.action = na.omit, data=data, random=~1|Plot...16/SubPlot)

plot_model(fit,type="pred",terms=c("T3","diameter","Species"))
plot_model(fit,type="pred",terms=c("T3","Class","Species"))
plot_model(fit,type="pred",terms=c("Soil_moist","Class","Species"))
plot_model(fit,type="pred",terms=c("Soil_moist","diameter","Species"))

plot_model(fit,type="pred",terms=c("T3","diameter","Species"))
plot_model(fit,type="pred",terms=c("Soil_moist","diameter","Species"))
