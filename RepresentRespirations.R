
all <- fread("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV/AllYearMeasurementsWDensity.csv")

all$Class <- as.character(all$Class)
all$type <- paste(all$DiamClass,all$Species,all$Class,sep="")
filteredR <- filter(all,rsq > 0.8)
filteredR <- filteredR[!is.na(filteredR$RespCorrectedWeight_GrCO2_KGr_Year),]
#LINEAR
ggplot(filteredR,aes(x=T3, y=RespCorrectedWeight_GrCO2_KGr_Year,color=Class))+
  geom_point(size=1)+
  geom_smooth(method = "lm")+
  stat_regline_equation(aes(label=..rr.label..))+
  labs(y=expression("g CO"[2]*" Year"^-1*"Kg DW"^-1),x="Temperature ºC")+
  facet_wrap(~Species+DiamClass,scales="free")
#Quadratic
install.packages("ggpmisc")
library(ggpmisc)
ggplot(filteredR,aes(x=T3, y=RespCorrectedWeight_GrCO2_KGr_Year,color=Class))+
  geom_point(size=1)+
  geom_smooth(method = "lm",formula = y ~ x + I(x^2))+
  stat_fit_glance(method = "lm", label.x="left", label.y="top",
                  method.args = list(formula = y ~ x + I(x^2)),
                  aes(label = sprintf('R^2~"="~%.3f~~italic(p)~"="~%.2f',
                                      stat(..r.squared..),stat(..p.value..))),
                  parse = TRUE,size=3) +
  stat_regline_equation(formula= y ~ x + I(x^2),size=3,label.x.npc = 0,label.y.npc = 0.5)+
  facet_wrap(~Species+DiamClass,scales="free")
#Logaritmic
ggplot(filteredR,aes(x=T3, y=RespCorrectedWeight_GrCO2_KGr_Year,color=Class))+
  geom_point(size=1)+
  geom_smooth(method = "lm",formula = y ~ log(x))+
  stat_regline_equation(formula=y ~ log(x),aes(label=..rr.label..))+
  labs(y=expression("g CO"[2]*" Year"^-1*"Kg DW"^-1),x="Temperature ºC")+
  facet_wrap(~Species+DiamClass,scales="free")
#Exponential

filteredR <- filteredR[!is.na(filteredR$RespCorrectedWeight_GrCO2_KGr_Year),]
ggplot(filteredR,aes(x=T3, y=RespCorrectedWeight_GrCO2_KGr_Year,color=Class))+
  geom_point(size=1)+
  stat_smooth(method = "lm",formula=y ~ exp(x))+
  stat_regline_equation(formula=y ~ exp(x),aes(label=..rr.label..))+
  labs(y=expression("g CO"[2]*" Year"^-1*"Kg DW"^-1),x="Temperature ºC")+
  facet_wrap(~Species+DiamClass,scales="free")



#Respiration Vs Soil moisture
ggplot(filteredR,aes(x=Soil_moist, y=RespCorrectedWeight_GrCO2_KGr_Year,color=Class))+
  geom_point(size=1)+
  # stat_smooth(method = "lm",formula=y ~ x)+
  # stat_regline_equation(formula=y ~ x,aes(label=..rr.label..))+
  labs(y=expression("g CO"[2]*" Year"^-1*"Kg DW"^-1),x=expression("Soil Moisture M"^3*"M"^-3))+
  facet_wrap(~Species+DiamClass,scales="free")

