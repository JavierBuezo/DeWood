
library(data.table)
library(rstatix)
library(plyr)
library(dplyr)
library(zoo)
library(multcompView)
library(ggbiplot)
library(ggpubr)
library(corrplot)
library(quantreg)
library(openxlsx)
library(easyGgplot2)

path.to.dir <- "C:/Users/NG.5027073/Dropbox (SCENIC MNCN CSIC)/eclipseworkspace/DeWood/DeWood"
setwd(path.to.dir)

############ Cleaning up data function ####################

ID <- c("F69_102+109_7_29","88_369_7_28","37_140_7_27") #ID de las muestras Outliers a eliminar en la campa?a de Julio

malditos <- data.frame(ID)
cleaningup <- function(y)
{
  y <- anti_join(y,malditos,by="ID")

  y$`Bark(%)` <- na.aggregate(y$`Bark(%)`)
  y$Waterpercentage <-na.aggregate((y$Waterpercentage))
  # y <- filter(y,rsq > 0.8)
  return(y)
}
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
###########################################################

path.to.data <-"C:/Users/NG.5027073/Dropbox (SCENIC MNCN CSIC)/eclipseworkspace/DeWood/DeWood/Files/Respiration Campaign July 2021"

setwd(path.to.data)
dir()
full_df <- fread("RESULTADOFINALJulio.csv")
full_df <- fread("RESULTADOFINALOctubre.csv")
full_df <- fread("RESULTADOFINALDiciembre")
full_prepared <- full_df[ , c("Bark(%)","ID","Waterpercentage","Class", "DiamClass","Species","T3","Soil_moist","RespCorrectedArea","Plot...16","SubPlot","rsq","RespCorrectedVolume")]  


j <- filter(full_prepared, Species =="FS",DiamClass =="25")

##############################  Gather all the data from different CSV ######################

path.to.data <- "C:/Users/NG.5027073/Dropbox (SCENIC MNCN CSIC)/eclipseworkspace/DeWood/DeWood/Files/Final results/CSV"
#Cargar todos los archivoscsv de la carpeta
files_path <- list.files(path.to.data, full.names = T, pattern = ".csv")
#Guardar el nombre del archivo para usarlo despues como titulo
files_nm <- list.files(path.to.data, pattern = ".csv")

full_df <- list.files(path.to.data, full.names = TRUE, pattern = ".csv") %>% lapply(fread) %>% 
  bind_rows()

full_prepared <- full_df[ , c("Bark(%)","ID","Waterpercentage","Class", "DiamClass","Species","T3","Soil_moist","RespCorrectedArea","Plot...16","SubPlot","rsq","RespCorrectedVolume","Month","RespCorrectedgrams")]  

full_prepared$type <- paste(full_prepared$Species,full_prepared$DiamClass,full_prepared$Class,sep="_")
full_prepared$type2 <- paste(full_prepared$type,full_prepared$Month,sep="_")
full_prepared$type3 <- paste(full_prepared$Species,full_prepared$DiamClass, full_prepared$Month,sep="_")




#Con este calculo pasamos los uM CO2, Cm2 ,S a: gr CO2, M2, minuto
full_prepared$RespCorrectedArea <- full_prepared$RespCorrectedArea *10000*0.000044 * 60


#Con este calculo pasamos los uM CO2, Cm3 ,S a: gr CO2, M3, minuto
full_prepared$RespCorrectedVolume <- full_prepared$RespCorrectedVolume * 1000000*0.000044*60


#Con este calculo pasamos los uM CO2, gr ,S a: uM CO2, gr, minuto
full_prepared$RespCorrectedgrams <- full_prepared$RespCorrectedgrams *0.000044*60 * 1000000

###################### Export averages to csv   ##################
j <- full_prepared
j <- full_prepared[ , c("Class","Bark(%)","Waterpercentage","T3","Soil_moist","RespCorrectedArea","RespCorrectedVolume","type2","RespCorrectedgrams")]           
j <- filter(j,Waterpercentage != 100,Waterpercentage >0)
j[is.na(j)]<-0

#Con este calculo pasamos los uM CO2, Cm2 ,S a: gr CO2, M2, minuto
j$RespCorrectedArea <- j$RespCorrectedArea *10000*0.000044 * 60

#Con este calculo pasamos los uM CO2, Cm3 ,S a: gr CO2, M3, minuto
j$RespCorrectedVolume <- j$RespCorrectedVolume *1000000*0.000044*60


#Con este calculo pasamos los uM CO2, gr ,S a: ug CO2, gr DW, minuto En la version con densidades definitiva esta respiración ya viene corregida
#Disponible en resp_g_kg_day
# j$RespCorrectedgrams <- j$RespCorrectedgrams *0.000044*60 * 1000000

prueba <- na.omit(j) %>% group_by(type2) %>% 
  summarise_each(funs(mean,sd,n=n()))



library(stringr)
separated <- str_split_fixed(prueba$type2, "_", 4)
prueba$species <- separated[,1]
prueba$Diameter <- separated[,2]
prueba$Class <- separated[,3]
prueba$Month <- separated[,4]
getwd()
# write.csv(prueba,"Means and sd until december.csv")

prueba$RespCorrectedArea_se <- prueba$RespCorrectedArea_sd/sqrt(prueba$RespCorrectedArea_n)
prueba$RespCorrectedVolume_se <- prueba$RespCorrectedVolume_sd/sqrt(prueba$RespCorrectedVolume_n)
prueba$RespCorrectedgrams_se <- prueba$RespCorrectedgrams_sd/sqrt(prueba$RespCorrectedgrams_n)


# modelos mixtos
library(nlme)

# code to do the selection of the random factor
## model selection, first we try with different combinations of random effects
# random factor
# this is probably too complex, I tried all possible combinations
# think what makes sense and simplify. Do the combinations make sense?
rint <- c("(1|Plot...16)", "(1|SubPlot)", "(1|Plot...16/SubPlot)") # random intercepts
rslopes <- c("(1+Class|Plot...16)","(1+Class|Subplot)","(1+Class|Plot...16/SubPlot)",         # random slope for Diamclass
            "(1+Class|Plot...16)","(1+Class|Subplot)","(1+Class|Plot...16/SubPlot)",         # random slope for class
            "(1+T3|Plot...16)","(1+T3|Subplot)","(1+T3|Plot...16/SubPlot)",         # random slope for time          
            "(1+Species|Plot...16) + (1+DiamClass|Plot...16) + (1+Class|Plot...16/SubPlot) + (1+Month|Plot...16/SubPlot)"
            )

# expand grid generates a matrix all the combinations between vectors
vmat <- expand.grid(rint, rslopes)
# we format the variables so that they are ok for the formula in lmer
vr_tmp <- paste(vmat[,1], "+", vmat[,2])
# we add the random intercepts, slopes and the combinations
vr <- c(rint, rslopes, vr_tmp)

# fixed factors
vf <- c("Species * DiamClass * Class* T3")

# prepare the dataframe removing the rows with NAs in our variables of interest
full_prepared_tmp <- full_prepared[full_prepared %>% dplyr::select(RespCorrectedgrams, Species, DiamClass, Class) %>% complete.cases(),]

# model without random factors
simple <- gls(RespCorrectedgrams ~ Species + DiamClass + Class + T3,
              method = "REML", data = full_prepared_tmp)

# run all the models
# rn <- vr[1]
random_mod <- lapply(vr, function(rn){
  form <- formula(paste0("RespCorrectedgrams~", vf, "+",rn))
  mod_rn <- tryCatch(lmer(form, data = full_prepared_tmp), error = function(e) NA)
})

# AIC of the random models
AIC_rs <- ldply(random_mod, function(mod) tryCatch(AIC(mod),error = function(e) NA))
AIC_rs <- data.frame(vr, AIC=AIC_rs)

# paste the AIC of the model without any random factors for comparison
AIC_simple <- data.frame(vr = "", V1 = AIC(simple))
AIC_rs <- rbind(AIC_simple, AIC_rs)
View(AIC_rs)
selected_random <- AIC_rs[which(AIC_rs[,2] == min(AIC_rs[,2], na.rm = T )),]
# however this model is overly complex. When we fit it the matrix is singular
# so I select the model with the lowest AIC that is not overly complex --> model 4

# fixed variable selection
library(MuMIn)

full_form <- formula(paste0("RespCorrectedgrams~", vf))
full_mod <- lme(full_form, random = ~ 1 | Plot...16, data = full_prepared_tmp)
# model selection
res <- dredge(full_mod, trace=2)

# variable importance
importance(res)
selected_mod <- get.models(res, subset = 1, method = "REML")[[1]]
piecewiseSEM::rsquared(selected_mod)

# install.packages("coefplot2",
#                  repos="http://www.math.mcmaster.ca/bolker/R",
#                  type="source")

# results graph
library(coefplot2)
coefplot2(selected_mod)

# since the graph above is a little uggly we made our own7
ff <- fortify(coeftab(selected_mod))
ff$pnames <- rownames(ff)
colnames(ff) <- c("Estimate", "Std.Error","y2", "y25","y75","y97","pnames")
ggplot() + 
  geom_pointrange(data=ff, mapping=aes(x=pnames, y=Estimate, ymin=y2, ymax=y97),
                  size=1, color="blue", fill="white", shape=22) +
  geom_hline(yintercept = 0, linetype = 3) +
  theme_bw(base_size = 12) # this is a nice theme + selection of letter size



################### BARPLOT ##########################

j <- prueba
j$type3 <- paste(j$Diameter,j$Month,sep="_")
j <- filter(j,Diameter == 25)
list1 <- split(j, j$type3)


plots <- lapply(list1,function(x) ggplot(x,aes(x=Class,y=RespCorrectedgrams_mean,fill=species),ylim=0.007)+
                  ggtitle(paste("Month",x$Month,sep = " "))+
                  ylab( expression(mu*"g CO"[2]* "g DW"^-1* "min"^-1))+
                  geom_bar(stat="identity",color="black",position=position_dodge())+
                  ylim(0,5)+
                  geom_errorbar(aes(ymin=RespCorrectedgrams_mean,ymax=RespCorrectedgrams_mean+RespCorrectedgrams_se), width=.2,
                                position=position_dodge(.9)))

plots

plots <- lapply(list1,function(x) ggplot(x,aes(x=Class,y=RespCorrectedArea_mean,fill=species),ylim=0.007)+
                  ggtitle(paste("Month",x$Month,sep = " "))+
                  ylab( expression("g CO"[2]* " cm"^-2* "min"^-1))+
                  geom_bar(stat="identity",color="black",position=position_dodge())+
                  ylim(0,0.2)+
                  geom_errorbar(aes(ymin=RespCorrectedArea_mean,ymax=RespCorrectedArea_mean+RespCorrectedArea_se), width=.2,
                  position=position_dodge(.9)))
  
plots
plots <- lapply(list1,function(x) ggplot(x,aes(x=Class,y=RespCorrectedVolume_mean,fill=species),ylim=0.007)+
                  ggtitle(paste("Month",x$Month,sep = " "))+
                  geom_bar(stat="identity",color="black",position=position_dodge())+
                  ylab( expression("g CO"[2]* "cm"^-2* "min"^-1))+
                  ylim(0,3.5)+
                  geom_errorbar(aes(ymin=RespCorrectedVolume_mean,ymax=RespCorrectedVolume_mean+RespCorrectedVolume_se), width=.2,
                                position=position_dodge(.9)))
plots

max(j$RespCorrectedgrams_mean)

setwd("C:/Users/javie/OneDrive - UPNA/DeWood Project/Final results/Figures/")
lapply(names(plots), 
       function(x) ggsave(filename=paste(x,"Barrplot CO2 per Volume.jpeg",sep=""), plot=plots[[x]]))



getwd()
prueba2 <- filter(j,j$RespCorrectedArea>0.5)

######################### Boxplots #######################
full_prepared$Class <- as.character(full_prepared$Class)
list1 <- split(full_prepared, full_prepared$type3)

plots <- lapply(list1, function (x) x %>% ggdotplot(x="Class", y="RespCorrectedArea", fill="Class", size=0.7,order=c("1","2","4"),add = c("mean_sd"),error.plot="crossbar",add.params = list(width = 0.5)))
plots
lapply(names(plots), 
       function(x) ggsave(filename=paste(x,"dotplot.jpeg",sep=""), plot=plots[[x]]))

################  Shapiro Tests #####################
j <- filter(full_prepared, Species =="AA",DiamClass =="1")
j<- cleaningup(j)

shapiro.test(j$RespCorrectedArea)
shapiro.test(j$RespCorrectedVolume)
shapiro.test(j$`Bark(%)`)


################  Kruskal Wallis and Shapiro Wilk. Wilcoxon signed rank test ###################
Diameteryouwant <- "25"

aasamples <- filter(full_prepared, Species == "AA" ,DiamClass ==Diameteryouwant)
aasamples <- cleaningup(aasamples)

aasamples$group <- aasamples$Class
aasamples1 <- aasamples %>% mutate(group =case_when(
 Class=="1" ~ "aa_1",
  Class=="2" ~ "aa_2",
  Class=="4" ~ "aa_4" 
))

fssamples <- filter(full_prepared, Species =="FS",DiamClass ==Diameteryouwant)
fssamples <- cleaningup(fssamples)
fssamples$group <- fssamples$Class
fssamples1 <- fssamples %>% mutate(group =case_when(
  Class=="1" ~ "fs_1",
  Class=="2" ~ "fs_2",
  Class=="4" ~ "fs_4" 
))
j <- rbind(aasamples1,fssamples1)

 resultado <- pairwise.wilcox.test(j$RespCorrectedArea, j$group,p.adjust.method = "BH")
 resultado[["p.value"]]

 mymat <- tri.to.squ(resultado$p.value)
 
 multcompLetters(mymat)
 prueba <- fssamples1
 
 prueba %>% group_by(group) %>%
   summarise_at(vars(Waterpercentage),
                list(name=mean))
 
 
 logs <- warnings()
 setwd("D:/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/Statistics")
 write.csv(resultado[["p.value"]],"RespCorrectedarea 1cm.csv",sep = "")
 
 
 ##################### PCA separated by species and Diameter Class######################

 
 j <- filter(full_prepared,Species=="FS",DiamClass =="25")

 j <- cleaningup(j)
j.pca <- prcomp(j[,c(1,3,7,8,9)], center = TRUE,scale. = TRUE)
summary(j.pca)
ggbiplot(j.pca)
j.class <- as.factor(j$Class)
ggbiplot(j.pca, ellipse = TRUE,groups = j.class)+  
  scale_shape_manual(name="Class", values=c(17:19)) +
  geom_point(aes(colour=j.class, shape=j.class), size = 3) +
  theme(legend.direction ="horizontal", 
        legend.position = "top")


##################### PCA separated by Diameter Class and degradation class ######################

j <- filter(full_prepared,Class==4,DiamClass =="25")

j <- cleaningup(j)
j.pca <- prcomp(j[,c(1,3,7,8,9)], center = TRUE,scale. = TRUE)
summary(j.pca)
ggbiplot(j.pca)
j.species <- as.factor(j$Species)

ggbiplot(j.pca, ellipse = TRUE,groups = j.species)+  
  scale_shape_manual(name="Species", values=c(17:19)) +
  geom_point(aes(colour=j.species, shape=j.species), size = 3) +
  theme(legend.direction ="horizontal", 
        legend.position = "top")


##################### Pearson Correlation #############################

lapply(unique(full_prepared$Species), function(x){
  lapply(unique(full_prepared$DiamClass),function(y){
    lapply(unique(full_prepared$Class),function(z){
      
      j <- filter(full_prepared, Species ==x,DiamClass ==y,Class == z)
      j <- cleaningup(j)
      j <- j[ , c("Bark(%)","Waterpercentage","T3","Soil_moist","RespCorrectedArea")]
      j <- scale(j, center = TRUE, scale = TRUE)
      j.cor <- cor(j, method = "pearson")
      round(j.cor, digits = 2)
    
      mres1 <- cor.mtest(j, conf.level = 0.99)
      print(paste("Guardando",x,y,z, "en",getwd()),sep = " ")
      png(paste("Pearson correlation",x,y,z,".png",sep = "_"), units="in", width=5, height=5, res=300)
      corrplot(na.omit(j.cor), method = "square", shade.col = NA, tl.col = "black", tl.cex = 0.5, tl.srt = 45, order = "FPC", type = "upper", diag = F, p.mat = mres1$p, sig.level = 0.05, insig = "label_sig", pch.col = "white", pch.cex = 2)
      dev.off()
      
    })
    
  })
  
})



######################  Spearman Correlation ##########################
#All classes,All diameters, all species

lapply(unique(full_prepared$Species), function(x){
  lapply(unique(full_prepared$DiamClass),function(y){
    lapply(unique(full_prepared$Class),function(z){
      j <- cleaningup(j)
      j <- filter(full_prepared, Species ==x,DiamClass ==y,Class == z)
      print(paste(x,y,z,sep = "-"))
      print(cor.test(j$RespCorrectedArea, j$T3,  method = "spearman"))
    })
    
  })
  
})

####################  Quantile regression ################

lm_eqn <- function(df){
  m <- lm(T3 ~ RespCorrectedArea, df);
  eq <- substitute(italic(RespCorrectedArea) == a + b %.% italic(T3)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

full_prepared$type <- paste(full_prepared$Species,full_prepared$DiamClass,full_prepared$Class,sep="_")

vct <- unique(full_prepared$type)

getwd()
lapply(vct, function(x) {
 j <- filter(full_prepared, full_prepared$type==x)
  ggplot(j,aes(x = T3, y =RespCorrectedArea)) +
    geom_point() +
    geom_smooth(method='lm', formula= y~x) +
    ggtitle(x) +
    theme_bw()+
    stat_cor(label.x = 3, label.y = 0.1) +
    stat_regline_equation(label.x = 3, label.y = 0.01)
  ggsave(paste0("RegresionLineal",x, ".png")) 
})

lapply(names(plots), 
       function(x) {ggsave(filename=paste("LinearRegression_",x,".jpeg",sep=""), plot=plots[[x]])
         
         })

# j<-full_prepared
# j <- filter(full_prepared,Species=="AA",Class=="2",DiamClass=="10")

lapply(names(plots), 
       function(x) ggsave(filename=paste(x,".jpeg",sep=""), plot=plots[[x]]))
getwd()
 what <-lapply(unique(full_prepared$Species), function(a){
  lapply(unique(full_prepared$DiamClass),function(b){
    lapply(unique(full_prepared$Class),function(c){
      j <- filter(full_prepared, Species ==a,DiamClass ==b,Class==c)
      j <- cleaningup(j)
      j.rq <- rq(RespCorrectedArea ~ T3,data=j)
     
      y <- c(j$RespCorrectedArea, 40, 36)
      x <- c(j$T3, 5, 4)
      
      print(paste("Guardando linear regression",a,b,c, "en",getwd()),sep = " ")
      p <- ggplot(j, aes(T3,RespCorrectedArea)) + 
        geom_point() + 
        geom_smooth(method="lm")
      p
      p1 <- p + geom_text(x = 10, y = 300, label = lm_eqn(j), parse = TRUE)
      p1
      png("prueba.png")
      p1
      dev.off()
      png(paste("Linear_regression",a,b,c,".png",sep = "_"), units="in", width=5, height=5, res=300)
      p1
      dev.off()
      print(paste("Guardando quantile regression",a,b,c, "en",getwd()),sep = " ")
      png(paste("Guardando quantile regression",a,b,c,".png",sep = "_"), units="in", width=5, height=5, res=300)
      qs <- 1:9/10
      qr1 <- rq(RespCorrectedArea ~ T3, data=j, tau = qs)
      plot(summary(qr1), parm="T3")
      dev.off()
      
    })
    
  })
  
})

j <- filter(full_prepared, Species =="AA",DiamClass =="1")
j <- cleaningup(j)
j.rq <- rq(RespCorrectedArea ~ T3,data=j)
j.rq
y <- c(j$RespCorrectedArea, 40, 36)
x <- c(j$T3, 5, 4)
plot(RespCorrectedArea ~ T3,data=j, pch = 16, main = "RespCorrectedArea ~ T3")
# plot(y ~ x, pch = 16, main = "RespCorrectedArea ~ Temperature")
# points(c(5, 4), c(40, 36), pch = 16, col = "dark orange")
abline(lm(RespCorrectedArea ~ T3, data = j), col = "red", lty = 2)
abline(rq(RespCorrectedArea ~ T3, data = j), col = "blue", lty = 2)
legend("topright", legend = c("lm", "rq"), col = c("red", "blue"), lty = 2)



############    ScatterPlot  Resp Vs Temp   ################
# eq <- function(x,y) {
#   m <- lm(y ~ x)
#   as.character(
#     as.expression(
#       substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
#                  list(a = format(coef(m)[1], digits = 4),
#                       b = format(coef(m)[2], digits = 4),
#                       r2 = format(summary(m)$r.squared, digits = 3)))
#     )
#   )
# }


  
full_prepared$type <- paste(full_prepared$Species,full_prepared$DiamClass,sep="_")

j <- full_prepared
# j<- filter(full_prepared,full_prepared$respiration>=0)

j$Class <- as.character(j$Class)
temascatter <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                     legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                     legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                     axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                     axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))

list1 <- split(j, j$type)
library(ggpubr)
plots <- lapply(list1, function(x) ggplot(x, aes(T3, RespCorrectedArea,color = Class)) + 
                  temascatter +
                  geom_point(aes(color = Class),size=1) +
                  labs(y=expression("g CO"[2]*" min"^-1*"m"^2),x="Temperature ?C")+
                  geom_smooth(method='lm')+
                  facet_wrap(~Class)+
                  stat_regline_equation(aes(label = ..rr.label..)))
plots
plots <- lapply(list1, function(x) ggplot(x, aes(T3, RespCorrectedVolume,color = Class)) + 
                  temascatter +
                  geom_point(aes(color = Class),size=1) +
                  labs(y=expression("g CO"[2]*" min"^-1*"m"^-3),x="Temperature ?C")+
                  geom_smooth(method='lm')+
                  facet_wrap(~Class)+
                  stat_regline_equation(aes(label = ..rr.label..)))

plots <- lapply(list1, function(x) ggplot(x, aes(T3, RespCorrectedgrams,color = Class)) + 
                  temascatter +
                  geom_point(aes(color = Class),size=1) +
                  labs(y=expression(mu*"M CO"[2]* "min"-1*" g DW"^-1),x="Temperature ?C")+
                  geom_smooth(method='lm')+
                  facet_wrap(~Class)+
                  stat_regline_equation(aes(label = ..rr.label..)))
                
plots
getwd()
lapply(names(plots), 
       function(x) ggsave(filename=paste(x,"Scatterplot and LM Volume.jpeg",sep=""), plot=plots[[x]]))


#####################################BOXPLOT##############################
j <- full_prepared
list1 <-split(j,j$type2)
boxplots <- lapply(list1,function(x) boxplot(RespCorrectedArea ~T3, j))
lapply(names(boxplots), 
       function(x) ggsave(filename=paste(x,"Boxplots ALL.jpeg",sep=""), plot=boxplots[[x]]))
