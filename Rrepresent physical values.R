library(data.table)                     
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(readxl)
library(tidyr) 

total4 <- fread("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/All_PhysicalTraits_CN_Ionomics.csv") 
total4$sample_code <- as.character(total4$sample_code)
total4$Plot <- as.character(total4$Plot)
total4$SubPlot <- as.character(total4$SubPlot)
total4$Class <- as.character(total4$Class)
total4$DiamClass <- as.character(total4$DiamClass)
names(total4) <- names(total4) %>% make.names()
cols <- select_if(total4,is.numeric)
aux<- cols[,1]
numerootrojoderhostias <- as.numeric(1)
colnames(cols)[1]
plots <- lapply(cols, function(aux){ 
  # print(as.character(aux))
  print(colnames(cols)[numerootrojoderhostias])
  numerootrojoderhostias <- numerootrojoderhostias + 1
  # ggplot(total4,aes(x=Class,y=aux,color=Class))+
  #   geom_boxplot()+
  #   labs(X="Class",y=paste("WTF",sep = ""))+
  #   facet_wrap(~Species+DiamClass)+
  #   stat_summary(fun.y = "mean",color="black",size=0.5)
  
  # print(aux)
})
  ?lapply
plots
colnames(total4)[14]<-"Nitrogen"
lol <- nitrogen
plots
library(EnvStats)
ggplot(total4,aes(x=Class,y=Nitrogen,color=Class),na.rm=TRUE)+
  geom_boxplot()+
  labs(X="Class",y="Ntotal (g/100g)")+
stat_n_text()+
facet_wrap(~Species+DiamClass)

prueba <- filter(total4,Class=="2")
prueba <- filter(prueba,Species=="AA")
prueba <- filter(prueba,DiamClass=="25")
total4$group <- paste(total4$DiamClass,total4$Species,total4$Class,sep = "_")

# install.packages("ggbiplot")
# install.packages("devtools")
# library(devtools)
# install_github("vqv/ggbiplot")

# Install
# install.packages()


#PCA Calculation and representation
#_______________________________________________________________________________
# Management of the NAs_________________________________________________________
results<-total4
nb <- estim_ncpPCA(results[,c(14,15,16,21,25:27,30,31,36,37,40:43,46)], scale=TRUE)
comp <- imputePCA(results[,c(14,15,16,21,25:27,30,31,36,37,40:43,46)], ncp=0, scale=TRUE)
results <- comp$completeObs
results<-as.data.frame(results)
results$Area=results_NA$Area
results$Species=results_NA$Species # NOW results has all its columns and values
results$Area<-as.factor(results$Area)
results$Species<-as.factor(results$Species)


res.pca <- PCA(results, quali.sup = 9:10)
# Load
library("FactoMineR")
library(factoextra)
library(fact)
library(corrplot)
library(ggbiplot)
library(FactoMineR)
library(missMDA)

#NA management
jimput <- imputePCA(j[,14:16],ncp = 2)
j.pca <- PCA(jimput, scale.unit = TRUE,ncp = 5, graph=T)
fviz_pca_var(j.pca, col.var ="black")

fviz_pca_ind(j.pca,
             label = "none", # hide individual labels
             habillage = as.factor(j$Species), # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE # Concentration ellipses
)
#_______________________________________________________________________________
### Contribution/ representation________________________________________________

# Representation regarding dimensions
var <- get_pca_var(j.pca)
corrplot(var$cos2, is.corr=FALSE) 


# Representation in space
PSD_representation_dim12 <-fviz_pca_var(j.pca, axes= c(3,4), col.var = "cos2",
                                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                        repel = TRUE # ?vite le chevauchement de texte
)

PSD_representation_dim34 <-fviz_pca_var(j.pca, axes= c(1,2), col.var = "cos2",
                                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                        repel = TRUE # ?vite le chevauchement de texte
)

#Contribution regarding dimensions
corrplot(var$contrib, is.corr=FALSE) 


j.pca <- prcomp(na.omit(j)[,c("Ntotal..g.100g.","Ctotal..g.100g.","Al..mg.Kg.")], center = TRUE,scale. = TRUE)
plot.PCA(j.pca,axes=c(1,2),choix = "ind",habillage = 2)              
dimdesc(j.pca, axes=c(1,2))


summary(j.pca)
ggbiplot(j.pca)
j.Species <- as.factor(na.omit(j$Species))

ggbiplot(j.pca, ellipse = TRUE,groups = j.Species)+  
  scale_shape_manual(name="Species", values=c(17:19)) +
  geom_point(aes(colour=j.species, shape=j.species), size = 3) +
  theme(legend.direction ="horizontal", 
        legend.position = "top")
