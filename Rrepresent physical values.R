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





#PCA Calculation and representation
# Load
library(FactoMineR)
library(factoextra)
library(fact)
library(corrplot)
library(ggbiplot)
library(FactoMineR)
library(missMDA)
library(RColorBrewer)
####################################
# Management of the NAs_________________________________________________________
results<-total4
results$CNRatio <- total4$`Ctotal (g/100g)`/total4$`Ntotal (g/100g)`
nb <- estim_ncpPCA(results[,c(14,15,16,21,25:27,30,31,36,37,40:43,46,47)], scale=TRUE)
comp <- imputePCA(results[,c(14,15,16,21,25:27,30,31,36,37,40:43,46,47)], ncp=1, scale=TRUE)
results <- comp$completeObs
results<-as.data.frame(results)
results$DiamClass=total4$DiamClass
results$Species=total4$Species # NOW results has all its columns and values
results$Class<-total4$Class
results$type <- paste(total4$DiamClass,total4$Species,total4$Class,sep="_")


# Representation in space
#####################################FILTER 1 cm 

results1cm <- filter(results,results$DiamClass=="1")
results1cmAA <- filter(results1cm,results1cm$Species=="AA")
results1cmFS <- filter(results1cm,results1cm$Species=="FS")


##PCA 1 cm ALL
results1cm$DiamClass=NULL
results1cm$Species=NULL # NOW results has all its columns and values
results1cm$Class<-NULL

res.pca1cm <- PCA(results1cm, quali.sup = 18) #I WANT TO SAVE THIS

PSD_biplot1cm <-fviz_pca_biplot (res.pca1cm,
                                            col.ind = results1cm$type, palette = brewer.pal(6, "Dark2"),
                                            addEllipses = TRUE, label = "var",
                                            col.var = "black", repel = TRUE,
                                            legend.title = "Type")
PSD_biplot1cm #I WANT TO SAVE THIS
PSD_inertia1cm <- fviz_eig(res.pca1cm, addlabels = TRUE, ylim = c(0, 40)) #I WANT TO SAVE THIS
var1cm <- get_pca_var(res.pca1cm)
corrplot(var1cm$cos2, is.corr=FALSE)
        
######BONUS PCA EACH SPECIES SEPARATELY

        ################AA############
        results1cmAA$DiamClass=NULL
        results1cmAA$Species=NULL # NOW results has all its columns and values
        results1cmAA$Class<-NULL
        res.pca1cmAA <- PCA(results1cmAA, quali.sup = 18) #I WANT TO SAVE THIS
        PSD_biplot1cmAA <-fviz_pca_biplot (res.pca1cmAA,
                                         col.ind = results1cmAA$type, palette = brewer.pal(6, "Dark2"),
                                         addEllipses = TRUE, label = "var",
                                         col.var = "black", repel = TRUE,
                                         legend.title = "Type")
        results1cmAA
        results1cmAA$type
         
        PSD_biplot1cmAA #I WANT TO SAVE THIS
        PSD_inertia1cmAA <- fviz_eig(res.pca1cmAA, addlabels = TRUE, ylim = c(0, 50)) 
        PSD_inertia1cmAA #I WANT TO SAVE THIS
        var1cmAA <- get_pca_var(res.pca1cmAA)
        corrplot(var1cmAA$cos2, is.corr=FALSE)
        
        #########################################
        #################FS######################
        results1cmFS$DiamClass=NULL
        results1cmFS$Species=NULL # NOW results has all its columns and values
        results1cmFS$Class<-NULL
        res.pca1cmFS <- PCA(results1cmFS, quali.sup = 18) #I WANT TO SAVE THIS
        PSD_biplot1cmFS <-fviz_pca_biplot (res.pca1cmFS,
                                           col.ind = as.factor(results1cmFS$type), palette = brewer.pal(6, "Dark2"),
                                           addEllipses = TRUE, label = "var",
                                           col.var = "black", repel = TRUE,
                                           legend.title = "Type")
        results1cmFS$type
        
        
        PSD_biplot1cmFS #I WANT TO SAVE THIS
        PSD_inertia1cmFS <- fviz_eig(res.pca1cmFS, addlabels = TRUE, ylim = c(0, 40)) 
        PSD_inertia1cmFS #I WANT TO SAVE THIS
        var1cmFS <- get_pca_var(res.pca1cmFS)
        corrplot(var1cmFS$cos2, is.corr=FALSE) 
        # SAVE IN PDF
        getwd()
        pdf ("PCA_1cm.pdf") # Open pdf
        PCA(results1cm, quali.sup = 18)
        PSD_biplot1cm
        PSD_inertia1cm
        corrplot(var1cm$cos2, is.corr=FALSE)
        PCA(results1cmAA, quali.sup = 18)
        PSD_biplot1cmAA
        PSD_inertia1cmAA
        corrplot(var1cmAA$cos2, is.corr=FALSE)
        PCA(results1cmFS, quali.sup = 18)
        PSD_biplot1cmFS
        PSD_inertia1cmFS
        corrplot(var1cmFS$cos2, is.corr=FALSE)
        dev.off () # Close pdf
    
########################################        
#############Filter 10 cm
        results10cm <- filter(results,results$DiamClass=="10")
        results10cmAA <- filter(results10cm,results10cm$Species=="AA")
        results10cmFS <- filter(results10cm,results10cm$Species=="FS")

        ##PCA 10 cm ALL
        results10cm$DiamClass=NULL
        results10cm$Species=NULL # NOW results has all its columns and values
        results10cm$Class<-NULL
        
        res.pca10cm <- PCA(results10cm, quali.sup = 18) #I WANT TO SAVE THIS
        
        PSD_biplot10cm <-fviz_pca_biplot (res.pca10cm,
                                         col.ind = results10cm$type, palette = brewer.pal(6, "Dark2"),
                                         addEllipses = TRUE, label = "var",
                                         col.var = "black", repel = TRUE,
                                         legend.title = "Type")
        PSD_biplot10cm #I WANT TO SAVE THIS
        PSD_inertia10cm <- fviz_eig(res.pca10cm, addlabels = TRUE, ylim = c(0, 40)) #I WANT TO SAVE THIS
        var10cm <- get_pca_var(res.pca10cm)
        corrplot(var10cm$cos2, is.corr=FALSE)
        ######BONUS PCA EACH SPECIES SEPARATELY
        
        ################AA############
        results10cmAA$DiamClass=NULL
        results10cmAA$Species=NULL # NOW results has all its columns and values
        results10cmAA$Class<-NULL
        res.pca10cmAA <- PCA(results10cmAA, quali.sup = 18) #I WANT TO SAVE THIS
        PSD_biplot10cmAA <-fviz_pca_biplot (res.pca10cmAA,
                                           col.ind = results10cmAA$type, palette = brewer.pal(6, "Dark2"),
                                           addEllipses = TRUE, label = "var",
                                           col.var = "black", repel = TRUE,
                                           legend.title = "Type")
        
        
        
        PSD_biplot10cmAA #I WANT TO SAVE THIS
        PSD_inertia10cmAA <- fviz_eig(res.pca10cmAA, addlabels = TRUE, ylim = c(0, 40)) 
        PSD_inertia10cmAA #I WANT TO SAVE THIS
        var10cmAA <- get_pca_var(res.pca10cmAA)
        corrplot(var10cmAA$cos2, is.corr=FALSE)
        #########################################
        #################FS######################
        results10cmFS$DiamClass=NULL
        results10cmFS$Species=NULL # NOW results has all its columns and values
        results10cmFS$Class<-NULL
        res.pca10cmFS <- PCA(results10cmFS, quali.sup = 18) #I WANT TO SAVE THIS
        PSD_biplot10cmFS <-fviz_pca_biplot (res.pca10cmFS,
                                           col.ind = results10cmFS$type, palette = brewer.pal(6, "Dark2"),
                                           addEllipses = TRUE, label = "var",
                                           col.var = "black", repel = TRUE,
                                           legend.title = "Type")
        
        
        
        PSD_biplot10cmFS #I WANT TO SAVE THIS
        PSD_inertia10cmFS <- fviz_eig(res.pca10cmFS, addlabels = TRUE, ylim = c(0, 40)) 
        PSD_inertia10cmFS #I WANT TO SAVE THIS
        var10cmFS <- get_pca_var(res.pca10cmFS)
        corrplot(var10cmFS$cos2, is.corr=FALSE)
        # SAVE IN PDF
        getwd()
        pdf ("PCA_10cm.pdf") # Open pdf
        PCA(results10cm, quali.sup = 18)
        PSD_biplot10cm
        PSD_inertia10cm
        corrplot(var10cm$cos2, is.corr=FALSE)
        PCA(results10cmAA, quali.sup = 18)
        PSD_biplot10cmAA
        PSD_inertia10cmAA
        corrplot(var10cmAA$cos2, is.corr=FALSE)
        PCA(results10cmFS, quali.sup = 18)
        PSD_biplot10cmFS
        PSD_inertia10cmFS
        corrplot(var10cmFS$cos2, is.corr=FALSE)
        dev.off () # Close pdf
        #Filter 10 cm
        
 ############################################       
        #############Filter 25 cm
        
        results25cm <- filter(results,results$DiamClass=="25")
        results25cmAA <- filter(results25cm,results25cm$Species=="AA")
        results25cmFS <- filter(results25cm,results25cm$Species=="FS")

        
        ##PCA 25 cm ALL
        results25cm$DiamClass=NULL
        results25cm$Species=NULL # NOW results has all its columns and values
        results25cm$Class<-NULL
        
        res.pca25cm <- PCA(results25cm, quali.sup = 18) #I WANT TO SAVE THIS
        
        PSD_biplot25cm <-fviz_pca_biplot (res.pca25cm,
                                          col.ind = results25cm$type, palette = brewer.pal(6, "Dark2"),
                                          addEllipses = TRUE, label = "var",
                                          col.var = "black", repel = TRUE,
                                          legend.title = "Type")
        PSD_biplot25cm #I WANT TO SAVE THIS
        PSD_inertia25cm <- fviz_eig(res.pca25cm, addlabels = TRUE, ylim = c(0, 40)) #I WANT TO SAVE THIS
        var25cm <- get_pca_var(res.pca25cm)
        corrplot(var25cm$cos2, is.corr=FALSE)
        ######BONUS PCA EACH SPECIES SEPARATELY
        
        ################AA############
        results25cmAA$DiamClass=NULL
        results25cmAA$Species=NULL # NOW results has all its columns and values
        results25cmAA$Class<-NULL
        res.pca25cmAA <- PCA(results25cmAA, quali.sup = 18) #I WANT TO SAVE THIS
        PSD_biplot25cmAA <-fviz_pca_biplot (res.pca25cmAA,
                                            col.ind = results25cmAA$type, palette = brewer.pal(6, "Dark2"),
                                            addEllipses = TRUE, label = "var",
                                            col.var = "black", repel = TRUE,
                                            legend.title = "Type")
        
        
        
        PSD_biplot25cmAA #I WANT TO SAVE THIS
        PSD_inertia25cmAA <- fviz_eig(res.pca25cmAA, addlabels = TRUE, ylim = c(0, 40)) 
        PSD_inertia25cmAA #I WANT TO SAVE THIS
        var10cmAA <- get_pca_var(res.pca10cmAA)
        corrplot(var10cmAA$cos2, is.corr=FALSE)
        #########################################
        #################FS######################
        results25cmFS$DiamClass=NULL
        results25cmFS$Species=NULL # NOW results has all its columns and values
        results25cmFS$Class<-NULL
        res.pca25cmFS <- PCA(results25cmFS, quali.sup = 18) #I WANT TO SAVE THIS
        PSD_biplot25cmFS <-fviz_pca_biplot (res.pca25cmFS,
                                            col.ind = results25cmFS$type, palette = brewer.pal(6, "Dark2"),
                                            addEllipses = TRUE, label = "var",
                                            col.var = "black", repel = TRUE,
                                            legend.title = "Type")
        
        
        
        PSD_biplot25cmFS #I WANT TO SAVE THIS
        PSD_inertia25cmFS <- fviz_eig(res.pca25cmFS, addlabels = TRUE, ylim = c(0, 40)) 
        PSD_inertia25cmFS #I WANT TO SAVE THIS
        var25cmFS <- get_pca_var(res.pca25cmFS)
        corrplot(var25cmFS$cos2, is.corr=FALSE)
        # SAVE IN PDF
        getwd()
        pdf ("PCA_25cm.pdf") # Open pdf
        PCA(results25cm, quali.sup = 18)
        PSD_biplot25cm
        PSD_inertia25cm
        corrplot(var25cm$cos2, is.corr=FALSE)
        PCA(results25cmAA, quali.sup = 18)
        PSD_biplot25cmAA
        PSD_inertia25cmAA
        corrplot(var25cmAA$cos2, is.corr=FALSE)
        PCA(results25cmFS, quali.sup = 18)
        PSD_biplot25cmFS
        PSD_inertia25cmFS
        corrplot(var25cmFS$cos2, is.corr=FALSE)
        dev.off () # Close pdf
     
############################
        #Compare between classes for different Diameters by species
        
        resultsclass1 <- filter(results,results$Class=="1")
        resultsclass1AA <- filter(resultsclass1,resultsclass1$Species=="AA")
        resultsclass1FS <- filter(resultsclass1,resultsclass1$Species=="FS")

        ##PCA Class 1 cm ALL
        resultsclass1$DiamClass=NULL
        resultsclass1$Species=NULL # NOW results has all its columns and values
        resultsclass1$Class<-NULL
        
        res.pcaclass1 <- PCA(resultsclass1, quali.sup = 18) #I WANT TO SAVE THIS
        
        PSD_biplotclass1 <-fviz_pca_biplot (res.pcaclass1,
                                          col.ind = resultsclass1$type, palette = brewer.pal(6, "Dark2"),
                                          addEllipses = TRUE, label = "var",
                                          col.var = "black", repel = TRUE,
                                          legend.title = "Type")
        PSD_biplotclass1 #I WANT TO SAVE THIS
        PSD_inertiaclass1 <- fviz_eig(res.pca10cm, addlabels = TRUE, ylim = c(0, 40)) #I WANT TO SAVE THIS
        varclass1 <- get_pca_var(res.pcaclass1)
        corrplot(varclass1$cos2, is.corr=FALSE)
        ######BONUS PCA EACH SPECIES SEPARATELY
        
        ################AA############
        resultsclass1AA$DiamClass=NULL
        resultsclass1AA$Species=NULL # NOW results has all its columns and values
        resultsclass1AA$Class<-NULL
        res.pcaclass1AA <- PCA(resultsclass1AA, quali.sup = 18) #I WANT TO SAVE THIS
        PSD_biplotclass1AA <-fviz_pca_biplot (res.pcaclass1AA,
                                            col.ind = resultsclass1AA$type, palette = brewer.pal(6, "Dark2"),
                                            addEllipses = TRUE, label = "var",
                                            col.var = "black", repel = TRUE,
                                            legend.title = "Type")
        
        
        
        PSD_biplotclass1AA #I WANT TO SAVE THIS
        PSD_inertiaclass1AA <- fviz_eig(res.pcaclass1AA, addlabels = TRUE, ylim = c(0, 50)) 
        PSD_inertiaclass1AA #I WANT TO SAVE THIS
        varclass1AA <- get_pca_var(res.pcaclass1AA)
        corrplot(varclass1AA$cos2, is.corr=FALSE)
        #########################################
        #################FS######################
        resultsclass1FS$DiamClass=NULL
        resultsclass1FS$Species=NULL # NOW results has all its columns and values
        resultsclass1FS$Class<-NULL
        res.pcaclass1FS <- PCA(resultsclass1FS, quali.sup = 18) #I WANT TO SAVE THIS
        PSD_biplotclass1FS <-fviz_pca_biplot (res.pcaclass1FS,
                                            col.ind = resultsclass1FS$type, palette = brewer.pal(6, "Dark2"),
                                            addEllipses = TRUE, label = "var",
                                            col.var = "black", repel = TRUE,
                                            legend.title = "Type")
        
        
        
        PSD_biplotclass1FS #I WANT TO SAVE THIS
        PSD_inertiaclass1FS <- fviz_eig(res.pca10cmFS, addlabels = TRUE, ylim = c(0, 40)) 
        PSD_inertiaclass1FS #I WANT TO SAVE THIS
        varclass1FS <- get_pca_var(res.pcaclass1FS)
        corrplot(varclass1FS$cos2, is.corr=FALSE)
        # SAVE IN PDF
        getwd()
        pdf ("PCA_Class1.pdf") # Open pdf
        PCA(resultsclass1, quali.sup = 18)
        PSD_biplotclass1
        PSD_inertiaclass1
        corrplot(varclass1$cos2, is.corr=FALSE)
        PCA(resultsclass1AA, quali.sup = 18)
        PSD_biplotclass1AA
        PSD_inertiaclass1AA
        corrplot(varclass1AA$cos2, is.corr=FALSE)
        PCA(resultsclass1FS, quali.sup = 18)
        PSD_biplotclass1FS
        PSD_inertiaclass1FS
        corrplot(varclass1FS$cos2, is.corr=FALSE)
        dev.off () # Close pdf
      ######################################
       ############### PCA Class 2 
        resultsclass2 <- filter(results,results$Class=="2")
        resultsclass2AA <- filter(resultsclass2,resultsclass2$Species=="AA")
        resultsclass2FS <- filter(resultsclass2,resultsclass2$Species=="FS")
        
        ##PCA Class 2  ALL
        resultsclass2$DiamClass=NULL
        resultsclass2$Species=NULL # NOW results has all its columns and values
        resultsclass2$Class<-NULL
        
        res.pcaclass2 <- PCA(resultsclass2, quali.sup = 18) #I WANT TO SAVE THIS
        
        PSD_biplotclass2 <-fviz_pca_biplot (res.pcaclass2,
                                            col.ind = resultsclass2$type, palette = brewer.pal(6, "Dark2"),
                                            addEllipses = TRUE, label = "var",
                                            col.var = "black", repel = TRUE,
                                            legend.title = "Type")
        PSD_biplotclass2 #I WANT TO SAVE THIS
        PSD_inertiaclass2 <- fviz_eig(res.pca10cm, addlabels = TRUE, ylim = c(0, 40)) #I WANT TO SAVE THIS
        varclass2 <- get_pca_var(res.pcaclass2)
        corrplot(varclass2$cos2, is.corr=FALSE)
        ######BONUS PCA EACH SPECIES SEPARATELY
        
        ################AA############
        resultsclass2AA$DiamClass=NULL
        resultsclass2AA$Species=NULL # NOW results has all its columns and values
        resultsclass2AA$Class<-NULL
        res.pcaclass2AA <- PCA(resultsclass2AA, quali.sup = 18) #I WANT TO SAVE THIS
        PSD_biplotclass2AA <-fviz_pca_biplot (res.pcaclass2AA,
                                              col.ind = resultsclass2AA$type, palette = brewer.pal(6, "Dark2"),
                                              addEllipses = TRUE, label = "var",
                                              col.var = "black", repel = TRUE,
                                              legend.title = "Type")
        
        
        
        PSD_biplotclass2AA #I WANT TO SAVE THIS
        PSD_inertiaclass2AA <- fviz_eig(res.pcaclass2AA, addlabels = TRUE, ylim = c(0, 40)) 
        PSD_inertiaclass2AA #I WANT TO SAVE THIS
        varclass2AA <- get_pca_var(res.pcaclass2AA)
        corrplot(varclass2AA$cos2, is.corr=FALSE)
        #########################################
        #################FS######################
        resultsclass2FS$DiamClass=NULL
        resultsclass2FS$Species=NULL # NOW results has all its columns and values
        resultsclass2FS$Class<-NULL
        res.pcaclass2FS <- PCA(resultsclass2FS, quali.sup = 18) #I WANT TO SAVE THIS
        PSD_biplotclass2FS <-fviz_pca_biplot (res.pcaclass2FS,
                                              col.ind = resultsclass2FS$type, palette = brewer.pal(6, "Dark2"),
                                              addEllipses = TRUE, label = "var",
                                              col.var = "black", repel = TRUE,
                                              legend.title = "Type")
        
        
        
        PSD_biplotclass2FS #I WANT TO SAVE THIS
        PSD_inertiaclass2FS <- fviz_eig(res.pca10cmFS, addlabels = TRUE, ylim = c(0, 40)) 
        PSD_inertiaclass2FS #I WANT TO SAVE THIS
        varclass2FS <- get_pca_var(res.pcaclass2FS)
        corrplot(varclass2FS$cos2, is.corr=FALSE)
        # SAVE IN PDF
        getwd()
        pdf ("PCA_Class2.pdf") # Open pdf
        PCA(resultsclass2, quali.sup = 18)
        PSD_biplotclass2
        PSD_inertiaclass2
        corrplot(varclass2$cos2, is.corr=FALSE)
        PCA(resultsclass2AA, quali.sup = 18)
        PSD_biplotclass2AA
        PSD_inertiaclass2AA
        corrplot(varclass2AA$cos2, is.corr=FALSE)
        PCA(resultsclass2FS, quali.sup = 18)
        PSD_biplotclass2FS
        PSD_inertiaclass2FS
        corrplot(varclass2FS$cos2, is.corr=FALSE)
        dev.off () # Close pdf
 
        
        ############### PCA Class 4 
        resultsclass4 <- filter(results,results$Class=="4")
        resultsclass4AA <- filter(resultsclass4,resultsclass4$Species=="AA")
        resultsclass4FS <- filter(resultsclass4,resultsclass4$Species=="FS")
        
        ##PCA Class 4  ALL
        resultsclass4$DiamClass=NULL
        resultsclass4$Species=NULL # NOW results has all its columns and values
        resultsclass4$Class<-NULL
        
        res.pcaclass4 <- PCA(resultsclass4, quali.sup = 18) #I WANT TO SAVE THIS
        
        PSD_biplotclass4 <-fviz_pca_biplot (res.pcaclass4,
                                            col.ind = resultsclass4$type, palette = brewer.pal(6, "Dark2"),
                                            addEllipses = TRUE, label = "var",
                                            col.var = "black", repel = TRUE,
                                            legend.title = "Type")
        PSD_biplotclass4 #I WANT TO SAVE THIS
        PSD_inertiaclass4 <- fviz_eig(res.pca10cm, addlabels = TRUE, ylim = c(0, 40)) #I WANT TO SAVE THIS
        varclass4 <- get_pca_var(res.pcaclass4)
        corrplot(varclass4$cos2, is.corr=FALSE)
        ######BONUS PCA EACH SPECIES SEPARATELY
        
        ################AA############
        resultsclass4AA$DiamClass=NULL
        resultsclass4AA$Species=NULL # NOW results has all its columns and values
        resultsclass4AA$Class<-NULL
        res.pcaclass4AA <- PCA(resultsclass4AA, quali.sup = 18) #I WANT TO SAVE THIS
        PSD_biplotclass4AA <-fviz_pca_biplot (res.pcaclass4AA,
                                              col.ind = resultsclass4AA$type, palette = brewer.pal(6, "Dark2"),
                                              addEllipses = TRUE, label = "var",
                                              col.var = "black", repel = TRUE,
                                              legend.title = "Type")
        
        
        
        PSD_biplotclass4AA #I WANT TO SAVE THIS
        PSD_inertiaclass4AA <- fviz_eig(res.pcaclass4AA, addlabels = TRUE, ylim = c(0, 50)) 
        PSD_inertiaclass4AA #I WANT TO SAVE THIS
        varclass4AA <- get_pca_var(res.pcaclass4AA)
        corrplot(varclass4AA$cos2, is.corr=FALSE)
        #########################################
        #################FS######################
        resultsclass4FS$DiamClass=NULL
        resultsclass4FS$Species=NULL # NOW results has all its columns and values
        resultsclass4FS$Class<-NULL
        res.pcaclass4FS <- PCA(resultsclass4FS, quali.sup = 18) #I WANT TO SAVE THIS
        PSD_biplotclass4FS <-fviz_pca_biplot (res.pcaclass4FS,
                                              col.ind = resultsclass4FS$type, palette = brewer.pal(6, "Dark2"),
                                              addEllipses = TRUE, label = "var",
                                              col.var = "black", repel = TRUE,
                                              legend.title = "Type")
        
        
        
        PSD_biplotclass4FS #I WANT TO SAVE THIS
        PSD_inertiaclass4FS <- fviz_eig(res.pca10cmFS, addlabels = TRUE, ylim = c(0, 40)) 
        PSD_inertiaclass4FS #I WANT TO SAVE THIS
        varclass4FS <- get_pca_var(res.pcaclass4FS)
        corrplot(varclass4FS$cos2, is.corr=FALSE)
        # SAVE IN PDF
        getwd()
        pdf ("PCA_Class4.pdf") # Open pdf
        PCA(resultsclass4, quali.sup = 18)
        PSD_biplotclass4
        PSD_inertiaclass4
        corrplot(varclass$cos2, is.corr=FALSE)
        PCA(resultsclass4AA, quali.sup = 18)
        PSD_biplotclass4AA
        PSD_inertiaclass4AA
        corrplot(varclass4AA$cos2, is.corr=FALSE)
        PCA(resultsclass4FS, quali.sup = 18)
        PSD_biplotclass4FS
        PSD_inertiaclass4FS
        corrplot(varclass4FS$cos2, is.corr=FALSE)
        dev.off () # Close pdf
        
        pdf("Only PCA Cluster by Diameter.pdf")
        PSD_biplot1cm
        PSD_biplot1cmAA
        PSD_biplot1cmFS
        PSD_biplot10cm
        PSD_biplot10cmAA
        PSD_biplot10cmFS
        PSD_biplot25cm
        PSD_biplot25cmAA
        PSD_biplot25cmFS
        dev.off ()
        
        pdf("Only PCA Cluster by Class.pdf")
        PSD_biplotclass1
        PSD_biplotclass1AA
        PSD_biplotclass1FS
        PSD_biplotclass2
        PSD_biplotclass2AA
        PSD_biplotclass2FS
        PSD_biplotclass4
        PSD_biplotclass4AA
        PSD_biplotclass4FS
        dev.off ()
        