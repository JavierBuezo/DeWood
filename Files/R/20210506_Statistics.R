library(agricolae)
library(Matrix)
library(lme4)
library(ggplot2)
library(GGally)
library(carData)
library(car)
library(PMCMRplus)
library(onewaytests)
library(corrplot)
library(readxl)
library(dplyr)
library(DunnettTests)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(factoextra)
library(ggforce)

#To filter by Tissue

mresults <-TOCHO_BENZOX_BUTOX_FENOX_1_

#mresults <- filter(X20210510_4_ADC_ODC_Activity, Tissue == "Shoot")
mresults$Treatment <- factor(mresults$Treatment)
#mresults_norm <- scale(mresults[,c(3,4)],center=T,scale=T)

print(head(mresults))
#print(mresults_norm)

#To show n, mean and sd of each "Treatment" and Boxplot

table(mresults$Treatment)
group_by(mresults, Treatment) %>% summarise(count = n(), mean = mean(Length, na.rm = TRUE), sd = sd(Length))
boxplot(Length ~ Treatment, data = mresults, main = " ", xlab = "Treatment", ylab = "Biomass [mg DW]", col = "steelblue", border = "black")

#To identify outliers

outliers <- mresults %>% group_by(Treatment) %>% identify_outliers(Length)

#To Dirty eliminate outliers
mresults[which(mresults$Length %in% outliers),]
mresults <- mresults[-which(mresults$Length %in% outliers) , ] 
#To analyze the ANOVA model residuals to check the normality for all groups together: build the linear model and create a QQ plot of residuals

mlinearmodel  <- lm(mresults$Length ~ mresults$Treatment)
ggqqplot(residuals(mlinearmodel))
shapiro_test(residuals(mlinearmodel))

#mresults %>% group_by(Treatment) %>% shapiro_test(Biomass_mg_DW)
#ggqqplot(mresults, "Biomass_mg_DW", facet.by = "Treatment")

#Variance homoscedasticity

bartlett.test(mresults$Length ~ mresults$Treatment)
#leveneTest(mresults$Biomass_mg_DW ~ mresults$Treatment)

#ANOVA and POST HOC tests: #Equal Variances Assumed

mmodel<-aov(Length ~ Treatment, data = mresults)
anova(mmodel)
cv.model(mmodel)
with(mresults,mean(Length))
df<-df.residual(mmodel)
MSerror<-deviance(mmodel)/df

#Tukey <- HSD.test(mmodel, "Treatment", console = TRUE)
#plot(Tukey)

SNK <- SNK.test(mmodel, "Treatment", console = TRUE)
plot(SNK)

#ANOVA and POST HOC tests: #Equal Variances Not Assumed
#mmodel2 <- kruskal.test(GLN ~ Treatment, data = mresults)
#mmodel2

#DunnetT3 <- dunnettT3Test(mmodel)
#summary(DunnetT3)
#summaryGroup(DunnetT3)
#plot(DunnetT3)

GH <- gamesHowellTest(mmodel)
summary(GH)
summaryGroup(GH)
plot(GH)

#PCA
#rownames(mresults) <- mresults$Treatment
mresults$Treatment <- NULL
mresults$Tissue <- NULL
pca <- prcomp(mresults, center = TRUE, scale. = TRUE)
print(pca)
plot(pca, type = "l")
summary(pca)
biplot(pca, scale = 0)

pc1 <- apply(pca$rotation[,1]*mresults,1, sum)
pc2 <- apply(pca$rotation[,2]*mresults,1, sum)
mresults$pc1 <- pc1
mresults$pc2 <- pc2

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
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
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

#Correlation: Pearson

mresults$Treatment <- NULL
#mresults$Tissue <- NULL
mresults <- scale(mresults, center = TRUE, scale = TRUE)
mresults.cor <- cor(mresults, method = "pearson")
round(mresults.cor, digits = 2)
corrplot(mresults.cor)
mres1 <- cor.mtest(mresults, conf.level = 0.99)
corrplot(mresults.cor, method = "square", shade.col = NA, tl.col = "black", tl.cex = 0.5, tl.srt = 45, order = "FPC", type = "upper", diag = F, p.mat = mres1$p, sig.level = 0.01, insig = "label_sig", pch.col = "white", pch.cex = 2)


#Scatter plot with ellipse
ggplot(mresults,aes(x=Length,y=Nchild))+
        geom_mark_ellipse(expand = 0,aes(fill=Treatment))+
        geom_point()+
        theme_bw()+
        geom_smooth(method=lm,se=FALSE)+
       # stat_regline_equation(label.x=3.5, label.y=7)+
        stat_cor(aes(label=..rr.label..),label.x=3.5,label.y=6.5)

#get equation and R Squared as a string
#lm_eqn <- function(mresults){
#        m <- lm(y ~ x, mresults);
#        eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                         list(a = format(unname(coef(m)[1]), digits = 2),
#                              b = format(unname(coef(m)[2]), digits = 2),
#                              r2 = format(summary(m)$r.squared, digits = 3)))
#        as.character(as.expression(eq));
#}

#p1 <- p + geom_text(x = 1,y = 1, label = lm_eqn(mresults), parse = TRUE)
        
        
summary(linearegresion)
#Linear regression
linearegresion <- lm(Length ~ Nchild,data=mresults)
abline(linearegresion)
