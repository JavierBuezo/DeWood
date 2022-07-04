library(data.table)
library(dplyr)

data <- fread("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV/AllYearMeasurementsWDensity.csv")
####NA management following https://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti_and_mumin"
###We are going to use Species, DiamClass, Class,T3,Soil_moist,DensityKgM3 and RespCorrectedWeight_GrCO2_KGr_Year
data <- data[!apply(data[,c("Species", "DiamClass", "Class", "T3", "Soil_moist", "DensityKgM3", "RespCorrectedWeight_GrCO2_KGr_Year","Plot...16")], 1, anyNA),]
dat <- filter(data,data$Species=="AA")

library(glmulti)

library(metafor)

rma.glmulti <- function(formula, data, ...)
  rma(formula, RespCorrectedWeight_GrCO2_KGr_Year, data=data, method="ML", ...)

res <- glmulti(RespCorrectedWeight_GrCO2_KGr_Year ~  DiamClass + Class + T3 + Soil_moist + DensityKgM3+Plot...16, data=dat,
               level=1, fitfunction=rma.glmulti, crit="aicc")

print(res)
plot(res)
top <- weightable(res)
top <- top[top$aicc <= min(top$aicc) + 2,]
top
summary(res@objects[[2]])
plot(res, type="s")

eval(metafor:::.glmulti)
coef(res)
mmi <- as.data.frame(coef(res))
mmi <- data.frame(Estimate=mmi$Est, SE=sqrt(mmi$Uncond), Importance=mmi$Importance, row.names=row.names(mmi))
mmi$z <- mmi$Estimate / mmi$SE
mmi$p <- 2*pnorm(abs(mmi$z), lower.tail=FALSE)
names(mmi) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
mmi$ci.lb <- mmi[[1]] - qnorm(.975) * mmi[[2]]
mmi$ci.ub <- mmi[[1]] + qnorm(.975) * mmi[[2]]
mmi <- mmi[order(mmi$Importance, decreasing=TRUE), c(1,2,4:7,3)]
round(mmi, 4)

filtered <- dat[,c("T3","Species","RespCorrectedWeight_GrCO2_KGr_Year","DiamClass","Class","Soil_moist","DensityKgM3","Plot...16")]


#######PROBAMOS ESCALANDO############
filteredscale <- filtered %>% mutate_at(c(1,3:8),funs(c(scale(.))))
rma.glmulti <- function(formula, data, ...)
  rma(formula, RespCorrectedWeight_GrCO2_KGr_Year, data=data, method="ML", ...)

res <- glmulti(RespCorrectedWeight_GrCO2_KGr_Year ~  DiamClass + Class + T3 + Soil_moist + DensityKgM3+Plot...16, data=filteredscale,
               level=1, fitfunction=rma.glmulti, crit="aicc")

print(res)
plot(res)
top <- weightable(res)
top <- top[top$aicc <= min(top$aicc) + 2,]
top
summary(res@objects[[3]])
plot(res, type="s")

eval(metafor:::.glmulti)
coef(res)
mmi <- as.data.frame(coef(res))
mmi <- data.frame(Estimate=mmi$Est, SE=sqrt(mmi$Uncond), Importance=mmi$Importance, row.names=row.names(mmi))
mmi$z <- mmi$Estimate / mmi$SE
mmi$p <- 2*pnorm(abs(mmi$z), lower.tail=FALSE)
names(mmi) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
mmi$ci.lb <- mmi[[1]] - qnorm(.975) * mmi[[2]]
mmi$ci.ub <- mmi[[1]] + qnorm(.975) * mmi[[2]]
mmi <- mmi[order(mmi$Importance, decreasing=TRUE), c(1,2,4:7,3)]
round(mmi, 4)

.#########MIXED MODELS DESDE NAGORE##########

#####FAGUS SYLVATICA
full_df <- fread("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV/AllYearMeasurementsWDensity.csv")
full_df <- filter(full_df,full_df$Species == "FS")
full_prepared <- full_df[ , c("Bark(%)","ID","Waterpercentage","Class", "DiamClass","Species","T3","Soil_moist","RespCorrectedWeight_GrCO2_KGr_Year","Plot...16","SubPlot","rsq","Month")]  

###################################################
# MORE modelos mixtos. EN ESTA VERSIÓN ELIMINO SPECIES Y SE HACEN 2 POR SEPARADO PARA AA Y FS
library(nlme)

# code to do the selection of the random factor
## model selection, first we try with different combinations of random effects
# random factor
# this is probably too complex, I tried all possible combinations
# think what makes sense and simplify. Do the combinations make sense?
rint <- c("(1|Plot...16)", "(1|SubPlot)", "(1|Plot...16/SubPlot)") # random intercepts
# rslopes <- c("(1+Class|Plot...16)","(1+Class|Subplot)","(1+Class|Plot...16/SubPlot)",         # random slope for Diamclass
#              "(1+Class|Plot...16)","(1+Class|Subplot)","(1+Class|Plot...16/SubPlot)",         # random slope for class
#              "(1+T3|Plot...16)","(1+T3|Subplot)","(1+T3|Plot...16/SubPlot)",         # random slope for time          
#              "(1+Species|Plot...16) + (1+DiamClass|Plot...16) + (1+Class|Plot...16/SubPlot) + (1+Month|Plot...16/SubPlot)"
# )#####DEJO EL ORIGINAL
rslopes <- c("(1+Class|Plot...16)","(1+Class|Subplot)","(1+Class|Plot...16/SubPlot)",         # random slope for Diamclass
             "(1+Class|Plot...16)","(1+Class|Subplot)","(1+Class|Plot...16/SubPlot)",         # random slope for class
             "(1+T3|Plot...16)","(1+T3|Subplot)","(1+T3|Plot...16/SubPlot)",         # random slope for temperature    
             "(1+Soil_moist|Plot...16)","(1+Soil_moist|Subplot)","(1+Soil_moist|Plot...16/SubPlot)",   #random slope for soil moisture 
             "(1+DiamClass|Plot...16) + (1+Class|Plot...16/SubPlot) + (1+Month|Plot...16/SubPlot)"
)#ELIMINADO SPecies
# expand grid generates a matrix all the combinations between vectors
vmat <- expand.grid(rint, rslopes)
# we format the variables so that they are ok for the formula in lmer
vr_tmp <- paste(vmat[,1], "+", vmat[,2])
# we add the random intercepts, slopes and the combinations
vr <- c(rint, rslopes, vr_tmp)

# fixed factors
vf <- c("DiamClass * Class* T3*Soil_moist")

# prepare the dataframe removing the rows with NAs in our variables of interest
full_prepared_tmp <- full_prepared[full_prepared %>% dplyr::select(RespCorrectedWeight_GrCO2_KGr_Year, DiamClass, Class) %>% complete.cases(),]

# model without random factors
simple <- gls(RespCorrectedWeight_GrCO2_KGr_Year ~ DiamClass + Class + T3+Soil_moist,
              method = "REML", data = full_prepared_tmp)

# run all the models
# rn <- vr[1]
random_mod <- lapply(vr, function(rn){
  form <- formula(paste0("RespCorrectedWeight_GrCO2_KGr_Year~", vf, "+",rn))
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

full_form <- formula(paste0("RespCorrectedWeight_GrCO2_KGr_Year~", vf))
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
# remotes::install_github("palday/coefplot2",
#                         subdir = "pkg")
# install.packages("coefplot2")
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
  theme_bw(base_size = 12)+ # this is a nice theme + selection of letter size
  theme(axis.text.x = element_text(angle = 75, vjust = 0.95, hjust=0.95))

####################################
#########ABIES ALBA##############
full_df <- filter(full_df,full_df$Species == "AA")
full_prepared <- full_df[ , c("Bark(%)","ID","Waterpercentage","Class", "DiamClass","Species","T3","Soil_moist","RespCorrectedWeight_GrCO2_KGr_Year","Plot...16","SubPlot","rsq","Month")]  

###################################################
# MORE modelos mixtos. EN ESTA VERSIÓN ELIMINO SPECIES Y SE HACEN 2 POR SEPARADO PARA AA Y FS
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
             "(1+DiamClass|Plot...16) + (1+Class|Plot...16/SubPlot) + (1+Month|Plot...16/SubPlot)"
)#ELIMINADO SPecies
# expand grid generates a matrix all the combinations between vectors
vmat <- expand.grid(rint, rslopes)
# we format the variables so that they are ok for the formula in lmer
vr_tmp <- paste(vmat[,1], "+", vmat[,2])
# we add the random intercepts, slopes and the combinations
vr <- c(rint, rslopes, vr_tmp)

# fixed factors
vf <- c("DiamClass * Class* T3* Soil_moist")

# prepare the dataframe removing the rows with NAs in our variables of interest
full_prepared_tmp <- full_prepared[full_prepared %>% dplyr::select(RespCorrectedWeight_GrCO2_KGr_Year, DiamClass, Class) %>% complete.cases(),]

# model without random factors
simple <- gls(RespCorrectedWeight_GrCO2_KGr_Year ~ DiamClass + Class + T3+ Soil_moist,
              method = "REML", data = full_prepared_tmp)

# run all the models
# rn <- vr[1]
random_mod <- lapply(vr, function(rn){
  form <- formula(paste0("RespCorrectedWeight_GrCO2_KGr_Year~", vf, "+",rn))
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

full_form <- formula(paste0("RespCorrectedWeight_GrCO2_KGr_Year~", vf))
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
# remotes::install_github("palday/coefplot2",
#                         subdir = "pkg")
# install.packages("coefplot2")
# results graph
library(coefplot2)
coefplot2(selected_mod)

# since the graph above is a little uggly we made our own7
ff <- fortify(coeftab(selected_mod))
ff$pnames <- rownames(ff)
colnames(ff) <- c("EstimateAA", "Std.Error","y2", "y25","y75","y97","pnames")

ggplot() + 
  geom_pointrange(data=ff, mapping=aes(x=pnames, y=Estimate, ymin=y2, ymax=y97),
                  size=1, color="blue", fill="white", shape=22) +
  geom_hline(yintercept = 0, linetype = 3) +
  theme_bw(base_size = 12)+ # this is a nice theme + selection of letter size
  theme(axis.text.x = element_text(angle = 75, vjust = 0.95, hjust=0.95))

