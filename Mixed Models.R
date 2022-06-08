
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

path.to.data <- "C:/Users/NG.5027073/Dropbox (SCENIC MNCN CSIC)/eclipseworkspace/DeWood/DeWood/Files/Final results/CSV"
#Cargar todos los archivoscsv de la carpeta
files_path <- list.files(path.to.data, full.names = T, pattern = ".csv")
#Guardar el nombre del archivo para usarlo despues como titulo
files_nm <- list.files(path.to.data, pattern = ".csv")
colnames(full_df)
full_df <- list.files(path.to.data, full.names = TRUE, pattern = ".csv") %>% lapply(fread) %>% 
  bind_rows()

full_df <- fread("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV/AllYearMeasurementsWDensity.csv")

full_prepared <- full_df[ , c("Bark(%)","ID","Waterpercentage","Class", "DiamClass","Species","T3","Soil_moist","RespCorrectedWeight_GrCO2_KGr_Year","Plot...16","SubPlot","rsq","Month")]  


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
full_prepared_tmp <- full_prepared[full_prepared %>% dplyr::select(RespCorrectedWeight_GrCO2_KGr_Year, Species, DiamClass, Class) %>% complete.cases(),]

# model without random factors
simple <- gls(RespCorrectedWeight_GrCO2_KGr_Year ~ Species + DiamClass + Class + T3,
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

install.packages("coefplot2",
                 repos="http://www.math.mcmaster.ca/bolker/R",
                 type="source")
remotes::install_github("palday/coefplot2",
                        subdir = "pkg")
install.packages("coefplot2")
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
