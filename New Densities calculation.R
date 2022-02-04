library(openxlsx)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plyr)
path.to.data <- "C:/Users/NG.5027073/Dropbox (SCENIC MNCN CSIC)/eclipseworkspace/DeWood/DeWood/Files/Final results/CSV"
path.to.data  <- "C:/Users/Javier/Documents/DeWood Git/DeWood/Files/Final results/CSV"
path.to.data  <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/Final results/CSV"
setwd(path.to.data)



  
field_table <- read.xlsx("Table field final.xlsx")
density_table <- fread("Density_table.csv")
colnames(density_table)[1] <- "LABEL"
density_table$LABEL <- as.character(density_table$LABEL)
all_table <- left_join(field_table,density_table,by="LABEL")
all_table$`D1.(cm)` <- as.numeric(all_table$`D1.(cm)`)
all_table$vol_withfield <- pi *((all_table$"D1.(cm)"/2)^2) * all_table$`Length.(cm)`
all_table$Code <- str_remove_all(all_table$Code,pattern = " ")
all_table$Code <- str_replace_all(all_table$Code,pattern = "1-",replacement = "01-")

#Extraer todas las samples que tengan p10


all_table$Plot <- substr(all_table$Code,5,5)
all_table$SubPlot <- substr(all_table$Code,7,7)
all_table$Class <- substr(all_table$Code,9,9)
all_table$Species <- substr(all_table$Code,10,11)
all_table$SubPlot <- as.numeric(all_table$SubPlot)
all_table$SubPlot <-as.character(all_table$SubPlot)
#Clase de Diametro
all_table$DiamClass <- substr(all_table$Code,1,2) 

all_table <- all_table %>% drop_na(SubPlot)


all_table <- all_table %>% mutate(sample_length =case_when(
  DiamClass=="01" ~ "10",
  DiamClass=="10" ~ "5",
  DiamClass=="25" ~ "5"
  
))

all_table$sample_length <- as.numeric(all_table$sample_length)
all_table$meanradius <- ((all_table$`D1.(cm)`+all_table$`D2.(cm)`+all_table$`D3.(cm)`+all_table$`D4.(cm)`)/4)/2
all_table$samplevolume <- pi * (all_table$meanradius^2) * all_table$sample_length
all_table$sample_density <- (all_table$`DW.(g)`/all_table$samplevolume) * 1000

all_table$type <- paste(all_table$Species,all_table$DiamClass,all_table$Class,sep="")


library(plyr)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
dat2 <- ddply(all_table, ~ type, transform, sample_density = impute.mean(sample_density))





# all_table$sample_length <- as.numeric(all_table$sample_length)
# all_table$meanDradiusleft <- ((all_table$`D1.(cm)` +all_table$`D2.(cm)`)/2)/2
# all_table$meanDradiusright <- ((all_table$`D3.(cm)`+all_table$`D4.(cm)`)/2)/2
# all_table$vol_withfield <


# all_table_1cm <- filter(all_table,DiamClass=="01")
# all_table_1cm$vol_withethanol <- all_table_1cm$`volume of the sample`/3.5 * all_table_1cm$`Length.(cm)`
# 
# all_table_10cm <- filter(all_table,DiamClass=="10")
# all_table_10cm$vol_withethanol <- all_table_10cm$`volume of the sample`/5*all_table_10cm$`Length.(cm)`
# all_table_25cm <- filter(all_table,DiamClass=="25")
# all_table_25cm$vol_withethanol <- all_table_25cm$`volume of the sample`/5*all_table_25cm$`Length.(cm)`
# j <- all_table_25cm
# j<- filter(full_prepared,full_prepared$respiration>=0)


# j$type <- paste(j$Species,j$DiamClass)
# j$Class <- as.character(j$Class)
# 
# list1 <- split(all_table,all_table$Species)


temascatter <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                     legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                     legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                     axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                     axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))







ggplot(full_df2,aes(Class,resptemporal,color=Class))+
  geom_point()+
  facet_wrap(~DiamClass+Species,scales="free",ncol=2)
#ORDERFACTOR

names(dat2)[names(dat2) == 'LABEL'] <- "sample_code"

dat3 <- dat2 %>% select(c("sample_code","sample_density"))


write.csv(dat2,"Field_tableW_New_Densities.csv")


files_path <- list.files(path.to.data, full.names = T, pattern = ".csv")
#Guardar el nombre del archivo para usarlo despues como titulo
files_nm <- list.files(path.to.data, pattern = ".csv")

full_df <- list.files(path.to.data, full.names = TRUE, pattern = ".csv") %>% lapply(fread) %>% 
  bind_rows()
full_df$sample_code <- as.character(full_df$sample_code)
full_df2 <- left_join(full_df,dat3, by = "sample_code")


full_df2$sample_density <- full_df2$sample_density/1000 
full_df2$RespCorrectedgrams <- full_df2$RespCorrectedVolume / full_df2$sample_density

full_df2$resp_g_kg_day <- full_df2$RespCorrectedgrams *0.000044*60*60*24*1000 #gr CO2, Kg DW-1 d -1




####This GGPLOT represents automatically the average of each group
full_df2$Class <- as.character(full_df2$Class)
filtrado <- filter(full_df2,Month == "7")
filtrado %>% 
  ggplot(aes(Class,resp_g_kg_day,fill = Class))+
  stat_summary(fun="mean",geom="bar", alpha=.7)+
  stat_summary(fun.data = "mean_cl_normal",
               geom="errorbar",
               width=.2)+
  # geom_bar(stat = "summary",fun="mean")+
  # geom_errorbar(stat= "summary", fun="mean_se")+
  facet_wrap(~DiamClass+Species,scales="free",ncol=2)

