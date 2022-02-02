library(openxlsx)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
path.to.data  <- "C:/Users/Javier/Documents/DeWood Git/DeWood/Files/"
path.to.data  <- "C:/Users/javie/Documents/DeWood GitHub/DeWood/Files"
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
ggplot(all_table,aes(Class,sample_density,color=Class))+
      geom_boxplot()+
      facet_wrap(~DiamClass+Species,scales="free",ncol=2)
#ORDERFACTOR


plots <- lapply(j, function(x) ggplot(x, aes(DiamClass, sample_density,color =Class)) + 
                  temascatter +
                  geom_point(aes(color = Class),size=1))+
                  
            
                  
plots
plot <- ggplot(all_table, aes(DiamClass,sample_density, color=Species)) + 
  temascatter +
  geom_point(aes(color = Class),size=1) +
  geom_abline(slope = 1,intercept = 0)
  # facet_wrap(~DiamClass)
  # geom_smooth(method="lm")
  # labs(y=expression("g CO"[2]*" min"^-1*"m"^2),x="Temperature ?C")+
  # geom_smooth(method='lm')+
  # facet_wrap(~Class)+
  # stat_regline_equation(aes(label = ..rr.label..))
plots

  
field_table$density <- pi * field_table$`D1.(cm)`                          
