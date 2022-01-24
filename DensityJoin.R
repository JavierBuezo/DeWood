library(data.table)
library(plyr)
library(dplyr)
library(stringr)
library(readxl)

##############################  Gather all the data from different CSV ######################

hexToText <- function(msg){
  hex <- sapply(seq(1, nchar(as.character(msg)), by=2), 
                function(x) substr(msg, x, x+1))
  hex <- subset(hex, !hex == "00")
  gsub('[^[:print:]]+', '', rawToChar(as.raw(strtoi(hex, 16L))))
}



path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Final results/CSV/"
#Cargar todos los archivoscsv de la carpeta
files_path <- list.files(path.to.data, full.names = T, pattern = ".csv")
#Guardar el nombre del archivo para usarlo despues como titulo
files_nm <- list.files(path.to.data, pattern = ".csv")
full_df <- fread("FinalResults_WDensities.csv")
full_df <- list.files(path.to.data, full.names = TRUE) %>% lapply(fread) %>% 
  bind_rows()

full_prepared <- full_df[ , c("Bark(%)","ID","Waterpercentage","Class", "DiamClass","Species","T3","Soil_moist","RespCorrectedArea","Plot...17","SubPlot","rsq","RespCorrectedVolume","Month","RespCorrectedgrams")]  

full_df$V1...1 <- NULL
full_df$V1...2 <- NULL
#############################################


field_table <- read_xlsx("C:/Users/javie/OneDrive - UPNA/DeWood Project/Table field final.xlsx")

################Read density table#####################################

path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/"

setwd(path.to.data)
dens_table <- fread("Density_table.csv")
colnames(dens_table)[1] <- "sample_code"
colnames(dens_table)[1] <- "LABEL"

dens_table$LABEL <- as.character(dens_table$LABEL)
                                  
final_df <- left_join(dens_table,field_table, by = "LABEL")
# 
# final_df$RespCorrectedgrams <- final_df$RespCorrectedVolume/final_df$Density_g_cm3
# path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Final results/CSV/"
# setwd(path.to.data)
# write.csv(final_df,"FinalResults_WDensities.csv")

final_df$Code <- str_remove_all(final_df$Code,pattern = " ")
final_df$Code <- str_replace_all(final_df$Code,pattern = "1-",replacement = "01-")

substr(final_df$Code,1,2)
substr(final_df$Code,10,11)


final_df$DiamClass <- substr(final_df$Code,1,2)
final_df$Plot <- substr(final_df$Code,5,5)
final_df$SubPlot <- substr(final_df$Code,7,7)
final_df$Class <- substr(final_df$Code,9,9)
final_df$Species <- substr(final_df$Code,10,11)

final_df$type <- paste(final_df$Species,final_df$DiamClass,sep="_")
final_df$type2 <- paste(final_df$Species,final_df$DiamClass,final_df$Class,sep="_")
final_df2 <- final_df[,c("LABEL","Density_g_cm3","type2")]

final_df2 <- final_df2[!(final_df2$LABEL==15),]
unique(final_df2$type2)
prueba <- na.omit(final_df2) %>% group_by(type2) %>% 
  summarise_each(funs(mean,sd,n=n()))

writeClipboard(as.character(prueba))

