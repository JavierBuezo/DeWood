library(ggplot2)
library(dplyr)
library(readxl) # 1
library(tidyverse)
library(fs)
library(tidyr)
library(naniar)

path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/"
path.to.data <- "D:/OneDrive - UPNA/DeWood Project"
path.to.data <- "C:/Users/Javier/Documents/DeWood Git/DeWood/Files"


#path.to.data <- "C:/Users/NG.5027073/Dropbox (MNCN CSIC)/pr?ce/Nagore uam/INVESTIGACION/2020 - DeWood Romania/EGM 4/"
#Leemos el excell con los codigos de plot y muestra, separados por fecha
setwd(path.to.data)

Table_Field <- read_xlsx("Table field final.xlsx")

#Eliminar los espacios de los c?digos si los hubiera y cambiar los 1 por 01 para que sea facil de extraer
Table_Field$Code <- str_remove_all(Table_Field$Code,pattern = " ")
Table_Field$Code <- str_replace_all(Table_Field$Code,pattern = "1-",replacement = "01-")

#Clase de Diametro
Table_Field$DiamClass <- substr(Table_Field$Code,1,2)
Table_Field$Plot <- substr(Table_Field$Code,5,5)
Table_Field$SubPlot <- substr(Table_Field$Code,7,7)
Table_Field$Class <- substr(Table_Field$Code,9,9)
Table_Field$Species <- substr(Table_Field$Code,10,11)
colnames(Table_Field)[1] <-"sample_code"

archivos <- paste(getwd(),"/Respiration Campaing April 2022/EGM_2/LM Results/",sep = "")
archivos <- paste(getwd(),"/Respiration Campaing April 2022/EGM_1/LM Results/",sep = "")

# archivos <- paste(getwd(),"/Respiration Campaign October 2021/EGM_2/LM Results/",sep = "")
# archivos <- paste(getwd(),"/Respiration Campaign October 2021/EGM_1/LM Results/",sep = "")

files_nm <- list.files(archivos, pattern =  ".csv")
                  
lapply(files_nm, function(x){
#Abrir archivo y quitarle aquellas filas que sean error
  print(paste("Reading",x))
  
  TablaMedidas2 <- read.csv(paste(archivos,x,sep = ""))
  TablaMedidas <- subset(TablaMedidas2, Sample_code1 !=0)
#Guardar la lista de ID para recorrer 
  codigos <- TablaMedidas$ID

 joinmanual <- lapply(codigos, function(y){
    #Recoger las muestras separadas en Sample1 y sample2
   
    sample1 <- TablaMedidas$Sample_code1[TablaMedidas$ID==y] 
    sample2 <- TablaMedidas$Sample_code2[TablaMedidas$ID==y]
    #Transformar columnas a num?rico porque R las coge como string
    Table_Field[,c("D1 (cm)","D2 (cm)","D3 (cm)", "D4 (cm)","Length (cm)")] <- sapply(Table_Field[,c("D1 (cm)","D2 (cm)","D3 (cm)", "D4 (cm)","Length (cm)")], as.numeric)
      medida <- Table_Field[Table_Field$sample_code == sample1,]

    if(sample2== 0 || is.na(sample2)) #si tenemos sample 1 y 2 hay que trabajarlos de forma diferente. 
    {
     #Guardar los datos correspondientes a ambos codes
      tmpsample1 <- Table_Field[Table_Field$sample_code == sample1,]
      mediaD1tmp1 <- (rowMeans(tmpsample1[,c("D1 (cm)","D2 (cm)")], na.rm = TRUE))/2 #Take the diameter average and divide it by 2, so I work with radius from now on
      mediaD2tmp1 <-(rowMeans(tmpsample1[,c("D3 (cm)","D4 (cm)")], na.rm = TRUE))/2#Take the diameter average and divide it by 2, so I work with radius from now on
      generatriz1 <- sqrt((tmpsample1$"Length (cm)")^2 + ((mediaD1tmp1-mediaD2tmp1)^2)) #Find the generatrix of the cone section
      medida$Areacm2 <- pi*((mediaD1tmp1^2 + mediaD2tmp1^2) + (mediaD1tmp1+mediaD2tmp1) * generatriz1) #Total area
      medida$Volcm3<- ((pi*tmpsample1$"Length (cm)")/3)*(mediaD1tmp1^2+mediaD2tmp1^2+(mediaD1tmp1*mediaD2tmp1)) #Total volume
      
     
        }
      else{ 
        tmpsample1 <- Table_Field[Table_Field$sample_code == sample1,]
      mediaD1tmp1 <- (rowMeans(tmpsample1[,c("D1 (cm)","D2 (cm)")], na.rm = TRUE))/2#Take the diameter average and divide it by 2, so I work with radius from now on
      mediaD2tmp1 <-(rowMeans(tmpsample1[,c("D3 (cm)","D4 (cm)")], na.rm = TRUE))/2#Take the diameter average and divide it by 2, so I work with radius from now on
      
      generatriz1 <- sqrt((tmpsample1$"Length (cm)")^2 + ((mediaD1tmp1-mediaD2tmp1)^2))
      Areatmp1 <- pi*((mediaD1tmp1^2 + mediaD2tmp1^2) + (mediaD1tmp1+mediaD2tmp1) * generatriz1)
      Voltmp1 <- ((pi*tmpsample1$"Length (cm)")/3)*(mediaD1tmp1^2+mediaD2tmp1^2+(mediaD1tmp1*mediaD2tmp1))
      
      tmpsample2 <- Table_Field[Table_Field$sample_code == sample2,]
      
      mediaD1tmp2 <- (rowMeans(tmpsample2[,c("D1 (cm)","D2 (cm)")], na.rm = TRUE))/2#Take the diameter average and divide it by 2, so I work with radius from now on
      mediaD2tmp2 <- (rowMeans(tmpsample2[,c("D3 (cm)","D4 (cm)")], na.rm = TRUE))/2#Take the diameter average and divide it by 2, so I work with radius from now on
      
      generatriz2 <- sqrt((tmpsample2$"Length (cm)")^2 + ((mediaD1tmp2-mediaD2tmp2)^2))
      Areatmp2 <- pi*((mediaD1tmp2^2 + mediaD2tmp2^2) + (mediaD1tmp2+mediaD2tmp2) * generatriz2)
      Voltmp2 <- (pi/3)*tmpsample2$"Length (cm)"*(mediaD1tmp2^2+mediaD2tmp2^2+(mediaD1tmp2*mediaD2tmp2))
      medida$Areacm2 <- Areatmp1 + Areatmp2
      medida$Volcm3 <- Voltmp1 + Voltmp2
      
      
      tmpsampleambas <- bind_rows(tmpsample1,tmpsample2)
      
      medida$`FW (g)` <- mean(tmpsampleambas$`FW (g)`, na.rm=TRUE)
      medida$`DW (g)` <- mean(tmpsampleambas$`DW (g)`, na.rm =TRUE)
      medida$`Bark (%)` <-  mean(tmpsampleambas$`Bark (%)`,na.rm =TRUE)
      medida$`Water content (g)` <- mean(tmpsampleambas$`Water content (g)`,na.rm =TRUE)
      medida$`Water percentage` <- mean(tmpsampleambas$`Water percentage`,na.rm =TRUE)
        

        
      }
    datos <- TablaMedidas[TablaMedidas$ID == y,]
    mergeado <- merge(medida,datos)
        
  })
  reg_mat<-bind_rows(joinmanual) #no termino de entender c?mo funciona esto ni por qu?, s? que funciona. Preguntar a Nagore
  reg_mat[paste("...",14:29,sep = "")] <- NULL # De los excells quedan columnas vac?as. Eliminarlas
  reg_mat$slope[reg_mat$slope < 0.03] <- 0 #Los valores menores que 0.03 se consideran 0. 
  names(reg_mat) <- gsub("\\ ", "", names(reg_mat)) #Eliminar los espacios en los nombres de columna
  write.csv(reg_mat,paste(x,"JoinWithFieldTable.csv",sep = ""))
  
})

.#