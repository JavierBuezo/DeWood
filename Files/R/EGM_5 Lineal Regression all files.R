# libraries
library(ggplot2)
library(dplyr)
library(readxl) # 1
library(tidyverse)
library(fs)
library(xlsx)
library(tidyr)
library(naniar)
library(data.table)
path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign December/EGM_2/Joint/"
path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign 09 September/"
path.to.data <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign October 2021/EGM_2/Joint/"
#path.to.data <- "C:/Users/NG.5027073/Dropbox (MNCN CSIC)/práce/Nagore uam/INVESTIGACION/2020 - DeWood Romania/EGM 4/"
#Leemos el excell con los codigos de plot y muestra, separados por fecha
setwd(path.to.data)

#Cargar todos los archivos xlsx de la carpeta
files_path <- list.files(path.to.data, full.names = T, pattern = ".xlsx")
#Guardar el nombre del archivo para usarlo despues como titulo
files_nm <- list.files(path.to.data, pattern = ".csv")



lapply(files_nm, function(y){
          message(paste("Abriendo", y))
          Tablafinal <- read.csv(y)
          
          #Empezamos a formatear la tabla para realizar el ciclo de regresiones. Hace falta una columna que marque las medidas que forman parte de la recta.
          #Creamos una columna auxiliar
    
          Tablafinal$marcastart<-Tablafinal$data_format
 
          #Vaciamos los valores que no son Start o End (M3, M5 y R5)
          TablatempnoM3 <- Tablafinal %>% replace_with_na_at(.vars = c("sample_code", "marcastart"), 
                                            condition = ~ .x == "M3")
          TablatempnoM5 <- TablatempnoM3 %>% replace_with_na_at(.vars = c("sample_code", "marcastart"), 
                                                             condition = ~ .x == "M5")
          TablatempnoR5 <- TablatempnoM5 %>% replace_with_na_at(.vars = c("sample_code", "marcastart"), 
                                                                condition = ~ .x == "R5")
          #Rellenamos las filas que ahora son NA con el valor de encima. De esta manera si la fila forma parte de una medida tendrá "Start", esto será lo que usemos como filtro.
          resultados <- TablatempnoR5 %>% fill(marcastart)
          
          #Empezamos la regresión como en el EGM-4. Creando un ID único.
          resultados$ID <- paste(resultados$egmplotcode, resultados$sample_code, resultados$Month, resultados$Day, sep = "_")
          vct <- unique(resultados$ID)
          if(!"Sample_code1" %in% colnames(resultados)){
          colnames(resultados)[which(names (resultados)=="sample_code")] <- "Sample_code1"
          }
          if(!"Sample_code2" %in% colnames(resultados)){
            resultados$Sample_code2 <- NA
          }

          #Comenzamos el ciclo
          regresioneslineales <- lapply(vct[-2], function(x) {
            tmp <- resultados[resultados$ID == x,]  #Se filtra aquellas filas que coincidan con el ID
            tmp2 <- tmp[tmp$marcastart == "Start",] #Hay que hacer un filtrado más, en el que además la columna marcastart ha de ser Start
            
            #En caso de que la fila solo termine con NA
            tmp2[is.na(tmp2)] <- 0
            
            # tmp2$'CO2.Ref' <- df$'CO2.Ref'*!c(FALSE,diff(tmp2$'CO2.Ref',lag=1)<0),
            # tmp2 %>% mutate('CO2.Ref' = ifelse('CO2.Ref' < lag('CO2.Ref,')))
            #Añadir un IF en caso de que haya un código sin medidas. Le dará robustez para el futuro sin preocuparme de ojear el archivo a mano
            if(nrow(tmp2) != 0)
              { 
              
              lm <- lm(tmp2$`CO2.Ref` ~ tmp2$RecNo)
                rsq <- broom::glance(lm)$r.squared
               slope <- broom::tidy(lm(tmp2$`CO2.Ref` ~ tmp2$RecNo))$estimate[2]
               Month <- tmp2$Month[1]
               Day <- tmp2$Day[1]
               Hour <- tmp2$Hour[1]
              Min <- tmp2$Min[1]
              ATMP <- (tmp2$Atm_press[1]/10) *  (9.869 * 10^-4) #Pasamos la presion  a atm
              Sample_code1 <- tmp2$Sample_code1[1]
              Sample_code2 <- tmp2$Sample_code2[1]
     
                # guardar un gráfico en el ordenador si r cuadrado está por debajo de 0.8
                if(rsq < 0.8) {
                  ggplot(tmp2,aes(x = RecNo, y =`CO2.Ref`)) +
                    geom_point() +
                    geom_smooth(method='lm', formula= y~x) +
                    ggtitle(x) +
                    theme_bw()
                  ggsave(paste0(x, ".png"),path = "imagestocheck") 
                } else {print(paste0(x, " is ok"))}
                # guardar valores de pendiente y r cuadrado
                save <- data.frame(ID = x ,slope, rsq,Month,Day,Hour,Min,Sample_code1,Sample_code2,ATMP)
            }
            else {
              message(paste("0 filas en ",x))
              }
            
          })

          reg_mat <- bind_rows(regresioneslineales)
            

            #Guardamos el archivo como un CSV para poder usarlo en la función. n.

            
            write.csv(reg_mat,paste(substr(y,1,nchar(y)-5),"result.csv"))
})

