# libraries
library(ggplot2)
library(dplyr)
library(readxl) # 1

path.to.data <- "C:/Users/javie/OneDrive - UPNA/DeWood Project/Respiration Campaign December/EGM_1"
path.to.data <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/EGM_1"
path.to.data <- "D:/OneDrive - UPNA/DeWood Project/Respiration Campaign October 2021/EGM_1"


#path.to.data <- "C:/Users/NG.5027073/Dropbox (MNCN CSIC)/práce/Nagore uam/INVESTIGACION/2020 - DeWood Romania/EGM 4/"

setwd(path.to.data)
#Cargar todos los archivos xlsx de la carpeta
files_path <- list.files(path.to.data, full.names = T, pattern = ".xlsx")
#Guardar el nombre del archivo para usarlo despues como titulo

files_nm <- list.files(path.to.data, pattern = ".xlsx")
#Crear un loops de loops en el que recorremos cada archivo de la lista

lapply(files_nm, function(y){
    print(paste("Leyendo",y))
    resultados <- read_xlsx(y)
    #cambiamos los nombres de las columnas ;plot y Sample code que dan error
    colnames(resultados)[which(names (resultados)==";Plot")]<-"egmplotcode"
    colnames(resultados)[which(names (resultados)=="Sample code")] <- "sample_code"
    #group_by(resultados,egmplotcode)
    #regresioneslineales2 <- lm(resultados$`CO2 Ref`~ resultados$RecNo)

    # general un vector con los valores que hay que iterar
    resultados$ID <- paste(resultados$egmplotcode, resultados$sample_code, resultados$Month, resultados$Day, sep = "_")
    vct <- unique(resultados$ID)

    regresioneslineales <- lapply(vct, function(x) {
      print(paste("Calculando",x))
      tmp <- resultados[resultados$ID == x,]
      lm <- lm(tmp$`CO2 Ref` ~ tmp$RecNo)
      rsq <- broom::glance(lm)$r.squared
      slope <- broom::tidy(lm(tmp$`CO2 Ref` ~ tmp$RecNo))$estimate[2]
      Month <- tmp$Month[1]
      Day <- tmp$Day[1]
      Hour <- tmp$Hour[1]
      Min <- tmp$Min[1]
      ATMP <- tmp$ATMP[1] *  (9.869 * 10^-4) #pasamos la presión a ATM
      #Formatear la tabla con Sample_code1 y Sample_code2 para que el formato sea el mismo que con los EGM_5. Y usar a partir de ahora los mismos
      #Sample_code1 se toma del ID y Samplecode2 se llena con NA.
      Sample_code1 <- gsub("[\\_\\_]", "", regmatches(tmp$ID[1], gregexpr("\\_.*?\\_", tmp$ID[1]))[[1]])
      Sample_code2 <- 0
  
  # guardar un gráfico en el ordenador si r cuadrado está por debajo de 0.8
     if(rsq < 0.8) {
        ggplot(tmp,aes(x = RecNo, y =`CO2 Ref`)) +
          geom_point() +
          geom_smooth(method='lm', formula= y~x) +
          ggtitle(x) +
         theme_bw()
        ggsave(paste0(x, ".png")) 
        } else {paste0(x, " is ok")}
      # guardar valores de pendiente , r cuadrado, día, hora y minuto
     save <- data.frame(ID = x ,slope, rsq,Month,Day,Hour,Min,Sample_code1,Sample_code2,ATMP)
  
})

reg_mat <- bind_rows(regresioneslineales)

# pegar los resultados en la tabla original para tener las columnas de identificación de los datos
# guardamos los datos que tienen ID distinto y la hora el mintuo etc del primer dato así sabemos a qué hora se tomo
# esto por si luego lo vamos a asociar con datos de registradoes de humedad temperatura etc.
# id_data <- distinct(resultados[,c("ID", "egmplotcode", "sample_code","Day", "Month", "Hour", "Min")], ID, egmplotcode, .keep_all = TRUE)
# left_join(reg_mat, id_data , by = "ID")
# guardar los resultados. Ten cuenta que los gardará en la carpeta que hayas definido como working directory 
# con la funcion setwd
write.csv(reg_mat, paste(y,"result.csv"))
}
)

