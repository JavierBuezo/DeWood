# libraries
library(ggplot2)
library(dplyr)
library(readxl) # 1

path.to.data <- "C:/Users/Fomare/OneDrive - UPNA/DeWood Project/Respiration Campaign July 2021/EGM_1/"
path.to.data <- "C:/Users/NG.5027073/Dropbox (MNCN CSIC)/práce/Nagore uam/INVESTIGACION/2020 - DeWood Romania/EGM 4/"

setwd(path.to.data)

resultados <- read_xlsx("EGM4 Measurements_30_07_21.xlsx") 
#resultados$plot <- resultados$egmplotcode 
#print(head(resultados))

#group_by(resultados,egmplotcode)
#regresioneslineales2 <- lm(resultados$`CO2 Ref`~ resultados$RecNo)

# general un vector con los valores que hay que iterar
resultados$ID <- paste(resultados$egmplotcode, resultados$sample_code, resultados$Month, resultados$Day, sep = "_")
vct <- unique(resultados$ID)

tmp.data<-c(1,2,3) 
if(tmp.data[1]!="no value") {
  p = p + geom_point()
}
p + geom_line()

# x <- vct[2]
regresioneslineales <- lapply(vct, function(x) {
  tmp <- resultados[resultados$ID == x,]
  lm <- lm(tmp$`CO2 Ref` ~ tmp$RecNo)
  rsq <- broom::glance(lm)$r.squared
  slope <- broom::tidy(lm(tmp$`CO2 Ref` ~ tmp$RecNo))$estimate[2]
  
  # guardar un gráfico en el ordenador si r cuadrado está por debajo de 0.8
  if(rsq < 0.8) {
    ggplot(tmp,aes(x = RecNo, y =`CO2 Ref`)) +
      geom_point() +
      geom_smooth(method='lm', formula= y~x) +
      ggtitle(x) +
      theme_bw()
    ggsave(paste0(x, ".png")) 
    } else {paste0(x, " is ok")}
  # guardar valores de pendiente y r cuadrado
  save <- data.frame(ID = x ,slope, rsq)
  
})

reg_mat <- bind_rows(regresioneslineales)

# pegar los resultados en la tabla original para tener las columnas de identificación de los datos
# guardamos los datos que tienen ID distinto y la hora el mintuo etc del primer dato así sabemos a qué hora se tomo
# esto por si luego lo vamos a asociar con datos de registradoes de humedad temperatura etc.
id_data <- distinct(resultados[,c("ID", "egmplotcode", "sample_code","Day", "Month", "Hour", "Min")], ID, egmplotcode, .keep_all = TRUE)
left_join(reg_mat, id_data , by = "ID")
# guardar los resultados. Ten cuenta que los gardará en la carpeta que hayas definido como working directory 
# con la funcion setwd
write.csv(reg_mat, "reg_mat.csv")


