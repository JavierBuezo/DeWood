library(ggpubr)
library(plyr)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(readxl)
library(data.table)
library(dplyr)
library(reshape2)


finalfiltered <- fread("C:/Users/javie/Documents/DeWood GitHub/DeWood/Files/TerosAndNagoSensors_byHour.csv")


########Function to find start values
##########################
start_val_sigmoid <- function(x, y) {
  fit <- lm(log(y[which.max(x)] - y + 1e-6) ~ x)
  list(
    a = y[which.max(x)],
    r = unname(-coef(fit)[2]))
}
##########################
lista <- start_val_sigmoid(finalfiltered$avgWC,finalfiltered$avgTerosWC)
lista
m <- NULL
m <- nls(avgTerosWC ~ a*exp(r* avgWC), data = finalfiltered,start=lista,trace=TRUE)

# m <- nls(avgWC ~ a*exp(r* avgTerosWC), data = finalfiltered,start=lista)

# m <- nls(avgTerosWC ~ a*exp(r* avgWC), data = finalfiltered,start= list(a=5842404.74, r=-66.72))

m
p <- coef(m)
p
dev.off()
plot(finalfiltered$avgWC,finalfiltered$avgTerosWC)
curve(p["a"]*exp(p["r"]*x),lwd=2,col="Red",add=TRUE)


#########Trying SCALING THE VARIABLES###########
finalfiltered$avgTerosWCcentered <- scale(finalfiltered$avgTerosWC)
finalfiltered$avgWCcentered <- scale(finalfiltered$avgWC)
lista<- start_val_sigmoid(finalfiltered$avgTerosWCcentered,finalfiltered$avgWCcentered)
lista
m <- NULL
m <- nls(avgTerosWCcentered ~ a*exp(r* avgWCcentered), data = finalfiltered,start=list(a=0.5,r=0.2),trace=TRUE)
p <- coef(m)
p
dev.off()
plot(finalfiltered$avgWCcentered,finalfiltered$avgTerosWCcentered)
curve(p["a"]*exp(p["r"]*x),lwd=2,col="Red",add=TRUE)



