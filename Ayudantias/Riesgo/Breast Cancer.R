
library(flexsurv)
library(survival)
library(tidyverse)
library(here)

##Cargamos los datos


datos <- read.csv("C:/Users/ferna/Documents/Series-tiempo-y-supervivencia/Ayudantias/Riesgo/Breast Cancer.csv")

datos<-na.omit(datos)

#Nos interesa saber el tiempo de supervivencia en meses

tiempo<-datos$Overall.Survival..Months.

tiempo<-as.numeric(tiempo)

delta<-datos$Overall.Survival.Status

delta<-ifelse(delta=="Living",0,1)


datos_1<-cbind(tiempo, delta)
datos_1<-as.data.frame(datos_1)


##de forma que el tiempo indique si hay o no censura se puede hacer con

Surv(tiempo, delta)[1:5,] ##es ejemplo ya que delta no es la variable de censura


###Ajuste de Modelos Parametricos##

expo<- flexsurvreg(Surv(tiempo, delta) ~ 1, data = datos_1, dist = "exp")  
expo



##Graficas de las funciones

###Supervivencia

x11()
plot(expo, type = "survival", ci = F, main = "Probabilidad de Supervivencia", 
     conf.int = F, xlab = "Tiempo (Meses)", ylab = "Probabilidad de Supervivencia")



##Hazard acumulado o Riesgo Acumulado

x11()
plot(expo, type = "cumhaz", ci = F, main = "Riesgo Acumulado", conf.int = F, xlab = "Tiempo (Meses)", ylab = "Riesgo Acumulado")



##Riesgo realtivo o Hazard

x11()
plot(expo, type = "hazard", ci = F, main = "Riesgo Relativo", xlab = "Tiempo (Meses)", ylab = "Riesgo Relativo")

###Bondad de Ajuste###

library("fitdistrplus")

#Prueba basada en la Kurtosis y el sesgo

x11()
descdist(datos_1$tiempo, boot = 500)


plot(datos_1$tiempo)

fn <- fitdist(datos_1$tiempo, "norm")
fg <- fitdist(datos_1$tiempo, "gamma")
fu <- fitdist(datos_1$tiempo, "unif")
par(mfrow = c(2, 2))
plot.legend <- c("norm", "gamma", "unif")
denscomp(list(fn, fg, fu), legendtext = plot.legend)
qqcomp(list(fn, fg, fu), legendtext = plot.legend)
cdfcomp(list(fn, fg, fu), legendtext = plot.legend)
ppcomp(list(fn, fg, fu), legendtext = plot.legend)

##Pruebas de Bondad de Ajuste

gofstat(list(fn, fg, fu))

##Creamos la función

fexpo<-fitdist(datos_1$tiempo, "exp")
summary(fexpo)

