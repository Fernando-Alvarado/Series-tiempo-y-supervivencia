library(flexsurv)
library(survival)
library(tidyverse)
library(ggplot2)
library(ggsurvfit)
library(survminer)
library(dplyr)



##Cargamos los datos


datos <- read.csv("C:/Users/ferna/Documents/Series-tiempo-y-supervivencia/Ayudantias/Riesgo/Breast Cancer.csv")


datos<-na.omit(datos)

#Nos interesa saber el tiempo de supervivencia en meses

tiempo<-datos$Overall.Survival..Months.

tiempo<-as.numeric(tiempo)

delta<-datos$Patient.s.Vital.Status


delta<-ifelse(delta=="Died of Disease",1,0)




datos_1<-cbind(tiempo, delta)
datos_1<-as.data.frame(datos_1)

summary(datos_1)

#delta<-ifelse(datos_1$tiempo<135 & 117<datos_1$tiempo, 1,0)


##de forma que el tiempo indique si hay o no censura se puede hacer con

data<-Surv(tiempo, delta) ##es ejemplo ya que delta no es la variable de censura

head(data)
km_1<-survfit(data~ 1, data = data, type = "kaplan-meier")

summary(km_1)



###Graficamos la funcion de supervivencia

km_plot<-survfit2(Surv(tiempo, delta) ~ 1, data = data) %>% 
  ggsurvfit() +
  labs(
    x = "Días",
    y = "Probabilidad de Supervivencia"
  ) + 
  add_confidence_interval() +
  add_risktable()

x11()
km_plot


##Log rank test
data<-na.omit(data)
data=cbind(data, datos$Cancer.Type.Detailed)
data<-as.data.frame(data)

data<-na.omit(data)
survdiff(Surv(tiempo, delta) ~V3, data = data)



cancer <- survfit(Surv(tiempo, delta) ~ V3, data = data)

x11()
ggsurvplot(cancer, data = data,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, 
           risk.table.col = "strata", 
           linetype = "strata", 
           surv.median.line = "hv", 
           ggtheme = theme_bw(), 
           palette = c("orange", "blue", "green", "red", "lightblue","yellow" ))



