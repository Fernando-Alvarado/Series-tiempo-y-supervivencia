library(flexsurv)
library(survival)
library(tidyverse)
library(ggplot2)
library(ggsurvfit)
library(survminer)
library(dplyr)
library(ranger)
library(ggfortify)


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

###

##Ahora usando el lado del tumor más grande
data<-Surv(tiempo, delta)

km_Lado<-survfit(data~ datos$Primary.Tumor.Laterality, data = datos, type = "kaplan-meier")
summary(km_Lado)

x11()
ggplot2::autoplot(km_Lado)




###Tipo de Cirugía

km_ciru<-survfit(data~ datos$Type.of.Breast.Surgery, data = datos, type = "kaplan-meier")
summary(km_ciru)

x11()
ggplot2::autoplot(km_ciru)


##Cox proportional models

cox_1<-coxph(Surv(tiempo, delta) ~ datos$Age.at.Diagnosis+ datos$Tumor.Size  + datos$Lymph.nodes.examined.positive, data = datos)
cox_1



summary(cox_1)


##Verificación de supuestos

##Proporcionalidad

riesgos<-cox.zph(cox_1)
riesgos

##H_0:la tasa de riesgo de un individuo es relativamente constante en el tiempo

x11()
ggcoxzph(riesgos)

##Analisis de Residuales


x11()
ggcoxdiagnostics(cox_1,  type = "schoenfeld", ox.scale = "time")


ggcoxdiagnostics(cox_1, type = "dfbetas", ox.scale = "observation.id", title = "Residuales dfbetas", 
                 subtitle = "dfbetas vs id", caption = "Análisis residual VS Betas")


