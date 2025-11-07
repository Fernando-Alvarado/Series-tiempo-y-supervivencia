library(forecast)
library(tseries)
library(ggplot2)
library(ggfortify)
library(quantmod)
library(caTools)
library(Metrics)
library(tidyverse)
library(here)


## Graficamos la serie de Tiempo

datos = read.csv(here("Proyecto3","EJE6", "co2MaunaLoa.csv"))

datos$t = c(1:132)
x11()
ggtsdisplay(datos$x)
## Es muy claro observar que no es estacionaria, la media es creciente y se puede observar
## periodicidad. La varianza parece constante

datos_diff <- diff(datos$x, lag = 12)#Haciendo la serie estacionaria
#x11()
#ggtsdisplay(datos_diff)
## Despues de realizar los procesos de diferenciaci�n, la gr�fica parece m�s estable.
## Se elimin� la tendencia.
## Observando la gr�fica de los AFC, notamos que hay picos cada 12 lags, sugiriendo 
## que no es estacionaria.

resultado = diff(datos_diff, lag = 1)
x11()
ggtsdisplay(resultado)

## Apliquemos suavizamiento exponencial

datosExp = ets(datos$x)
datos$exp = datosExp$fitted
datos$resExp = datosExp$residuals

##ggplot(data=datos,aes(x=t,y=x,group = 1))+
#  labs(x='',y='')+
#  geom_line(col='black')+
#  geom_line(aes(x=t,y=exp),col='red')+
#  geom_point(size=.5)+
#  theme_minimal()

ggplot(data=datos,aes(x=t,y=resExp,group = 1))+
  labs(x='',y='')+
  geom_line(col='black')+
  geom_point(size=.5)+
  theme_minimal()

## De igual manera, est� centrado en cero y parece que hay casi la misma cantidad de
## residuales que est�n arriba como por abajo de la recta y=0.
## No obstante, de nuevo apreciamos que hay picos que aparecen a distancias constantes 
## de tiempo, sugieriendo, nuevamente, que no es estacionario el proceso.

# La prueba de Ljung-Box nos puede ayudar a argumentar lo anterior, obtenemos un 
# p-value = 0.000947, concluyendo que hay autocorrelaci�n de los datos, i.e., no es 
# ruido blanco.
Box.test(datos$resExp,type='Ljung-Box')
#H0: No hay autocorrelacion => es un ruido blanco 



#Tenemos suficiente evidencia para rechazar H0
#Por lo tanto tenemos autocorelacion y no es ruido blanco