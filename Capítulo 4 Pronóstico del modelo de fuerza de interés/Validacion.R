##########
#Este programa contiene los valores de las pruebas no paramétricas de validación
####La creación de la verificación empírica e importación de las bases de datos
#reales que alcanzó la fuerza de interés de la carterA, los 3 gráficos Q-Q Y
#el criterio de Akaike.
#@author: Alejandro García de León Jiménez
#@version: 21/11/2024
#@see Tesis "Pronóstico de carteras de inversión usando modelos dinámicos lineales"
####################################################################################
####################################################################################
library(quantmod)
library(data.table)
library(PerformanceAnalytics) 
library(dplyr)
#Este repositorio en Github contiene la función
#bt.prep() para alinear, limpiar y borrar valores
#ausentes
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)
#Ultima fecha de historia disponible
ACCIONES$Date[1416]
#Box plot de P1
boxplot(P1,main = "Box-Plot del pronóstico de la cartera", border ="darkblue", 
        col ="skyblue")
grid(10)
stats <- boxplot.stats(P1)$stats
#Línea de mediana
segments(x0 = 1 - 0.2, y0 = stats[3], x1 = 1 + 0.2, y1 = stats[3], col = "blue", lty = 2)
#Líneas para los cuantiles
segments(x0 = 1 - 0.2, y0 = stats[2], x1 = 1 + 0.2, y1 = stats[2], col = "red", lty = 2)
segments(x0 = 1 - 0.2, y0 = stats[4], x1 = 1 + 0.2, y1 = stats[4], col = "red", lty = 2)
#Etiquetas para la mediana y los cuantiles
text(x = 1, y = stats[3], labels = paste("Mediana:", round(stats[3], 5)), pos = 2, col = "black", cex = 0.5)
text(x = 1, y = stats[2], labels = paste("Q1:", round(stats[2], 5)), pos = 2, col = "black", cex = 0.5)
text(x = 1, y = stats[4], labels = paste("Q3:", round(stats[4], 5)), pos = 2, col = "black", cex = 0.5)
#Cálculo del rango intercuartílico
rank = quantile(P1, c(0.75),type=2)-quantile(P1, c(0.25),type=2)
names(rank) = "Rango intercuartílico"
rank = round(rank,8)
rank = as.data.frame(rank)
View(rank)
#Pérdidas y ganancias de la cartera
Perd_Gan = exp(P1)
View(Perd_Gan)
#Tabla de frecuencias
#Se crean las categorías de interés
#Se mayor o igual a 1
#o ser menor que 1
Perd_Gan1 = c(1:10)
for(i in 1:10){
  if(Perd_Gan[i]>=1){
    Perd_Gan1[i] = 1
  }else{
    Perd_Gan1[i] = 0
  }
}
#Se crea la variable categórica con los 
#niveles respectivos.
Perd_Gan1 = factor(x = Perd_Gan1, levels = c(0,1) , labels = c("Menor que 1","Mayor que 1"))
#Se crea una tabla de frecuencias.
T_Perdidas_Ganancias <- table(Perd_Gan1)
View(T_Perdidas_Ganancias)
#Se calcula la prueba Kolmogorov
#Smirnov a los objetos serie y P1
ks.test(P1,serie)
#Se calcula la prueba Kolmogorov
#Smirnov a los objetos head(serie) y P1
ks.test(P1,head(serie))
#Se calcula la prueba Kolmogorov
#Smirnov a los objetos tail(serie) y P1
ks.test(P1,tail(serie))
#############################
############################Seccion Validacion empírica
#############################
Symbols1<-c("GCARSOA1.MX","GM.MX", "LIVEPOL1.MX",
            "NSANYN.MX","OSOB.SG","WMT.MX")
#tienen que ir en orden
#alfabético
nh1=17 #días de historia
library("quantmod")
#CARGA DE DATOS DE ACCIONES
start_date1=as.Date("2024-05-12")-nh1 #fecha inicial
#Creación del objeto para guardar los datos
dataEnv1<-new.env()
#obtener los datos
getSymbols.yahoo(Symbols1,env=dataEnv1,from=start_date1, 
                 to= as.Date("2024-05-12"))
bt.prep(dataEnv1,align='remove.na',fill.gaps = T)
stock_prices1 = dataEnv1$prices
#moneda mexicana
head(stock_prices1[,1,with = F])
stock_prices_EQFX1=merge(stock_prices1,join = "inner")
#valores iniciales
x01=stock_prices_EQFX1[nrow(stock_prices_EQFX1),]
ACCIONES1=data.table(Date=as.Date(index(stock_prices_EQFX1)),
                     coredata(stock_prices_EQFX1))
View(ACCIONES1)
B=as.matrix(ACCIONES1[,-1])
Validacion_2 = B %*% pos_eq
Validacion_2
numerador1 = Validacion_2[-1,]
denominador1 = Validacion_2[-11,]
Insumo1 = numerador1/denominador1
Insumo1 = log(Insumo1)
View(Insumo1)
ks.test(P1,Insumo1)
###############
##############
##Gráficos cuantil cuantil
###Gráfico 1
par(mar = c(5, 5, 4, 4) + 0.1)  
qqplot(P1, serie, 
       main = "Comparación Q-Q Plot entre P1 y serie",
       xlab = "Cuantiles de P1", ylab = "Cuantiles de serie",
       pch = 16, # Tipo de punto
       col = "darkblue")# Color de los puntos.
# Recta de la función identidad
abline(a = 0, b = 1, col = "darkgreen", lwd = 2)  
#Cuadrícula
grid(col = "gray", lty = 2)# Color y estilo de la línea de la cuadrícula
####Gráfico 2
Objetivo = c(serie,P1)
Objetivo1 = c(serie,Insumo1)
####Q-Q plot de series completas
par(mar = c(5, 5, 4, 4) + 0.1)  
qqplot(Objetivo,Objetivo1,
       main = "Comparación Q-Q Plot entre Objetivo y Objetivo1",
       xlab = "Cuantiles de Objetivo",
       ylab = "Cuantiles de Objetivo1",
       pch = 16,# Tipo de punto
       col = "darkblue")# Color de los puntos
#Recta de referencia
abline(a = 0, b = 1, col = "darkgreen", lwd = 2) 
# Cuadrícula
grid(col = "gray", lty = 2) # Color y estilo de línea de la cuadrícula
###Gráfico 3
par(mar = c(5, 5, 4, 4) + 0.1) 
####Q-Q plot de mediciones y valores 
#reales alcanzados.
qqplot(P1,Insumo1,
       main = "Comparación Q-Q Plot entre P1 e Insumo1",
       xlab = "Cuantiles de P1",
       ylab = "Cuantiles de Insumo1",
       pch = 16,# Tipo de punto
       col = "darkblue")# Color de los puntos
#Recta de referencia
abline(a = 0, b = 1, col = "darkgreen", lwd = 2) 
# Cuadrícula
grid(col = "gray", lty = 2)# Color y estilo de línea de la cuadrícula
#######
#######Criterio de Información de Akaike
#Se construye la serie de los residuos 
#epsilon
residuos = c(length(r1):1)
#Se repiten las primeras tres
#observaciones.
residuos[c(1,2,3)] = rep(r1[1], times =3)
#Se llena la base de datos.
for(i in 4:length(r1)){
  residuos[i] = r1[i] - sum(phi1*r1[c(i-1,i-2,i-3)])
  - sum(psi1*residuos[c(i-1,i-2,i-3)])
}
#Para las primeras observaciones se repite
#la primera observación
residuos[c(1:3)] = rep(residuos[4], times =3)
#Se muestran los valores más recientes
#de los residuos.
tail(residuos)
###Valor de la logsimilitud
Logsimilitud = (-1415/2)*log(2*pi*VarX) + mean(residuos^2)
#Calculo del AIC usando un enfoque clásico de series de tiempo
library(forecast)
fit <- Arima(r1, order = c(3, 0, 3))  # ARMA(3,3)
AIC(fit)  # Devuelve el AIC del modelo ajustado


