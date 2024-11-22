##########
#Este programa construye la serie objetivo a pronosticar de la fuerza de interés de la cartera
#Aplica prueba Dickey Fuller ajustada, centraliza la serie en 0  y estima la 
#varianza de la misma
#@author: Alejandro García de León Jiménez
#@version: 21/11/2024
#@see Tesis "Pronóstico de carteras de inversión usando modelos dinámicos lineales"
####################################################################################
####################################################################################
#Se calculan los cocientes dentro de la ecuación
#anterior.
numerador = portafolio[-1,]
denominador = portafolio[-length(portafolio),]
Insumo = numerador/denominador
#Se calculan los logaritmos de los cocientes 
#obteniendo las fuerzas de interés y  
#almacenándolos en la variable Insumo
Insumo = log(Insumo)
#Se imprimen los primeros valores de la cartera
head(Insumo)
#Solo se trata de una conversión de objeto 
# a ts en el software.
serie = ts(data = Insumo, frequency = 360)
library("tseries")
adf.test(serie, k=0)
mu1 = mean(serie)
mu1
r1 = serie -mu1
#Se muestran los primeros valores de la 
#nueva serie r1
head(r1)
set.seed(122)
#Se almacena en un vector Simulacion, 
#una corrida de números aleatorios
#size indica cuantos de estos serán
#generados y x el conjunto de datos.
Simulacion = sample(size = 100, x = r1)
#Se visualizan los primeros valores
#de la corrida
head(Simulacion)
#Se calcula la varianza muestral de
#la corrida aleatoria almacenada
#en la variable Simulacion
VarX = var(Simulacion)
#Se imprime en pantalla 
#el valor de la varianza
VarX