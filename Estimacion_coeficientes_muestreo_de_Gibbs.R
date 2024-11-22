##########
#Este programa implementa el muestreo de Gibbs para estimar los coeficientes
##del modelo ARMA(3,3) bajo supuestos de normalidad
#@author: Alejandro García de León Jiménez
#@version: 21/11/2024
#@see Tesis "Pronóstico de carteras de inversión usando modelos dinámicos lineales"
####################################################################################
####################################################################################
set.seed(134)
#Valor inicial
#Se crean matrices para almacenar las simulaciones
#De los coeficientes del modelo ARMA(p,q)
psi = matrix(ncol = 3, nrow = 100)
phi = matrix(ncol = 3, nrow = 100)
for(i in 1:100){
  r = rnorm(3,0,(sqrt(VarX)))
  #Un valor admisible inicial para la distribución. 
  x0 = c(1,1,1)
  y0 = c(1,1,1)
  x = matrix(0,ncol = 3, nrow = 100)
  y = matrix(0,ncol = 3, nrow = 100)
  x[i,] = x0
  y[i,] = y0
  #Parte autorregresiva.
  #Se genera una estimación para la varianza.
  for(j in 3:1){
    #Que valga 0 en la (i,j)-ésima coordenada.
    x[i,j] = 0
    k = (x[i,1]*r[1])^2 + (x[i,2]*r[2])^2 + (x[i,3]*r[3])^2
    k = k + (y[i,1])^2 + (y[i,2])^2 + (y[i,3])^2
    k = sqrt(k)
    #Se simulan los coeficientes psi usando esta varianza.
    psi[i,j] = rnorm(1,0,k)
    x[i,j] =  psi[i,j]}
  #Parte de promedios móviles.
  for(m in 3:1){
    y[i,m] = 0
    k = (x[i,1]*r[1])^2 + (x[i,2]*r[2])^2 + (x[i,3]*r[3])^2
    k = k + (y[i,1])^2 + (y[i,2])^2 + (y[i,3])^2
    k = sqrt(k)
    #Se simulan los coeficientes psi usando esta varianza.
    phi[i,m] = rnorm(1,0,k)
    y[i,m] = phi[i,m]}}
#Como resultado en el vector x se agrupan los coeficientes de la parte autorregresiva.
#En el vector y se agrupan los coeficientes de la parte de promedios móviles.
#Primeros valores de psi
head(psi)
#Primeros valores de phi
head(phi)
#Estimadores de los coeficientes del modelo ARMA(3,3).
phi1 = c(mean(phi[,1]),mean(phi[,2]),mean(phi[,3]))
psi1 = c(mean(psi[,1]),mean(psi[,2]),mean(psi[,3]))
#Se observan los valores de las estimaciones.
head(phi1)
head(psi1)
