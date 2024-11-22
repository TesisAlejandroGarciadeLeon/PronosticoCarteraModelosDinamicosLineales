##########
#Este programa Resuelve el problema de Markowitz para la cartera utilizada en la tesis profesional.
#@author: Alejandro García de León Jiménez
#@version: 21/11/2024
#@see Tesis "Pronóstico de carteras de inversión usando modelos dinámicos lineales"
####################################################################################
####################################################################################
#Renglones debajo de la matriz de covarianza	
MATRIZ_MARKOWITZ = rbind(W,rf,rend)
#Columnas a la derecha de la matriz de covarianza
c1 = t(t(rep(-1, times = nrow(W))))
c1 = rbind(c1, t(t(c(0,0))))
c2 = t(-rend)
c2 = rbind(c2, t(t(c(0,0))))
#Matriz a resolver
MATRIZ_MARKOWITZ = cbind(MATRIZ_MARKOWITZ,c1,c2)
#Se calcula el rendimiento que se desea sea de .7
#Por ende 1.7 = exp(re) donde re
# es la fuerza de interés; se almacena re a continuación
re = log(1.7)
#Se repite el valor 0, 6 veces
b = t(t(rep(0, times= 6)))
#Se concatena por renglones la variable re y el 
#vector de ceros, b.
b = rbind(b,t(t(c(1,re))))
#####Resolucion problema de Markowitz
Markowitz_coeficientes = solve(MATRIZ_MARKOWITZ)%*%b
Markowitz_coeficientes
pos_eq=t(t(Markowitz_coeficientes[c(1:6),1]))
pos_eq
portafolio = A %*% pos_eq
head(portafolio)

