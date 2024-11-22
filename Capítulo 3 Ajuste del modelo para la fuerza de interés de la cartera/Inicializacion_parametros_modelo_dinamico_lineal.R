##########
#Este programa inicializa los parámetros del modelo dinámico
#lineal para llevar a cabo el pronóstico
#@author: Alejandro García de León Jiménez
#@version: 21/11/2024
#@see Tesis "Pronóstico de carteras de inversión usando modelos dinámicos lineales"
####################################################################################
####################################################################################
R = matrix(ncol = 1, nrow = 4, c(1, psi1[1], psi1[2], psi1[3]), byrow = T)
View(R)
G = matrix(ncol = 1, nrow = 3, c(phi1[1], phi1[2], phi1[3]), byrow = T)
G = cbind(G,diag(x=1, nrow = 3, ncol = 3))
G = rbind(G,rep(0, times = 4))
G
F1= matrix(nrow = 1, diag(1, ncol = 4)[1,], byrow = T)
F1
sd = sqrt(VarX)
sd
#Media y matriz de covarianza iniciales
#Valores iniciales
m = rep(0,times = 4)
C = diag(1, ncol = 4, nrow = 4)
View(m)
View(C)
Valor_inicial = c(0,0,0,0)
#Media  de la distribucion predictiva
at = G%*%m
View(at)
#Matriz de covarianza distribucion predictiva
R_t = (G%*%C)
R_t = R_t%*%t(G) 
R_t = R_t +  (VarX *(R%*%t(R)))
View(R_t)

