##########
#Este programa realiza el pronóstico usando el filtro de Kalman
#y regresa el pronóstico más la media de la serie original en P1
#@author: Alejandro García de León Jiménez
#@version: 21/11/2024
#@see Tesis "Pronóstico de carteras de inversión usando modelos dinámicos lineales"
####################################################################################
####################################################################################
set.seed(124)
Y = c(1:10)
Pronostico = matrix(nrow = 100, ncol = 10)
for(k in 1:10)
{#Media de la distribución predictiva.
  at = G%*%m
  #Covarianza de la distribución
  #predictiva.
  R_t = G%*%C
  R_t = R_t%*%t(G)
  #Covarianza del ARMA(3,3).
  W = (VarX *(R%*%t(R)))
  #Matriz de covarianzas
  #definitiva de la distribución
  #predictiva
  R_t = R_t + W
  for(i in 1:100){
    #Simulación de la normal multivariada
    Z = as.matrix(rnorm(4,0,1), ncol = 1, nrow = 4)
    A = chol(R_t) #Matriz G^t
    A = t(A)
    X1 = at + (A%*%Z)
    Pronostico[i,k] = F1%*%X1
  }
  Y[k] = mean(Pronostico[,k])
  ft = (F1)%*%at
  et = Y[k]-ft
  Q = F1%*%R_t
  Q = Q %*% t(F1)
  m = solve(Q)%*%et
  m =  t(F1)%*%m
  m= R_t%*%m
  m = at + m
  #Actualización de C
  dif2 = R_t%*%t(F1)
  dif2 = dif2 %*%solve(Q)
  dif2 = dif2 %*% F1
  dif2 = dif2 %*% R_t
  C = R_t-dif2
}
View(Y[c(1:10)])
#Pronóstico agregando mu1
P1 = Y[c(1:10)] + mu1
View(P1)