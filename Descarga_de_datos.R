##########
#Este programa importa datos de acciones de Yahoo Finance, define rendimientos promedio y matrices de covarianzas y correlación de precios y rendimientos logarítmicos
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
#ACCIONES Y DIVISAS
#Cargar las seis acciones
Symbols<-c("GCARSOA1.MX","GM.MX", "LIVEPOL1.MX","NSANYN.MX","OSOB.SG", "WMT.MX")
nh=3660 
#CARGA DE DATOS DE ACCIONES
start_date=as.Date("2024-04-26")-nh #fecha inicial
dataEnv<-new.env()
#obtener los datos
getSymbols.yahoo(Symbols,env=dataEnv,from=start_date, to= as.Date("2024-04-26"))
bt.prep(dataEnv,align='remove.na',fill.gaps = T)
stock_prices = dataEnv$prices
#En pesos mexicanos
head(stock_prices[,1,with = F])
stock_prices_EQFX=merge(stock_prices,join = "inner")
#Valores iniciales
x0=stock_prices_EQFX[nrow(stock_prices_EQFX),] 
ACCIONES=data.table(Date=as.Date(index(stock_prices_EQFX)),coredata(stock_prices_EQFX))
head(ACCIONES)
#Se crea un vector de unos para la matriz de 
#Markowitz
rf = t(rep(1, times = 6))
#Se crea la matriz A con la historia del objeto 
#ACCIONES sin considerar el atributo date; por
# eso se retira la primera columna
A = as.matrix(ACCIONES[,-1])
#Se define la variable rend la cual se obtiene de
#aplicar  las definiciones anteriores primero al 
#instrumento inicial
rend = log(A[-1,1]/A[-nrow(ACCIONES),1])
#se toma la media de las columnas de rend
rend = mean(as.matrix(rend))
#Se aplica el anterior procedimiento a las columnas
# restantes
for(i in 2:6){
  k = i
  d = log(A[-1,k]/A[-nrow(A),k])
  d = mean(as.matrix(d))
  rend = cbind(rend,d)}
#Se aplica doble transpuesta para compatibilidad 
#entre objetos.
rend = t(t(rend))
#Se imprime el encabezado de rend
head(rend)
#Cálculo de covarianzas
W = as.matrix(ACCIONES[-1,-1])
W = W/as.matrix(ACCIONES[-nrow(ACCIONES),-1])
W = cov(log(W))
head(W)
#Cálculo de correlación
W1 = as.matrix(ACCIONES[-1,-1])
W1 = W1/as.matrix(ACCIONES[-nrow(ACCIONES),-1])
W1 = cor(log(W1))
head(W1)
#Matriz de correlación
#de los precios de las 
#acciones
cor(as.matrix(ACCIONES[,-1]))
#Matriz de covarianza
#de los precios de las 
#acciones
cov(as.matrix(ACCIONES[,-1]))
#Gráfico de correlación
library(corrplot)
W3 = cor(as.matrix(ACCIONES[,-1]))
corrplot(W3)
