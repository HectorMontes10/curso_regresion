setwd("/home/hectormontes/Escritorio/Maestria/pronosticos/regresion")
# Librerías
library(tidyverse)
library(janitor)
library(openxlsx)
library(flextable)
library(viridis)
library(scales)
library(DT)
library(lubridate)
library(gridExtra)
library(ggplot2)
library(gganimate)
library(animation)
library(gifski)
library(magick)
library(nortest)
library(fitdistrplus)
library(tseries)
library(lmtest)
library(stats)
library(sandwich)

# Simulamos precios bajo distribución sesgada

set.seed(12555)
options(scipen=999)
precios_reales <- 50000*round((75e6+50e6*rgamma(0.95*1500, shape=2, rate=1))/50000,0)
summary(precios_reales)

# Generemos los errores de pronóstico a descontar buscando como objetivo un
# modelo que explique el 90% de la variabilidad del precio real sin contar la
# contaminación

r2<-0.90
sigma_y <- sd(precios_reales)
var_y <- sigma_y^2
var_e <- (1-r2) * var_y 
sigma_e <- sqrt(var_e)
errores <- rnorm(0.95*1500, mean=0, sd=sigma_e)

precios_predichos <- precios_reales-errores

# A partir de los valores predichos y los siguientes parámetros deseados para el
# modelo, deduzca el índice de valuación inmobiliaria

b0 <-15
b1 <- 2

x = (precios_predichos-b0)/b1

# Escale los valores de x para que se muevan entre 0 y 100, pero con x_min = 10

scaled_x <- 10 + 90*(x -min(x))/(max(x)-min(x))
hist(scaled_x)

# Genere el dataset final (el escalado de la x generará betas teóricos 
# diferentes a los propuestos anteriormente, pero la proporción se respetará)

data_inm <- data.frame(
  IVI = scaled_x,
  precio = precios_reales
)

# Realice un gráfico de dispersión y confirme la correlación artificial inducida

p1 <- data_inm %>% 
  ggplot(aes(x= IVI, y= precio/1e6)) +
  geom_point()+ theme_light()

# Contamine el dataset con una muestra de 5% de datos no correlacionados

data_contaminada<-data.frame(
  IVI = runif(1500*0.05,10,100),
  precio = 50000*round((75e6+50e6*rgamma(1500*0.05, shape=2, rate=1))/50000,0)
)
data_inm <- rbind(data_inm, data_contaminada)

# Realice el gráfico de nuevo y confirme un escenario más realista

p2 <- data_inm %>% 
  ggplot(aes(x= IVI, y= precio/1e6)) +
  geom_point()+ theme_light()

# Confirme las características del modelo

model_imb <- lm(data = data_inm, formula = precio~IVI)
summary(model_imb)
data_inm$residuales <- model_imb$residuals

p3 <- ggplot(data = data_inm, aes(x=residuales)) + geom_histogram()
grid.arrange(p1,p2,p3)

# Verifique lo que ocurre si descarta IVI mayores a 60

data_filtered <- data_inm[data_inm$IVI<=60,]
model_imb_adj <- lm(data = data_filtered , formula = precio~IVI)
data_filtered$residuales <- model_imb_adj$residuals
p4 <- ggplot(data = data_filtered, aes(x=residuales)) + geom_histogram()
grid.arrange(p1,p2,p3,p4)

write_csv(data_inm[, c("IVI","precio")], "precios_inm.csv")
