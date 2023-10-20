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
library(reshape2)

# Simulamos precios bajo distribución sesgada

set.seed(12555)
options(scipen=999)

#Creando un modelo teórico:

intercepto <- 20e6
b1 <- -100e6
b2 <- 200e6 
b3 <- 270e6
b4 <- 300e6
b5 <- 300e6
b6 <- 250e6
b7 <- 50e6
b8 <- -100e6

porcentaje_1 <- runif(1500,0,0.4)
porcentaje_2 <- runif(1500,0,0.5*(1-porcentaje_1))
porcentaje_3 <- runif(1500,0,0.5*(1-porcentaje_1-porcentaje_2))
porcentaje_4 <- runif(1500,0,0.5*(1-porcentaje_1-porcentaje_2-
                                    porcentaje_3))
porcentaje_5 <- runif(1500,0,0.5*(1-porcentaje_1-porcentaje_2-
                                    porcentaje_3-porcentaje_4))
porcentaje_6 <- (1-porcentaje_1-porcentaje_2-porcentaje_3-porcentaje_4-
                   porcentaje_5)

base_ICI <- 0.3*porcentaje_4 + 0.2*porcentaje_5+0.05*porcentaje_6
mean_ICI_e <- mean(base_ICI)
sd_ICI_e <- sd(base_ICI)
ICI <- base_ICI + rnorm(1500, mean=0.10*mean_ICI_e, sd=0.10*sd_ICI_e)
base_tc <-  0.3*porcentaje_1 + 0.2*porcentaje_2+0.05*porcentaje_3
mean_tc_e <- mean(base_tc)
sd_tc_e <- sd(base_tc)
tc <- base_tc + rnorm(1500, mean=0.05*mean_tc_e, sd=0.07*sd_tc_e) 

price <- intercepto + b1*porcentaje_1 + b2*porcentaje_2 + b3*porcentaje_3 + 
  b4*porcentaje_4 + b5*porcentaje_5 + b6*porcentaje_6 + b7*ICI + b8*tc

var_price <- var(price)
r2<-0.8
var_e <- (1-r2)*var_price
sd_e <- sqrt(var_e)

price <- price + rnorm(1500, mean=0, sd=sd_e)

df_prices <- data.frame(
  p_est_1 = porcentaje_1,
  p_est_2 = porcentaje_2,
  p_est_3 = porcentaje_3,
  p_est_4 = porcentaje_4,
  p_est_5 = porcentaje_5,
  p_est_6 = porcentaje_6,
  ICI = ICI,
  tc = tc,
  price = price
)

matriz_correlaciones <- cor(df_prices)
matriz_correlaciones_melted <- melt(matriz_correlaciones)

ggplot(matriz_correlaciones_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

correlaciones <- cor(df_prices)
print(correlaciones)

ggplot(data=df_prices, aes(x=price))+
  geom_histogram()

pairs(df_prices)
write_csv(df_prices, "precios_inm_multiple.csv")
