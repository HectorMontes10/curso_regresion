# install.packages('ISLR')
#install.packages('pracma')
library(pracma)
library(ISLR)
set.seed(777)

vector_x = seq(1,10)
y = 2 + 1*vector_x + 0.5*vector_x^2 + rnorm(10,0,0.02)

datos <- data.frame(
 x= vector_x,
 y = y
)

modelo_ortogonal <- lm(y~poly(vector_x,2), data=datos)
modelo_crudo <- lm(y~poly(vector_x,2,raw = TRUE), data=Auto)

p <- poly(vector_x, 2)
print(p)
atributos_polinomio <- attributes(p)$coefs
alpha <- atributos_polinomio$alpha
norm2 <- atributos_polinomio$norm2

# xbar <- mean(vector_x)
# x <- x - xbar
# x <- outer(x, 0L:2L, "^")
# QR <- qr(x)
# 
# z <- QR$qr
# z <- z * (row(z) == col(z))
# Z <- qr.qy(QR, z)
# 
# norm2 <- colSums(Z^2)
# alpha <- (colSums(x*Z^2)/norm2 + xbar)[1L:2L]
# alpha[1] <- alpha[1] - 1
# norm2 <- c(1, norm2) # to use "common" code below

Z <- matrix(1, length(vector_x), 2 + 1L) # Z[,1] == 1
Z[, 2] <- vector_x - alpha[1L]
if(2 > 1)
  i=2
  for(i in 2:2)
    Z[, i+1] <- (vector_x - alpha[i]) * Z[, i]  -
  (norm2[i+1] / norm2[i]) * Z[, i-1]

Z <- Z / rep(sqrt(norm2[-1L]), each = length(vector_x))
colnames(Z) <- 0L:2L
Z <- Z[, -1, drop = FALSE]

coefs_ortogonales_mod <- modelo_ortogonal$coef
coefs_ortogonales_mod[1] + coefs_ortogonales_mod[2] * Z[, 1]  + coefs_ortogonales_mod[3] * Z[, 2]    

# Polinomio de grado 0 --> F_0(es decir el polinomio constante)
F_0 <- function (x) {
  a = 1 
  return(a)
}
# Polinomio de grado 1 --> F_1(es decir el término lineal)
F_1 <- function(x){
  ecuacion_termino_lineal = (x-alpha[1]) / sqrt(norm2[3])
  return(ecuacion_termino_lineal)
} 
# Polinomio de grado n --> F_2(es decir el término con x^n)
# Este polinomio se produce usando la siguiente recursividad:

#F_n(x) = [(x-alpha[n]) * sqrt(norm2[d+1]) * F_{n-1}(x) - norm2[n+1] / sqrt(norm2[n]) * F_{n-2}(x)] / sqrt(norm2[n+2])
#Entonces calculando para n= 2 tenemos:

F_2 <- function(x){
  numerador = (x - alpha[2]) *  sqrt(norm2[3]) * F_1(x)  - (norm2[3] / norm2[2]) * F_0(x)
  denominador = sqrt(norm2[4])
  return(numerador/denominador)
}
  
columna_F_0 <- F_0(vector_x)
print(columna_F_0)

columna_F_1 <- F_1(vector_x)
print(columna_F_1)

columna_F_2 <- F_2(vector_x)
print(columna_F_2)

matriz_modelo_recursiva <- data.frame(
  F_0 = 1,
  F_1 = columna_F_1,
  F_2 = columna_F_2
)

predicciones_ortogonales <- as.matrix(matriz_modelo_recursiva)%*%coefs_ortogonales_mod
coefs_crudos_mod<- modelo_crudo$coef
predicciones_crudas <- coefs_crudos_mod[1] + coefs_crudos_mod[2] * vector_x + coefs_crudos_mod[3] * vector_x^2

df_comparacion <- data.frame(
  predicciones_ortogonales = predicciones_ortogonales,
  predicciones_crudas = predicciones_crudas
)

df_comparacion
