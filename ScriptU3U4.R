#4 Julio 2023 - Regresion polinomial simple, solo tienes una X
# 19*b0+138*b1+1335*b2=649.5
# 138*b0+1335*b1+14935.5*b2=5306.6
# 1335*b0+14935.5*b1+181427.25*b2=51666.75

?solve

a <- matrix(c(19,138,1335,
              138, 1335, 14935.5,
              1335, 14935.5, 181427), nrow = 3)

b <- matrix(c(649.5,5306.6, 51666.75), nrow = 3)

solve(a,b)

#[1,] -6.6755591
#[2,] 11.7644938
#[3,] -0.6345802

# y = -6.6755591 + 11.7644938(*x) - 0.6345802 * (x^2)

# Linear model lm modelo de REGRESION LINEAL
#formula (dep izq, ind der), datos
setwd("A:\\agl2")
datos <- read.csv("resistencia.csv")
# y = b0 + b1*x
# y = resistencia, x = concentracion
#Esto calcula la prediccion de concentracion dependiendo de la resistencia del materia

modelo <- lm(resistencia ~ concentracion,
              data = datos)

datos2 <- read.csv("marketing.csv")
datos2 <- datos2[1:12,1:3]
modelo2 <- lm(Sales ~ Spend + Month, data = datos2)

#-----------------------------------------------------------------------------------
#Modelo de REGRESION POLINOMIAL LINEAL
datos <- read.csv("resistencia.csv")

modeloCuadrado <- lm(resistencia ~ concentracion
                      +I(concentracion^2),
             data = datos)

modeloCubico <- lm(resistencia ~ concentracion
             +I(concentracion^2)+ I(concentracion^3),
             data = datos)
plot(x=datos$concentracion, y=datos2$resistencia)

prediccion <- predict(modeloCuadrado, data.frame(concentracion = c(16,17)))






