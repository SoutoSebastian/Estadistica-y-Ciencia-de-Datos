#####Ej4######

#a
datos <- read.csv("glakes.csv", header = TRUE)
datos$LT <- log(datos$Time)
datos$W <- datos$Tonnage^0.25

modelo <- lm(LT ~ W, data=datos)

summary(modelo)

#b
coeficientes <- coef(modelo)

#c
sigma2 <- summary(modelo)$sigma^2


#d
#t valor = 11.332 
#p valor = < 3.6e-12
#el p valor es menor que 0.01 entonces hay evidencia suficiente para rechazar

#e
coeficientes <- summary(modelo)$coefficients
theta_0 <- coeficientes[1, "Estimate"]  # Estimación de theta_0

X <- cbind(1, datos$W)
XtX_inv <- solve(t(X) %*% X)
d_00 <- XtX_inv[1,1]


estadistico <- (theta_0 - 10)/(sqrt(sigma2*(d_00)))

cov <- vcov(modelo)
estadistico2 <- (theta_0 - 10)/sqrt(cov[1,1])


pval <- pt(estadistico, nrow(datos)-2, lower.tail = FALSE)

#como pval = 1, no rechazo para ningun alpha.


#f

beta_0_hat <- coeficientes[1]
se_beta <- sqrt(diag(cov))
se_beta_0 <- se_beta[1]

alpha <- 0.05
df <- nrow(datos) - 2
tcrit <- qt(1 - alpha/2, df)
lower <- beta_0_hat - tcrit * se_beta_0
upper <- beta_0_hat + tcrit * se_beta_0

#Intervalo de Confianza:
c(estimate = beta_0_hat, lower = lower, upper = upper)

#R^2

R2 <- summary(modelo)$r.squared
#Aproximadamente el 81.6% de la variabilidad de y se explica por la relacion lineal con x en el 
#modelo

#Correlacion muestral:
LT_hat <- modelo$fitted.values
cor_LT_hat <- cor(datos$LT, LT_hat)
cor_LT_hat^2
#Son casi iguales, tiene sentido pues es la variabilidad que explica el modelo


#g

peso_0 <- 625
w_0 <- peso_0^0.25
newdata <- data.frame(W = w_0)
pred_media <- predict(modelo, newdata = newdata, interval = "confidence", level = 0.95)
pred_media

#Prediccion:
coef <- coef(modelo)
x_0 <- c(1,w_0)
pred_0 <- sum(x_0*coef) 
pred_0

#hardcodeado:
intervalo <- c(pred_0-tcrit*sqrt(t(x_0)%*%cov%*%x_0), pred_0+tcrit*sqrt(t(x_0)%*%cov%*%x_0)) 
intervalo



#h
pred_obs <- predict(modelo, newdata = newdata, interval = "prediction", level = 0.95)
pred_obs

#hardcodeado:
intervalo_pred <- c(pred_0-tcrit*sqrt(sigma2+t(x_0)%*%cov%*%x_0), pred_0+tcrit*sqrt(sigma2+t(x_0)%*%cov%*%x_0)) 
intervalo_pred

#i
intervalo_pred_tiempo <- exp(intervalo_pred)
intervalo_pred_tiempo



####Ej 5####


credit <- read.csv("credit.txt")

#a

modelo <- lm(Balance ~ Income + Rating + Limit + Cards + Age + Education + Gender + Student + Married, data = credit)
summary(modelo)

#b

segunda_obs <- credit[2,]

#prediccion 2:
pred2 <- predict(modelo, newdata = segunda_obs)

#residuo 2:
residuo <- segunda_obs$Balance - pred2

#Suma residuos:
sumaResiduos <- sum(resid(modelo))

#Correlaciones:
CorEd <- cor(credit$Education, resid(modelo))
CorLim <- cor(credit$Limit, resid(modelo))

#no hay relacion lineal entre educacion y residuos

#La correlacion lineal para cada una de las variables explicativas y los residuos vale 0, por 
#propiedad de modelo lineal.

#c

S2 <- summary(modelo)$sigma^2 #es la estimacion insesgada de S^2

#d

summary(modelo)

#Las mas relevantes parecen ser las que tienen el p-valor mas chico. Ya que este corresponde
#al test donde rechazamos que el parametro es igual a 0, si el p-valor es chico esto quiere
#decir que se rechaza que el parametro sea 0 para los niveles de confianza habituales.
#Income, Rating, Limit, Cards, StudentYes son las mas notables, pero las que no son relevantes
#son Married, Gender, Education.

#e

#Mirando la salida del summary el p valor para el primer test planteado es: 0.0310

#Para el otro test:
tvalue <- -2.164

pVal <- pt(tvalue, nrow(credit) - 10)

#o sino como es negativo, sabemos que el p valor del otro es 2*p-val

pVal_v2 <- 0.0310/2

#f
coeficientes <- coef(modelo)

cov <- vcov(modelo)
ed_6_hat <- coeficientes[7]
se_beta <- sqrt(diag(cov))
se_Ed <- se_beta[7]

alpha <- 0.1
df <- nrow(credit) - 10
tcrit <- qt(alpha/2, df, lower.tail = FALSE)
lower <- ed_6_hat - tcrit * se_Ed
upper <- ed_6_hat + tcrit * se_Ed


#g

#primero construyo el intervalo para B_age como siempre.

age_5_hat <- coeficientes[6]
se_beta <- sqrt(diag(cov))
se_age <- se_beta[6]

alpha <- 0.05
df <- nrow(credit) - 10
tcrit <- qt(alpha/2, df, lower.tail = FALSE)
lower <- age_5_hat - tcrit * se_age
upper <- age_5_hat + tcrit * se_age

#Y ahora el int para -3Bage:

int <- c(-3 * upper, -3*lower)


#####Ej 6#####
#Preguntar

datos <- read.csv("Debernardi.csv")


datos$logLYVE1 <- log(datos$LYVE1)
datos$diagnosis <- factor(datos$diagnosis)

modelo <- lm(logLYVE1 ~ diagnosis, data = datos)
summary(modelo)

#intercept es Media estimada de log(LYVE1) para el grupo 1
#diagnosis 2 es Diferencia entre grupo 2 y grupo 1
#diagnosis 3 es Diferencia entre grupo 3 y grupo 1

anova(modelo) #test de todas medias iguales, p valor muy chico -> rechazamos H0.

#Ahora para saber entre que grupos estan las diferencias:

TukeyHSD(aov(modelo), conf.level = 0.95)

"A nivel de significancia simultáneo del 5%, se observan diferencias significativas 
entre las medias del logaritmo de LYVE1 para los tres niveles de la variable diagnosis.
Se puede observar que ningun intervalo de confiaza contiene al 0."



####Ej 7#########

credit <- read.csv("credit.txt")

set.seed(123)   
n <- nrow(credit)


idx_train <- sample(1:n, size = round(2*n/3), replace = FALSE) #tomo index de datos al hacer de 1 a n con el tamaño pedido y sin repo

entrenamiento <- credit[idx_train, ]
testeo <- credit[-idx_train, ]

Ws <- rep(NA, 8)

#1er modelo (Inc + Cards+ Age)

modelo1 <- lm(Balance ~ Income + Cards + Age, data = entrenamiento) 
predTest1 <- predict(modelo1, newdata = testeo)
errores1 <- (testeo$Balance - predTest1)^2
W1 <- sum(errores1)
Ws[1] <- W1

#2do modelo (Inc + Cards)

modelo2 <- lm(Balance ~ Income + Cards , data = entrenamiento) 
predTest2 <- predict(modelo2, newdata = testeo)
errores2 <- (testeo$Balance - predTest2)^2
W2 <- sum(errores2)
Ws[2] <- W2

#3er modelo (Inc + Age)

modelo3 <- lm(Balance ~ Income + Age, data = entrenamiento) 
predTest3 <- predict(modelo3, newdata = testeo)
errores3 <- (testeo$Balance - predTest3)^2
W3 <- sum(errores3)
Ws[3] <- W3

#4to modelo (Age + Cards)

modelo4 <- lm(Balance ~ Cards + Age, data = entrenamiento) 
predTest4 <- predict(modelo4, newdata = testeo)
errores4 <- (testeo$Balance - predTest4)^2
W4 <- sum(errores4)
Ws[4] <- W4

#5to modelo (Inc)

modelo5 <- lm(Balance ~ Income, data = entrenamiento) 
predTest5 <- predict(modelo5, newdata = testeo)
errores5 <- (testeo$Balance - predTest5)^2
W5 <- sum(errores5)
Ws[5] <- W5

#6to modelo (Cards)

modelo6 <- lm(Balance ~ Cards, data = entrenamiento) 
predTest6 <- predict(modelo6, newdata = testeo)
errores6 <- (testeo$Balance - predTest6)^2
W6 <- sum(errores6)
Ws[6] <- W6

#7mo modelo(Age)

modelo7 <- lm(Balance ~ Age, data = entrenamiento) 
predTest7 <- predict(modelo7, newdata = testeo)
errores7 <- (testeo$Balance - predTest7)^2
W7 <- sum(errores7)
Ws[7] <- W7

#8vo modelo (cte)

modelo8 <- lm(Balance ~ 1, data = entrenamiento) 
predTest8 <- predict(modelo8, newdata = testeo)
errores8 <- (testeo$Balance - predTest8)^2
W8 <- sum(errores8)
Ws[8] <- W8

Ws

#El de menor W es el modelo 1 que usaba las 3 variables, por lo tanto este es el modelo elegido
#si nos basamos en el criterio de W.


########Ej 8########

glakes <- read.csv("glakes.csv")

#Agrego las columas necesarias para hacer todos los modelo.

glakes$LT <- log(glakes$Time)
glakes$W <- glakes$Tonnage^0.25
glakes$W2 <- glakes$Tonnage^0.5


modelo1 <- lm(Time ~ Tonnage -1, data = glakes)
modelo2 <- lm(Time ~ Tonnage, data = glakes)
modelo3 <- lm(LT ~ W, data = glakes)
modelo4 <- lm(LT~ W + W2, data=glakes)


errores1 <- numeric(nrow(glakes))
errores2 <- numeric(nrow(glakes))
errores3 <- numeric(nrow(glakes))
errores4 <- numeric(nrow(glakes))

for(i in 1:nrow(glakes)) {
  
  datos_train <- glakes[-i, ]
  datos_test <- glakes[i, , drop = FALSE]  
  
  modelo1 <- lm(Time ~ Tonnage -1, data = datos_train)
  modelo2 <- lm(Time ~ Tonnage, data = datos_train)
  modelo3 <- lm(LT ~ W, data = datos_train)
  modelo4 <- lm(LT~ W + W2, data=datos_train)
  
  prediccion1 <- predict(modelo1, newdata = datos_test)
  prediccion2 <- predict(modelo2, newdata = datos_test)
  prediccion3 <- exp(predict(modelo3, newdata = datos_test))
  prediccion4 <- exp(predict(modelo4, newdata = datos_test))
  
  error1 <- (prediccion1 - datos_test$Time)^2
  error2 <- (prediccion2 - datos_test$Time)^2
  error3 <- (prediccion3 - datos_test$Time)^2
  error4 <- (prediccion4 - datos_test$Time)^2
  
  errores1[i] <- error1
  errores2[i] <- error2
  errores3[i] <- error3
  errores4[i] <- error4
}

W1 <- sum(errores1)
W2 <- sum(errores2)
W3 <- sum(errores3)
W4 <- sum(errores4)

Ws <- c(W1, W2, W3, W4)

#El de menor W es el modelo 3 por lo tanto bajo este criterio elegiriamos ese modelo.


#####Ej 9#####


cemento <- read.table("cemento.txt", header = TRUE)

#a

matrizCorrelacion <- cor(cemento)

#Nos interesan las posiciones de la fila de Y que tienen valores cercanos a 1 en módulo.
#Las variables que parecen explicar la variabilidad de Y son x2,x3 y x4. x1 tiene una asociacion
#moderada.

#b

#modelo lineal:

modelo <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = cemento)

#estimador de minimos cuadrados:
coeficientes <- coef(modelo)

#tests:
summary(modelo)

#Observando los p-valores son todos grandes, es decir que para niveles habituales
#no rechazamos H0. No hay evidencia significativa como para rechazar que Bi = 0
#para todo i.

#Significancia de la regresion. (PREGUNTAR)

#Con el test ANOVA testeamos que todos sean iguales a 0 o que haya alguno distinta de 0.


anova(modelo)
#podemos observar que las variables x1, x2 y x3 son significantes?


#Contradiccion:
#En el primer test me daba que todos eran 0 y ahora que hay 3 significativas.


#c

suma_x <- rowSums(cemento[, c("x1","x2","x3","x4","x5")])
suma_x

#Todos los valores son cercanos a 100.
#Cada fila representa una muestra de cemento. Los 5 componentes están expresados 
#en porcentaje del peso total, por lo que la suma de las cinco variables suele 
#ser cercana a 100%.


#d

modeloSinIntercept <- lm(y ~ x1 + x2 + x3 + x4 + x5 - 1, data = cemento ) 
summary(modeloSinIntercept)

#Los coeficientes que son significativamente distintos de 0 son el de x2, x3 y x4.
  

#e

#Nuevo modelo

modeloNuevo <- lm(y ~ x2 + x3 + x4 -1, data = cemento)
coeficientesNuevos <- coef(modeloNuevo)


#f: Seleccion de modelos.


errores1 <- numeric(nrow(cemento))
errores2 <- numeric(nrow(cemento))
errores3 <- numeric(nrow(cemento))


for(i in 1:nrow(cemento)) {
  
  datos_train <- cemento[-i, ]
  datos_test <- cemento[i, , drop = FALSE]  
  
  modelo1 <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = datos_train)
  modelo2 <- lm(y ~ x1 + x2 + x3 + x4 + x5 - 1, data = datos_train ) 
  modelo3 <- lm(y ~ x2 + x3 + x4 -1, data = datos_train)
  
  
  prediccion1 <- predict(modelo1, newdata = datos_test)
  prediccion2 <- predict(modelo2, newdata = datos_test)
  prediccion3 <- predict(modelo3, newdata = datos_test)
  
  
  error1 <- (prediccion1 - datos_test$y)^2
  error2 <- (prediccion2 - datos_test$y)^2
  error3 <- (prediccion3 - datos_test$y)^2
  
  
  errores1[i] <- error1
  errores2[i] <- error2
  errores3[i] <- error3
  
}

W1 <- sum(errores1)
W2 <- sum(errores2)
W3 <- sum(errores3)


Ws <- c(W1, W2, W3)


#El modelo de menor W es el tercero como se esperaba, asi que me quedaria con este mismo.



#####Experimentos numericos#########

###Ej 10#######

#a
x <- rnorm(100)

#b

epsilon <- rnorm(100,0,sqrt(0.025))

#c

y <- -1 + 0.5 * x + epsilon

#La longitud del vector y es 100, loa verdaderos valores de B0 y B1 son -1 y 0.5 respectivamente.


#d

plot(x, y,
     xlab = "x",
     ylab = "y",
     main = "Scatterplot de x vs y",
     pch = 19)

#e

modelo <- lm(y ~ x)
summary(modelo)

#Betas estimados: -1.02410 y 0.50605, cercanos a los reales
#Son significativos, pues su p valor es mucho mas chico que 0.05.

S2 <- summary(modelo)$sigma^2
S2
#mientras que el valor real es 0.25


#f

plot(x, y, pch = 19, col = "gray40",
     main = "Recta estimada vs recta verdadera",
     xlab = "x", ylab = "y")

# Recta estimada (modelo)
abline(modelo, col = "blue", lwd = 2)

# Recta verdadera
abline(a = -1, b = 0.5, col = "red", lwd = 2, lty = 2)

legend("topleft",
       legend = c("Recta estimada", "Recta verdadera"),
       col = c("blue", "red"),
       lwd = 2,
       lty = c(1, 2))

#se observa que son muy parecidas.


#g

modeloPol <- lm(y ~ x + I(x^2))

summary(modeloPol)

#se puede observar que el p valor de B2 es 0.104 asi que a niveles de confianza habituales
#no rechazamos H0 es decir B2 = 0. Entonces se puede decir que el término cuadrático no mejora
#el ajuste.

plot(x, y, pch = 19, col = "gray60")   # scatterplot

# recta estimada
abline(modelo, col = "blue", lwd = 2)

# recta verdadera
abline(a = -1, b = 0.5, col = "red", lwd = 2, lty = 2)

# curva polinomial
x_grid <- seq(min(x), max(x), length.out = 200)
y_poli <- predict(modeloPol, newdata = data.frame(x = x_grid))
lines(x_grid, y_poli, col = "green", lwd = 2)


#g 

#(para var = 0.25)

epsilon <- rnorm(100,0,sqrt(0.25))
y <- -1 + 0.5 * x + epsilon

modelo <- lm(y ~ x)
summary(modelo)
S2 <- summary(modelo)$sigma^2


modeloPol <- lm(y ~ x + I(x^2))
summary(modeloPol)

plot(x, y, pch = 19, col = "gray60")   # scatterplot
abline(modelo, col = "blue", lwd = 2)
abline(a = -1, b = 0.5, col = "red", lwd = 2, lty = 2)
x_grid <- seq(min(x), max(x), length.out = 200)
y_poli <- predict(modeloPol, newdata = data.frame(x = x_grid))
lines(x_grid, y_poli, col = "green", lwd = 2)


#(para var = 2.5)

epsilon <- rnorm(100,0,sqrt(2.5))
y <- -1 + 0.5 * x + epsilon

modelo <- lm(y ~ x)
summary(modelo)
S2 <- summary(modelo)$sigma^2


modeloPol <- lm(y ~ x + I(x^2))
summary(modeloPol)

plot(x, y, pch = 19, col = "gray60")   # scatterplot
abline(modelo, col = "blue", lwd = 2)
abline(a = -1, b = 0.5, col = "red", lwd = 2, lty = 2)
x_grid <- seq(min(x), max(x), length.out = 200)
y_poli <- predict(modeloPol, newdata = data.frame(x = x_grid))
lines(x_grid, y_poli, col = "green", lwd = 2)

#A medida que aumentamos la varinza los modelos difieran más de la recta real.



#####Ej 11######

set.seed(123) 

#a

x1 <- runif(100,0,1)
x2 <- runif(100,0,1)
e <- rnorm(100, 0, 1)

y <- 2 + 2*x1 + 0.3*x2 + e

#Verdaderos coef: 2 , 2, 0.3


#b
datos <- data.frame(y, x1, x2)
pairs(datos, pch = 19, col = "blue")

#en el de y vs x1 se puede ver una dependencia lineal.


#c

modelo <- lm(y ~ x1 + x2)
summary(modelo)

coeficientes <- coef(modelo) #son parecidos

#Intercept y B1 significativos, mientras que B2 no. 

#Multiple R-squared:  0.2753, valor bajo el modelo no explica gran parte
#la variabiliad de los datos.
#F-statistic: 18.43 on 2 and 97 DF,  p-value: 1.65e-07
#p valor chico, entonces hay ev suf para rechazar H0 a nivel 0.05, existe beta != 0



#d
#sigma = 0.5
e <- rnorm(100, 0, 0.5)
y <- 2 + 2*x1 + 0.3*x2 + e


datos <- data.frame(y, x1, x2)
pairs(datos, pch = 19, col = "blue")


modelo <- lm(y ~ x1 + x2)
summary(modelo)
coeficientes <- coef(modelo)
#R^2 más grande (0.6661) y todo lo otro parecido.


#sigma = 0.25
e <- rnorm(100, 0, 0.25)
y <- 2 + 2*x1 + 0.3*x2 + e


datos <- data.frame(y, x1, x2)
pairs(datos, pch = 19, col = "blue")


modelo <- lm(y ~ x1 + x2)
summary(modelo)
coeficientes <- coef(modelo)
#R^2 más grande(0.8155) y ahora B2 es significativo.




########Ej 12########

#a
library(MASS)

n <- 50

mu <- c(1, 2)
Sigma <- matrix(c(1, 0.5,
                  0.5, 1), 
                nrow = 2, byrow = TRUE)

set.seed(123)   # para reproducibilidad
datos <- mvrnorm(n = n, mu = mu, Sigma = Sigma)

#No son independientes, esto es si y solo si la matriz de covarianza es diagonal.

#b

#Paso lo anterior a un data frama asi puedo manipularlo mas facil.

df <- as.data.frame(datos)
colnames(df) <- c("x1", "x2")

x1 <- df$x1
x2 <- df$x2

e <- rnorm(50)

y <- 1 + 2*x1 + 4*x2 + e 


#c

modelo <- lm(y ~ x1 + x2)
summary(modelo)
coeficientes_hat <- coef(modelo) #son cercanos


#d

V <- vcov(modelo) #sigma^2 (X^t X) ^-1
C <- cov2cor(V) #con esta linea pasamos de matriz de covarianzas a matriz de correlacion


#Entre B0 y B1 0.08 casi no estan correlacionados, B0 y B2 -0.8 muy correlacionados
#Entre B1 y B2 es aprox 0.5

#para que sean independientes las correlaciones deberian ser todas 0.


#e

Nrep <- 1000
estimaciones0 <- 0
estimaciones1 <- 0
estimaciones2 <- 0

for (i in 1:Nrep){
  n <- 50
  mu <- c(1, 2)
  Sigma <- matrix(c(1, 0.5,
                    0.5, 1), 
                  nrow = 2, byrow = TRUE)
  datos <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
  
  df <- as.data.frame(datos)
  colnames(df) <- c("x1", "x2")
  x1 <- df$x1
  x2 <- df$x2
  
  e <- rnorm(50)
  y <- 1 + 2*x1 + 4*x2 + e 
  modelo <- lm(y ~ x1 + x2)
  coeficientes <- coef(modelo)
  
  estimaciones0[i] <- coeficientes[1]
  estimaciones1[i] <- coeficientes[2]
  estimaciones2[i] <- coeficientes[3]
}

#f

coef_df <- data.frame(estimaciones0, estimaciones1, estimaciones2)
pairs(coef_df, pch = 19, col = "blue")


#B0 y B2 si parecen correlacionarse.


cov_b0_b1 <- cov(estimaciones0, estimaciones1)
cov_b0_b2 <- cov(estimaciones0, estimaciones2)
cov_b1_b2 <- cov(estimaciones1, estimaciones2)

sd_b0 <- sd(estimaciones0)
sd_b1 <- sd(estimaciones1)
sd_b2 <- sd(estimaciones2)

rho_b0_b1 <- cov_b0_b1 / (sd_b0 * sd_b1)
rho_b0_b2 <- cov_b0_b2 / (sd_b0 * sd_b2)
rho_b1_b2 <- cov_b1_b2 / (sd_b1 * sd_b2)

rho_b0_b1
rho_b0_b2
rho_b1_b2

#Coincide con lo que se ve en el gráfico.



#g


n <- 50

mu <- c(5, 2)
Sigma <- matrix(c(1, 0,
                  0, 1), 
                nrow = 2, byrow = TRUE)

set.seed(123)   
datos <- mvrnorm(n = n, mu = mu, Sigma = Sigma)


df <- as.data.frame(datos)
colnames(df) <- c("x1", "x2")

x1 <- df$x1
x2 <- df$x2

e <- rnorm(50)

y <- 1 + 2*x1 + 4*x2 + e 


modelo <- lm(y ~ x1 + x2)
summary(modelo)
coeficientes_hat <- coef(modelo) 


V <- vcov(modelo) 
C <- cov2cor(V) 



Nrep <- 1000
estimaciones0 <- 0
estimaciones1 <- 0
estimaciones2 <- 0

for (i in 1:Nrep){
  n <- 50
  mu <- c(5, 2)
  Sigma <- matrix(c(1, 0,
                    0, 1), 
                  nrow = 2, byrow = TRUE)
  datos <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
  
  df <- as.data.frame(datos)
  colnames(df) <- c("x1", "x2")
  x1 <- df$x1
  x2 <- df$x2
  
  e <- rnorm(50)
  y <- 1 + 2*x1 + 4*x2 + e 
  modelo <- lm(y ~ x1 + x2)
  coeficientes <- coef(modelo)
  
  estimaciones0[i] <- coeficientes[1]
  estimaciones1[i] <- coeficientes[2]
  estimaciones2[i] <- coeficientes[3]
}

#f

coef_df <- data.frame(estimaciones0, estimaciones1, estimaciones2)
pairs(coef_df, pch = 19, col = "blue")


cov_b0_b1 <- cov(estimaciones0, estimaciones1)
cov_b0_b2 <- cov(estimaciones0, estimaciones2)
cov_b1_b2 <- cov(estimaciones1, estimaciones2)

sd_b0 <- sd(estimaciones0)
sd_b1 <- sd(estimaciones1)
sd_b2 <- sd(estimaciones2)

rho_b0_b1 <- cov_b0_b1 / (sd_b0 * sd_b1)
rho_b0_b2 <- cov_b0_b2 / (sd_b0 * sd_b2)
rho_b1_b2 <- cov_b1_b2 / (sd_b1 * sd_b2)

rho_b0_b1
rho_b0_b2
rho_b1_b2


#Preguntar que sentido tiene que cambie la correlacion







