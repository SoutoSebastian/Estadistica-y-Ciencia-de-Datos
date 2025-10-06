######Ej 1#########

set.seed(10)

datos <- c(500, 488, 426, 510, 450, 368, 508, 514, 426, 476, 512, 526, 444, 524, 236)

est1 <- function(x){
  return (mean(x))
}

est2 <- function(x) {
  x2 <- x^2
  Xn2 <- mean(x2)
  num <- -1 + sqrt(1 + 4 * Xn2)
  lambda2_hat <- num / 2
  return(lambda2_hat)
}

#1

muestrab <- rpois(15, est1(datos))
est1b <- est1(muestrab)

#2

estimadoresB <- 0

for (i in 1:1000){
  muestrab <- rpois(15, est1(datos))
  est1b <- est1(muestrab)
  estimadoresB[i] <- est1b
}


#3

hist(estimadoresB,
     main= "Estimadores bootstrap",
     breaks = 20)


#4

estvar <- var(estimadoresB)


#Ahora para el estimador 2.


#1

est2b <- est2(muestrab)

#2

estimadores2B <- 0

for (i in 1:1000){
  muestrab <- rpois(15, est2(datos))
  est2b <- est2(muestrab)
  estimadores2B[i] <- est2b
}


#3

hist(estimadores2B,
     main= "Estimadores bootstrap",
     breaks = 18)


#4

est2var <- var(estimadores2B)



#tiene mayor varianza que el 1. Y son muy parecidos, el 2 un poco para la derecha


#Ahora para el modelo no parametrico.

#1

muestrab <- sample(datos, length(datos), replace = TRUE)
est1b <- est1(muestrab)

#2

estimadoresB <- 0

for (i in 1:1000){
  muestrab <- sample(datos, length(datos), replace = TRUE)
  est1b <- est1(muestrab)
  estimadoresB[i] <- est1b
}


#3

hist(estimadoresB,
     main= "Estimadores bootstrap",
     breaks = 18)


#4

estvar <- var(estimadoresB)



#########Ej 2############


datos2 <- scan(what = numeric(), text = "2 2 4 6 1 3 1 3 2 4 4 4 4 4 6 3 3 4 1 2 1 6 3 2 3 4 1 1 5 4 1 4 6
4 1 2 1 5 4 3 3 1 3 1 6 5 1 3 2 3 6 2 4 2 6 6 5 2 4 4 1 4 3 1 2 1
6 1 1 3 1 6 6 1 2 6 1 1 4 5 4 1 5 2 2 1 6 6 1 2 1 3 1 3 3 4 3 3 3 5")

#1

muestraboot <- sample(datos2, length(datos2), replace = TRUE)

estTita <- mean(muestraboot %% 2 == 0)


#2

titas.boot <- 0

for (i in 1:5000){
  muestrab <- sample(datos2, length(datos2), replace = TRUE)
  titas.boot[i] <- mean(muestrab %% 2 == 0)
}


#3

hist(titas.boot,
     main = "estimaciones bootstrap",
     freq = FALSE,
     col = "lightblue"
     )


#tiene forma acampanada



#4


se_est <- sd(titas.boot)
promedio <- mean(titas.boot)


#5 INTERVALOS DE CONFIANZA


alfa <- 0.05

#Metodo Normal:

cuantil <- qnorm(0.975, mean = 0, sd = 1)
termino <- cuantil * se_est

izq <- estTita - termino
der <- estTita + termino

intervalo_normal <- c(izq, der)


#Metodo Cuantiles:

intervalo_cuantil <- quantile(titas.boot, probs = c(alfa/2, 1-alfa/2))




