set.seed(2025)

########Ej 3#############

datos <- read.table("datos1.txt", header = TRUE)

#Primero estimamos el valor central con la media y la mediana.

media <- mean(datos$x)
mediana <- median(datos$x)

#a. Generamos una muestra con la muestra original.

muestraboot <- sample(datos$x, length(datos$x), replace = TRUE)

#b. Calculamos la medida en la muestra

mebiaboot <- mean(muestraboot)
medianaboot <- median(muestraboot)

#c. Repetimos el proceso 1000 veces

mediasboot <- 0
medianasboot <- 0

for (i in 1:1000){
  muestrab <- sample(datos$x, length(datos$x), replace = TRUE)
  mediasboot[i] <- mean(muestrab)
  medianasboot[i] <- median(muestrab)
}

#d. Estimamos desvio standar

sdMedia <- sd(mediasboot)
sdMediana <- sd(medianasboot)


#nos queda que con la mediana el desvio estandar es mayor.

#####Ej 4########

#Tomamos casos fav / casos tot para aproximar. Deberia valer 1/6.


datos2 <- scan(what = numeric(), text = "2 2 4 6 1 3 1 3 2 4 4 4 4 4 6 3 3 4 1 2 1 6 3 2 3 4 1 1 5 4 1 4 6
4 1 2 1 5 4 3 3 1 3 1 6 5 1 3 2 3 6 2 4 2 6 6 5 2 4 4 1 4 3 1 2 1
6 1 1 3 1 6 6 1 2 6 1 1 4 5 4 1 5 2 2 1 6 6 1 2 1 3 1 3 3 4 3 3 3 5")

#1

muestraboot <- sample(datos2, length(datos2), replace = TRUE)

estTita <- mean(muestraboot == 5)


#2

titas.boot <- 0

for (i in 1:5000){
  muestrab <- sample(datos2, length(datos2), replace = TRUE)
  titas.boot[i] <- mean(muestrab == 5)
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

#Ninguno de los dos intervalos contiene al valor que deberia tener tita si el dado fuera
#equilibrado.



#####Ej 5########

library(Lock5Data)


data("MustangPrice")

#a. Realizo un plot y estimo la correlacion

plot(MustangPrice$Miles, MustangPrice$Price,
     main = "Precio vs. Kilometraje",
     xlab = "Millas recorridas",
     ylab = "Precio (USD)",
     pch = 19, col = "steelblue")


est_correlacion <- cor(MustangPrice$Miles, MustangPrice$Price)


#b. Puedo generar muestras bootstrap con el modelo no parametrico y estimar la correlacion
#repetir este proceso muchas veces.

#c.  llevamos a cabo lo del item b

corBoot <- 0

for (i in 1:5000){
  muestrab <- MustangPrice[sample(1:nrow(MustangPrice), nrow(MustangPrice), replace = TRUE), ]
  
  corBoot[i] <- cor(muestrab$Miles, muestrab$Price)
}




hist(corBoot,
     main = "estimaciones correlacion bootstrap",
     freq = FALSE,
     col = "lightblue"
)

#Tenemos que la gran mayoria de los valores estimados se encuentran entre el -1 y el -0.8
#esto quiere decir que hay una relacion decreciente entre precio y millas.


#d. Uso el metodo de cuantiles.

alfa <- 0.05

intervalo_correlacion <- quantile(corBoot, probs = c(alfa/2, 1-alfa/2))

hist(corBoot,
     main = "Bootstrap de la correlación",
     freq = FALSE,
     col = "lightblue",
     xlab = "Coeficiente de correlación")
abline(v = intervalo_correlacion, col = "red", lwd = 2, lty = 2)  # límites

#El intervalo obtenido esta muy cerca del -1 por lo tanto podemos decir que hay correlacion.



#######Ej 6#########

#No existe el archivo.. supongo que se llama datosP

#a. Hallamos est de la mediana para mujeres y para hombres.

# Mujeres
ingresosMujeres <- datosP[datosP$mujer == 1, "ingresototal.conruido"]
estMedianaM <- median(ingresosMujeres)

# Hombres
ingresosHombres <- datosP[datosP$mujer == 0, "ingresototal.conruido"]
estMedianaH <- median(ingresosHombres)

# Diferencia
diferenciaMediana <- estMedianaM - estMedianaH



#b. Hacemos muestras bootstrap y estimamos

muestrabMujeres <- sample(ingresosMujeres, length(ingresosMujeres), replace = TRUE)
estMedianaMb <- median(muestrabMujeres)

muestrabHombres <- sample(ingresosHombres, length(ingresosHombres), replace = TRUE)
estMedianaHb <- median(muestrabHombres)

difMedianaB <- estMedianaMb - estMedianaHb



#c. Repetimos 5000 veces.

diferenciasBoot <- 0

for (i in 1:5000){
  muestrabMujeres <- sample(ingresosMujeres, length(ingresosMujeres), replace = TRUE)
  estMedianaMb <- median(muestrabMujeres)
  
  muestrabHombres <- sample(ingresosHombres, length(ingresosHombres), replace = TRUE)
  estMedianaHb <- median(muestrabHombres)
  
  diferenciasBoot[i] <- estMedianaMb - estMedianaHb
}
  
  
hist(diferenciasBoot,
     main = "estimaciones diferencias bootstrap",
     freq = FALSE,
     col = "lightblue"
)  
  

sdDifMediana <- sd(diferenciasBoot)
  

#d. Intervalos de confianza.

#Metodo normal

cuantil <- qnorm(0.975, mean = 0, sd = 1)
termino <- cuantil * sdDifMediana

izq <- diferenciaMediana - termino
der <- diferenciaMediana + termino

intervalo_normal <- c(izq, der)

#Metodo cuantiles.

alfa <- 0.05

intervalo_difMediana <- quantile(diferenciasBoot, probs = c(alfa/2, 1 - alfa/2))


######Ej 7########

#a. Tomamaos la mediana de Price y despues hacemos bootstrap.

library(Lock5Data)


data("MustangPrice")

Price <- MustangPrice$Price

medianaPrice <- median(Price)


medianasBoot <- 0

for (i in 1:5000){
  muestraBoot <- sample(Price, length(Price), replace = TRUE)
  medianasBoot[i] <- median(muestraBoot)
}

#b. Realizamos un histograma.

hist(medianasBoot,
     main = "Bootstrap para la mediana de Price",
     freq = FALSE,
     breaks = 18,
     col = "lightblue",
     xlab = "Mediana de Price")


#No se puede construir un intervalo normal, pues no hay forma acampanada.

#Método de percentiles: asume que la distribución bootstrap refleja bien 
#la distribución de la estadística, pero si hay saltos o valores repetidos,
#el intervalo puede ser inexacto o poco confiable.


#c. Analizamos para la media.


meanPrice <- mean(Price)


meansBoot <- 0

for (i in 1:5000){
  muestraBoot <- sample(Price, length(Price), replace = TRUE)
  meansBoot[i] <- mean(muestraBoot)
}

hist(meansBoot,
     main = "Bootstrap para la media de Price",
     freq = FALSE,
     breaks = 18,
     col = "lightblue",
     xlab = "Media de Price")



#Vemos que tiene forma acampanada y parece suave, entonces parece apropiado usar bootstrap.



######Ej 8#########


#a. Defino funciones para calcular el intervalo de confianza de la mediana.

boot.metodo1 <- function(nivel, B, datos){
  mediana <- median(datos)
  
  medianasBoot <- 0
  
  for(i in 1:B){
    muestrab <- sample(datos, length(datos), replace = TRUE)
    medianasBoot[i] <- median(muestrab)
  }
  
  sdMediana <- sd(medianasBoot)
  
  cuantil <- qnorm(1 - (1-nivel)/2, mean = 0, sd = 1)
  termino <- cuantil * sdMediana
  
  izq <- mediana - termino
  der <- mediana + termino
  
   return (c(izq, der))
}
  
  
boot.metodo2 <- function(nivel, B, datos){
  alfa <- 1 - nivel
  medianasBoot <- 0
  
  for(i in 1:B){
    muestrab <- sample(datos, length(datos), replace = TRUE)
    medianasBoot[i] <- median(muestrab)
  }
  
  intervalo <- quantile(medianasBoot, probs = c(alfa/2, 1- alfa/2))
  return (intervalo)
}
  
  
#b. Ponemos aprueba el metodo 1.

datos <- rnorm(30, 0, 1)
nivel <- 0.95
B <- 2000 
  
intervalo_1 <- boot.metodo1(nivel, B, datos)
  
  
#c. Lo repito 1000 veces.

intervalos <- vector("list", 1000)
cubrimiento <- 0
longitud <- 0

for (i in 1:1000){
  int <- boot.metodo1(nivel,B,datos)
  
  intervalos[[i]] <- int
  cubrimiento[i] <- int[1] <= 0 && 0 <= int[2]
  longitud[i] <- int[2] - int[1]
}

mean(longitud)
mean(cubrimiento)

#Alternativa usando replicate:


# --- Ejecutar nIter iteraciones usando replicate ---
intervalos <- replicate(1000, boot.metodo1(nivel, B, datos), simplify = FALSE)

# --- Calcular cobertura (si el valor 0 está dentro del intervalo) ---
cubrimiento <- sapply(intervalos, function(int) int[1] <= 0 && 0 <= int[2])

# --- Calcular longitud de cada intervalo ---
longitud <- sapply(intervalos, function(int) int[2] - int[1])

# --- Resultados ---
proporcion_cubrimiento <- mean(cubrimiento)
longitud_promedio <- mean(longitud)


#d. Lo hago para distintos ns:


ns <- c(30,50, 100, 1000)

tabla_info <- data.frame(
  n = numeric(),
  cubrimiento = numeric(),
  longitud = numeric()
)

for (n in ns) {
  intervalos <- replicate(1000, {
    datos <- rnorm(n, 0, 1)  # NUEVA MUESTRA CADA VEZ
    boot.metodo1(nivel, B, datos)
  }, simplify = FALSE)
  
  cubrimiento <- sapply(intervalos, function(int) int[1] <= 0 && 0 <= int[2])
  
  longitud <- sapply(intervalos, function(int) int[2] - int[1])
  
  proporcion_cubrimiento <- mean(cubrimiento)
  longitud_promedio <- mean(longitud)
  
  tabla_info <- rbind(tabla_info,
                      data.frame(n = n,
                                 cubrimiento = proporcion_cubrimiento,
                                 longitud = longitud_promedio))
}


#d. Para el metodo 2.


tabla_info2 <- data.frame(
  n = numeric(),
  cubrimiento = numeric(),
  longitud = numeric()
)

for (n in ns) {
  intervalos <- replicate(1000, {
    datos <- rnorm(n, 0, 1)  # NUEVA MUESTRA CADA VEZ
    boot.metodo2(nivel, B, datos)
  }, simplify = FALSE)
  
  cubrimiento <- sapply(intervalos, function(int) int[1] <= 0 && 0 <= int[2])
  
  longitud <- sapply(intervalos, function(int) int[2] - int[1])
  
  proporcion_cubrimiento <- mean(cubrimiento)
  longitud_promedio <- mean(longitud)
  
  tabla_info2 <- rbind(tabla_info2,
                      data.frame(n = n,
                                 cubrimiento = proporcion_cubrimiento,
                                 longitud = longitud_promedio))
}

