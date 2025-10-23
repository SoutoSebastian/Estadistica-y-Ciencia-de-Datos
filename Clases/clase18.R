datosA <- read.csv("datosA.csv")
datosB <- read.csv("datosB.csv")

A <- datosA$peso_gr
B <- datosB$peso_gr

#item a

alpha <- 0.05

#rechazo si S1**2 sobre S2**2 menor a ...

n1 <- 20
n2 <- 15

cuantilIzq <- qf(alpha/2, n1 -1, n2-1)
cuantilDer <- qf(1 - alpha/2, n1-1, n2-1)

intAceptacion <- c(cuantilIzq, cuantilDer)

Tobs <- var(A) / var(B) 

p_value <- 2 * min(pf(Tobs, df1 = n1 - 1, df2 = n2 - 1), 1 - pf(Tobs, df1 = n1 - 1, df2 = n2 - 1))

#ACEPTO!!!


#item b

intAceptacion <- c(cuantilIzq, cuantilDer)


#item c

alpha <- 0.01

ARaya <- mean(A)
BRaya <- mean(B)


sP <- sqrt(((n1 - 1) * var(A) + (n2 - 1) * var(B)) / (n1 + n2 - 2))


cuantil <- pt(1-alpha/2, n1+n2-2)


TcObs <- abs(ARaya - BRaya) / (sP * sqrt(1 / n1 + 1 / n2))

# Decisión del test
if (TcObs > cuantil) {
  cat("Rechazamos H0: Las medias son diferentes.\n")
} else {
  cat("No rechazamos H0: No hay evidencia suficiente para decir que las medias son diferentes.\n")
}

# Calcular p-valor
p_value <- 2 * (1 - pt(TcObs, df = n1 + n2 - 2))
cat("p-valor:", p_value, "\n")

