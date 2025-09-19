#####Ej1


#a

datos <- c(1.52, 1.65, 1.72, 1.65, 1.72, 1.83, 1.62, 1.75, 1.72, 1.68,
            1.51, 1.65, 1.58, 1.65, 1.61, 1.70, 1.60, 1.73, 1.61, 1.52,
            1.81, 1.72, 1.50, 1.82, 1.65)

n <- length(datos)

prom <- mean(datos)

S <- sd(datos)

cuantil <- qt(1-0.25, n-1)
termino <- cuantil * S/sqrt(n)
intervalo <- c(prom -termino, prom + termino)

#b

cuantil1 <- qchisq(0.05, 24)
cuantil2 <- qchisq(1 - 0.05, 24)


izq <- (24*S^2)/cuantil2
der <- (24*S^2)/cuantil1

intervalob = c(izq,der)

#c



