#####Ej4#######


#a

datos <- c(1.52, 1.65, 1.72, 1.65, 1.72, 1.83, 1.62, 1.75, 1.72, 1.68,
           1.51, 1.65, 1.58, 1.65, 1.61, 1.70, 1.60, 1.73, 1.61, 1.52,
           1.81, 1.72, 1.50, 1.82, 1.65)

n <- length(datos)

prom <- mean(datos)

S <- sd(datos)

cuantil <- qt(1-0.025, n-1)
termino <- cuantil * S/sqrt(n)
intervalo <- c(prom -termino, prom + termino)

#b

cuantil1 <- qchisq(0.05, 24)
cuantil2 <- qchisq(1 - 0.05, 24)


izq <- (24*S^2)/cuantil2
der <- (24*S^2)/cuantil1

intervalob = c(izq,der)

#c



intervaloc <- exp(-intervalo)


########EJ5###########


#d

datos5 <- c(25, 45, 50, 61, 39, 40, 45, 47, 38, 39,
           54, 60, 39, 46, 39, 50, 42, 50, 62, 50)

cuantilInf <- qchisq(0.005,40)
cuantilSup <- qchisq(1-0.005,40)

izq <- cuantilInf/(2*sum(datos5))
der <- cuantilSup/(2*sum(datos5))

intervaloc <- c(izq, der)


######EJ 11####

n
p

ma <- rbinom(n, 1, p)


intervalo1 <- function(nivel, datos){
  cuantil <- qnorm(1 - (1-nivel)/2)
  prom <- mean (datos)
  n <- length(datos)
  
  termino <- cuantil * sqrt((prom*(1-prom)/n))
  izq = prom - termino
  der = prom + termino
  
  return(c(izq,der))
  }




