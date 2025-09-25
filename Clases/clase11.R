#Ej 1

#generacion de datos:

Nrep <- 1000
ns <- c(6, 20, 50, 100, 1000)

intervalos <- list()  # Inicializar como lista
cubre <- list()

alfa <- 0.05

e_real <- 0.1

for (n in ns) {
  w <- runif(n, 1.3, 1.5)
  u <- runif(n, 0, 0.2)
  
  datosA <- w + 0.5 * u
  datosB <- w - 0.5 * u
  
  dif <- datosA - datosB
  
  prom <- mean(dif)
  S <- sd(dif)
  
  cuantil <- qnorm(1 - alfa / 2, 0, 1)
  
  termino <- cuantil * (S / sqrt(n))
  
  izq <- prom - termino
  der <- prom + termino
  
  intervalo <- c(izq, der)
  
  intervalos[[paste0("n=", n)]] <- intervalo
  cubre[[paste0("n=", n)]] <- izq <= 0.1 && 0.1 <= der
}

#hay que hacerlo varias veces y sacar la proporcion. 


ns <- c(6, 20, 50, 100, 1000)
alfa <- 0.05
num_simulaciones <- 1000
esperanza_real <- 0.1

proporciones_cubrimiento <- numeric(length(ns))  # Aquí guardaremos resultados

for (i in seq_along(ns)) {
  n <- ns[i]
  cubre <- 0  # Contador de intervalos que cubren la esperanza
  
  for (sim in 1:num_simulaciones) {
    w <- runif(n, 1.3, 1.5)
    u <- runif(n, 0, 0.2)
    
    datosA <- w + 0.5 * u
    datosB <- w - 0.5 * u
    
    dif <- datosA - datosB  # Esto es igual a u
    
    prom <- mean(dif)
    S <- sd(dif)
    
    cuantil <- qnorm(1 - alfa / 2)
    
    termino <- cuantil * (S / sqrt(n))
    
    izq <- prom - termino
    der <- prom + termino
    
    # ¿Cubre el intervalo a la esperanza real?
    if (izq <= esperanza_real && esperanza_real <= der) {
      cubre <- cubre + 1
    }
  }
  
  # Guardar proporción de cobertura
  proporciones_cubrimiento[i] <- cubre / num_simulaciones
}

# Mostrar resultados
resultado <- data.frame(
  n = ns,
  ProporcionCobertura = proporciones_cubrimiento
)

print(resultado)

