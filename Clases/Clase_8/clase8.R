intervalo <- function(datos, sigma, nivel){
  z <- qnorm(1 - (1 - nivel) / 2, lower.tail = TRUE)
  prom <- mean(datos)
  n <- length(datos)
  izq <- prom - z * sqrt(sigma/n) 
  der <- prom + z * sqrt(sigma/n)
  
  return (c(izq,der))
}


#item a

datos_normales <- rnorm(5,4,3)

#item b 

int_datos <- intervalo(datos_normales, 9, 0.95)

#si pertenece.


#item c

library(ggplot2)

Nrep = 1000
mu <- 4
set.seed(42)
sigma2 <- 9

# Guardamos los intervalos y si cubren
intervalos <- data.frame(
  simulacion = 1:Nrep,
  inf = numeric(Nrep),
  sup = numeric(Nrep),
  cubre = logical(Nrep)
)
for (i in 1:Nrep) {
  muestra <- rnorm(n, mean = mu, sd = sqrt(sigma2))
  ic <- intervalo(muestra, sigma2, nivel = 0.95)
  intervalos$inf[i] <- ic[1]
  intervalos$sup[i] <- ic[2]
  intervalos$cubre[i] <- mu >= ic[1] && mu <= ic[2]
}
# Tomamos una muestra para visualizar (e.g., 100 simulaciones)
intervalos_vis <- intervalos[1:100, ]
intervalos_vis$simulacion <- factor(intervalos_vis$simulacion, levels =
                                      rev(intervalos_vis$simulacion))
ggplot(intervalos_vis, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth =1) +
  geom_vline(xintercept = mu, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("red", "blue"), labels = c("No cubre", "Cubre")) +
  labs(
    title = "IC para mu",
    x = "Valor",
    y = "Simulacion",
    color = "Cubre mu?"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())


#item d


intervalo_s2 <- function(datos, nivel){
  n <- length(datos)
  t_value <- qt(1 - (1 - nivel) / 2, df = n - 1)
  
  s <- sd(datos)
  
  prom <- mean(datos)
  
  error_estandar <- s / sqrt(n)
  
  izq <- prom - t_value * error_estandar
  der <- prom + t_value * error_estandar
  
  # Devolver el intervalo
  return(c(izq, der))
}


# Parámetros
Nrep <- 1000  # Número de simulaciones
mu <- 4       # Media poblacional
sigma2 <- 9   # Varianza poblacional
n <- 30       # Tamaño de la muestra
set.seed(42)  # Semilla para reproducibilidad

# Inicializamos un data frame para guardar los resultados de los intervalos
intervalos <- data.frame(
  simulacion = 1:Nrep,
  inf = numeric(Nrep),
  sup = numeric(Nrep),
  cubre = logical(Nrep)
)

# Simulación de intervalos de confianza
for (i in 1:Nrep) {
  # Tomamos una muestra aleatoria de la distribución normal
  muestra <- rnorm(n, mean = mu, sd = sqrt(sigma2))
  
  # Calculamos el intervalo de confianza usando la función intervalo_s2
  ic <- intervalo_s2(muestra, nivel = 0.95)
  
  # Guardamos los límites del intervalo y si cubre la media mu
  intervalos$inf[i] <- ic[1]
  intervalos$sup[i] <- ic[2]
  intervalos$cubre[i] <- mu >= ic[1] && mu <= ic[2]
}

# Tomamos una muestra para visualizar (e.g., 100 simulaciones)
intervalos_vis <- intervalos[1:100, ]
intervalos_vis$simulacion <- factor(intervalos_vis$simulacion, levels = rev(intervalos_vis$simulacion))

# Visualizamos los intervalos de confianza con ggplot2
library(ggplot2)

ggplot(intervalos_vis, aes(y = simulacion, x = inf, xend = sup, color = cubre)) +
  geom_segment(aes(x = inf, xend = sup, yend = simulacion), linewidth = 1) +
  geom_vline(xintercept = mu, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("red", "blue"), labels = c("No cubre", "Cubre")) +
  labs(
    title = "Intervalos de confianza para la media (μ)",
    x = "Valor",
    y = "Simulación",
    color = "Cubre μ?"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank())




