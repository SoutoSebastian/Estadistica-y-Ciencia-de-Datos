#########Ej 5########

#a

n<- 16
sigma <- 20
mu0 <- 100

test<- function(prom){
  estadistico <- (prom - mu0)*sqrt(n)/sigma
  
  if (estadistico >= 1.645){
    return(1)
  }else{
    return(0)
  }
}



#b

# Parámetros


mu1 <- 110
alpha <- 0.05

# Desvío del promedio
sigma_xbar <- sigma / sqrt(n)

# Punto crítico
c <- mu0 + qnorm(1 - alpha) * sigma_xbar

# Eje de valores
x <- seq(85, 125, length = 1000)

# Densidades
f0 <- dnorm(x, mean = mu0, sd = sigma_xbar)
f1 <- dnorm(x, mean = mu1, sd = sigma_xbar)

# Gráfico base
plot(x, f0, type = "l", lwd = 2, col = "blue",
     ylab = "Densidad", xlab = expression(bar(X)[n]),
     main = "Distribuciones bajo H0 y H1")

lines(x, f1, lwd = 2, col = "red")

# Región crítica (nivel del test)
polygon(c(x[x > c], rev(x[x > c])),
        c(f0[x > c], rep(0, sum(x > c))),
        col = rgb(0, 0, 1, 0.3), border = NA)

# Potencia (bajo H1)
polygon(c(x[x > c], rev(x[x > c])),
        c(f1[x > c], rep(0, sum(x > c))),
        col = rgb(1, 0, 0, 0.3), border = NA)

# Línea vertical del punto crítico
abline(v = c, lty = 2)

# Leyenda
legend("topright",
       legend = c(expression(H[0]:~mu==100), expression(H[1]:~mu==110),
                  "Nivel del test", "Potencia"),
       col = c("blue", "red", rgb(0,0,1,0.3), rgb(1,0,0,0.3)),
       lwd = c(2,2,10,10), bty = "n")


#c

# Parámetros
mu0 <- 100
mu1 <- 110
sigma <- 20
alpha <- 0.05

# Caso n = 16
n1 <- 16
sd_xbar1 <- sigma / sqrt(n1)
c16 <- mu0 + qnorm(1 - alpha) * sd_xbar1

# Caso n = 36
n2 <- 36
sd_xbar2 <- sigma / sqrt(n2)
c36 <- mu0 + qnorm(1 - alpha) * sd_xbar2

# Eje x amplio que cubra ambas distribuciones
x <- seq(90, 115, length = 2000)

# Densidades
f0_n16 <- dnorm(x, mean = mu0, sd = sd_xbar1)  # H0, n=16
f1_n16 <- dnorm(x, mean = mu1, sd = sd_xbar1)  # H1, n=16
f0_n36 <- dnorm(x, mean = mu0, sd = sd_xbar2)  # H0, n=36
f1_n36 <- dnorm(x, mean = mu1, sd = sd_xbar2)  # H1, n=36

# Inicio del gráfico: densidad H0 n=16 (base)
plot(x, f0_n16, type = "l", lwd = 2, col = "blue",
     ylim = c(0, max(f0_n16, f1_n16, f0_n36, f1_n36)*1.05),
     xlab = expression(bar(X)[n]), ylab = "Densidad",
     main = "Densidades de " ~ bar(X)[n] ~ " bajo H0 y H1 para n=16 y n=36")

# Agregar las otras densidades
lines(x, f1_n16, lwd = 2, col = "red")
lines(x, f0_n36, lwd = 2, col = "darkblue", lty = 2)  # H0 n=36 (línea discontinua)
lines(x, f1_n36, lwd = 2, col = "darkred",  lty = 2)  # H1 n=36 (línea discontinua)

# Sombrear región de rechazo y potencia para n=16 (azul claro / rojo claro)
ix16 <- which(x > c16)
polygon(c(x[ix16], rev(x[ix16])),
        c(f0_n16[ix16], rep(0, length(ix16))),
        col = rgb(0,0,1,0.25), border = NA)  # nivel (H0, n=16)
polygon(c(x[ix16], rev(x[ix16])),
        c(f1_n16[ix16], rep(0, length(ix16))),
        col = rgb(1,0,0,0.15), border = NA)  # potencia (H1, n=16)

# Sombrear región de rechazo y potencia para n=36 (otro color semitransparente)
ix36 <- which(x > c36)
polygon(c(x[ix36], rev(x[ix36])),
        c(f0_n36[ix36], rep(0, length(ix36))),
        col = rgb(0,0.6,0.8,0.25), border = NA)   # nivel (H0, n=36)
polygon(c(x[ix36], rev(x[ix36])),
        c(f1_n36[ix36], rep(0, length(ix36))),
        col = rgb(1,0.6,0,0.20), border = NA)     # potencia (H1, n=36)

# Líneas verticales de puntos críticos
abline(v = c16, lty = 2, col = "blue")
abline(v = c36, lty = 2, col = "darkblue")

# Leyenda con valores numéricos
legend("topright",
       legend = c(
         paste0("H0, n=16 (pdf)"),
         paste0("H1, n=16 (pdf)"),
         paste0("H0, n=36 (pdf, dashed)"),
         paste0("H1, n=36 (pdf, dashed)"),
         paste0("c16 = ", round(c16,3)),
         paste0("c36 = ", round(c36,3)),
         paste0("Potencia n16 = ", round(1 - pnorm((c16 - mu1)/sd_xbar1),3)),
         paste0("Potencia n36 = ", round(1 - pnorm((c36 - mu1)/sd_xbar2),3))
       ),
       col = c("blue","red","darkblue","darkred","blue","darkblue","black","black"),
       lty = c(1,1,2,2,2,2,NA,NA), bty = "n", cex = 0.8)




######Ej 6########


#a 

valor_critico <- qt(0.92, 11)

grasa <- c(21, 18, 19, 16, 18, 24, 22, 19, 24, 14, 18, 15)

promedio <- mean(grasa)
s <- sd(grasa)

estadistico <- sqrt(12) *(promedio - 18)/s


######Ej 9######

#e

# Parámetros
n <- 50
p0 <- 0.3
alpha <- 0.05
z <- qnorm(1 - alpha/2)

# Vector de valores verdaderos de p
p <- seq(0.05, 0.95, by = 0.001)

# Funciones de potencia


pi_T <- 1 - pnorm(z - sqrt(n)*(p - p0)/sqrt(p*(1-p))) +
  pnorm(-z - sqrt(n)*(p - p0)/sqrt(p*(1-p)))

# Gráfico
plot(p, pi_W, type = "l", col = "blue", lwd = 2,
     ylim = c(0,1), xlab = "p verdadero", ylab = "Potencia aproximada",
     main = "Funciones de potencia aproximadas (n = 50, p0 = 0.3)")
lines(p, pi_T, col = "red", lwd = 2, lty = 2)
abline(v = p0, lty = 3)
legend("bottomright", legend = c("Test clásico (W)", "Test plug-in (T)"),
       col = c("blue", "red"), lwd = 2, lty = c(1,2))



#f


set.seed(123)  # reproducibilidad

n <- 50
p0 <- 0.3
alpha <- 0.05
z <- qnorm(1 - alpha/2)
p_vals <- c(0.03, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.97)
B <- 100000  # cantidad de simulaciones

# vectores donde guardamos las potencias
pi_W <- numeric(length(p_vals))
pi_T <- numeric(length(p_vals))

for (i in seq_along(p_vals)) {
  p <- p_vals[i]
  
  # simulamos muestras de tamaño n, repetidas B veces
  Xbar <- rbinom(B, n, p) / n
  
  # estadísticos
  W <- sqrt(n) * (Xbar - p0) / sqrt(p0*(1 - p0))
  T <- sqrt(n) * (Xbar - p0) / sqrt(Xbar*(1 - Xbar))
  
  # rechazos
  pi_W[i] <- mean(abs(W) > z)
  pi_T[i] <- mean(abs(T) > z)
}

# mostramos resultados
res <- data.frame(p = p_vals, potencia_W = pi_W, potencia_T = pi_T)
print(res)

# gráfico interpolado
plot(p_vals, pi_W, type = "o", pch = 16, col = "blue", ylim = c(0,1),
     xlab = "p verdadero", ylab = "Potencia estimada",
     main = "Funciones de potencia simuladas (n = 50, p0 = 0.3)")
lines(p_vals, pi_T, type = "o", pch = 17, col = "red", lty = 2)
abline(v = p0, lty = 3)
legend("bottomright", legend = c("Test clásico (W)", "Test plug-in (T)"),
       col = c("blue", "red"), lwd = 2, pch = c(16,17), lty = c(1,2))


######EJ 12#######

#a
set.seed(2025)

Nsim <- 200000
n1 <- 20
n2 <- 10
alpha <- 0.05
z <- qnorm(1 - alpha)   # percentil 0.95, umbral unilateral

rejects <- 0L

for (i in 1:Nsim) {
  x1 <- rnorm(n1, mean = 0, sd = 1)      # bajo H0: mu = 0
  T1 <- sqrt(n1) * mean(x1)
  if (T1 > z) {
    rejects <- rejects + 1
  } else {
    x2 <- rnorm(n2, mean = 0, sd = 1)
    x_all <- c(x1, x2)
    T30 <- sqrt(n1 + n2) * mean(x_all)
    if (T30 > z) rejects <- rejects + 1
  }
}

p_hat <- rejects / Nsim
cat("Rechazo empírico (nivel real):", p_hat, "\n")

#El nivel queda aproximandamente 0.07.


#b

Nsim <- 200000
n1 <- 20
n2 <- 10
alpha <- 0.05
z <- qnorm(1 - alpha)   # percentil 0.95, umbral unilateral
mus <- c(0.5, 1, 2, 5, 10)
potencias <- 0


for (j in 1:length(mus)){
  rejects <- 0L
  for (i in 1:Nsim) {
    x1 <- rnorm(n1, mean = mus[j], sd = 1)      # bajo H0: mu = 0
    T1 <- sqrt(n1) * mean(x1)
    if (T1 > z) {
      rejects <- rejects + 1
    } else {
      x2 <- rnorm(n2, mean = mus[j], sd = 1)
      x_all <- c(x1, x2)
      T30 <- sqrt(n1 + n2) * mean(x_all)
      if (T30 > z) rejects <- rejects + 1
    }
  }
  
  potencias[j] <- rejects / Nsim

}


########EJ 16#########

datos_x <- c(25, 19.5, 16.6, 21.3, 20.7, 16.8)
datos_y <- c(23.8, 19, 15.9, 20.4, 19.6, 15.8)

datosD <- datos_x - datos_y



library(ggplot2)

datos <- data.frame(val = datosD)

ggplot(datos, aes(sample = val)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ-Plot de datos_x")



D_raya <- mean(datosD)
S <- sd(datosD)


#######Cuestionario##########

datos <- c(24, 21, 27, 28, 23, 22, 36, 24, 33, 14, 25)

S2 <- var(datos)

U_obs <- 10*S2/49

p_val <- pchisq(U_obs,10)


sa <- 2.76
sb <- 1.92
xa <- 6.84
xb <- 7.81
na <- 55
nb <- 57

sp2 <- ((2.76)^2 * 54 + (1.92^2)*56) / (55+57-2)

Tobs <- abs(xa - xb)/(sqrt(sp2*(1/na + 1/nb)))
pval <- 2 * pt(Tobs, 110, lower.tail = FALSE)

limInf<- xa - xb - qt(0.005, 110, lower.tail = FALSE) * sqrt(sp2 *(1/na + 1/nb))
limSup<- xa - xb + qt(0.005, 110, lower.tail = FALSE) * sqrt(sp2 *(1/na + 1/nb)) 

