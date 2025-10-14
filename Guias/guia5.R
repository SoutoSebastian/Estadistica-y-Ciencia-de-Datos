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


