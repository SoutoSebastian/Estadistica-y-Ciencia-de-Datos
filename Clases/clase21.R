#Ej4

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
#t valor = 6.105 
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


pval <- pt(estadistico, 31-2, lower.tail = FALSE)

#como pval = 1, no rechazo para ningun alpha.


#f





