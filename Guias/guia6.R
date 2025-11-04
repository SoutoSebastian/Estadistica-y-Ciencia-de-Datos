#Ej4

#a
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
#t valor = 11.332 
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


pval <- pt(estadistico, nrow(datos)-2, lower.tail = FALSE)

#como pval = 1, no rechazo para ningun alpha.


#f

beta_0_hat <- coeficientes[1]
se_beta <- sqrt(diag(cov))
se_beta_0 <- se_beta[1]

alpha <- 0.05
df <- nrow(datos) - 2
tcrit <- qt(1 - alpha/2, df)
lower <- beta_0_hat - tcrit * se_beta_0
upper <- beta_0_hat + tcrit * se_beta_0

#Intervalo de Confianza:
c(estimate = beta_0_hat, lower = lower, upper = upper)

#R^2

R2 <- summary(modelo)$r.squared
#Aproximadamente el 81.6% de la variabilidad de y se explica por la relacion lineal con x en el 
#modelo

#Correlacion muestral:
LT_hat <- modelo$fitted.values
cor_LT_hat <- cor(datos$LT, LT_hat)
cor_LT_hat^2
#Son casi iguales, tiene sentido pues es la variabilidad que explica el modelo


#g

peso_0 <- 625
w_0 <- peso_0^0.25
newdata <- data.frame(W = w_0)
pred_media <- predict(modelo, newdata = newdata, interval = "confidence", level = 0.95)
pred_media

#Prediccion:
coef <- coef(modelo)
x_0 <- c(1,w_0)
pred_0 <- sum(x_0*coef) 
pred_0

#hardcodeado:
intervalo <- c(pred_0-tcrit*sqrt(t(x_0)%*%cov%*%x_0), pred_0+tcrit*sqrt(t(x_0)%*%cov%*%x_0)) 
intervalo



#h
pred_obs <- predict(modelo, newdata = newdata, interval = "prediction", level = 0.95)
pred_obs

#hardcodeado:
intervalo_pred <- c(pred_0-tcrit*sqrt(sigma2+t(x_0)%*%cov%*%x_0), pred_0+tcrit*sqrt(sigma2+t(x_0)%*%cov%*%x_0)) 
intervalo_pred

#i
intervalo_pred_tiempo <- exp(intervalo_pred)
intervalo_pred_tiempo



