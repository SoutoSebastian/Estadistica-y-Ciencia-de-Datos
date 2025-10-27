autos <- cars


y <- cars$dist
x <- cars$speed


X <- cbind(1, x)


beta <- solve(t(X) %*% X) %*% t(X) %*% y

predecidos <- X %*% beta


plot(x, y, main="Gráfico de Dispersión", xlab="speed", ylab="dist", pch=19, col="blue")
abline(a = beta[1], b = beta[2], col="green")
points(x, predecidos, col = "orange")


residuos <- y - predecidos

estVar <- 1/50 * t(residuos) %*% residuos


X2 <- cbind(X, x^2)
beta2 <- solve(t(X2) %*% X2) %*% t(X2) %*% y
y_pred_pol <- X2 %*% beta2


plot(x, y, main="Gráfico de Dispersión", xlab="speed", ylab="dist", pch=19, col="blue")
abline(a = beta[1], b = beta[2], col="green")
points(x, predecidos, col = "orange")
lines(x, y_pred_pol, col="red", lwd=2)

