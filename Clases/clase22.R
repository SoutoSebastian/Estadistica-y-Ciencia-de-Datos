library(ISLR2)

auto <- data(Auto)
modelo <- lm()

modelo <- lm(mpg ~ horsepower, data = Auto)

errores1 <- numeric(nrow(Auto))
errores2 <- numeric(nrow(Auto))
errores3 <- numeric(nrow(Auto))
for(i in 1:nrow(Auto)) {
  
  datos_train <- Auto[-i, ]
  datos_test <- Auto[i, , drop = FALSE]  
  
  modelo1 <- lm(mpg ~ horsepower, data = datos_train)
  modelo2 <- lm(mpg ~ horsepower + I(horsepower^2), data = datos_train)
  
  prediccion1 <- predict(modelo1, newdata = datos_test)
  prediccion2 <- predict(modelo2, newdata = datos_test)

  error1 <- (prediccion1 - datos_test$mpg)^2
  error2 <- (prediccion2 - datos_test$mpg)^2
  
  errores1[i] <- error1
  errores2[i] <- error2
}

W1 <- sum(errores1)
W2 <- sum(errores2)
