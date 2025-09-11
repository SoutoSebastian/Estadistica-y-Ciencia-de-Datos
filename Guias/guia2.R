####GUIA 2####

library(ggplot2)
library(tidyr)
library(dplyr)

####Ejercicio 7####

X <- c(
  1.53, 1.65, 1.72, 1.83, 1.62,
  1.75, 1.72, 1.68, 1.65, 1.61,
  1.70, 1.60, 1.73, 1.61, 1.52,
  1.81, 1.72, 1.50, 1.51, 1.65,
  1.58, 1.82, 1.65, 1.72, 1.65
)


#i

mu <- mean(X)
sigma2 <- (sum((X - mu)^2))/length(X) 

#ii

p_tipico <- 1 - pnorm( (mu - 1.73)/sqrt(sigma2), mean = 0, sd = 1)

p_tipicoMGM <- 1 - (sum(X > 1.73))/length(X)


####Ejercicio 9####


#ITEM B

# Datos del servidor A
A <- c(1.49, 1.12, 1.08, 0.65, 0.98, 0.86, 0.37, 0.41, 0.91, 0.85,
       0.25, 0.78, 0.30, 1.41, 0.18, 0.52, 0.40, 0.69, 0.73, 0.81)

# Datos del servidor B
B <- c(1.32, 0.59, 1.41, 1.15, 0.34, 1.16, 1.29, 1.25, 1.18, 0.27,
       1.31, 0.96, 0.57, 0.66, 0.67, 1.34, 0.47, 1.07, 1.13, 0.25)


#estimacion de tita 

titaA <- sqrt((2/mean(A)))
espA <- 2/(titaA)^2



#####Ejercicio 14####

#a

set.seed(123)
datos <- data.frame(x= rexp(100, rate = 1))


ggplot(datos, aes(x = x)) +
  stat_ecdf(geom = "step", color = "blue", size = 1.2) +
  stat_function(fun = pexp, args = list(rate = 1), 
                color = "red", size = 1.2) +
  labs(title = "Función de distribución empírica vs teórica",
       x = "x", y = "F(x)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,1))


#b

#Usando ej 1 y 5.

#Metodo de Momentos:

lambdaM <- 1/mean(datos$x)

#Máxima verosimilitud: es igual al de momentos.


#c

#propuesta: F(x) = 1 - e^-(estimador de lambda)*x, plug-in o prop de invarianza.

ggplot(datos, aes(x = x)) +
  stat_ecdf(geom = "step", color = "blue", size = 1.2) +
  stat_function(fun = pexp, args = list(rate = 1), 
                color = "red", size = 1.2) +
  stat_function(fun = function(x) pexp(x, rate = lambdaM),
                color = "darkgreen", linetype = "dashed", size = 1.2) +
  labs(title = "Función de distribución empírica vs teórica",
       x = "x", y = "F(x)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,1))

#Ya no es mas escalonada, es suave y parece aproximar mejor a la distribucion teórica.


#d

#####n=10

set.seed(123)
datos <- data.frame(x= rexp(10, rate = 1))


ggplot(datos, aes(x = x)) +
  stat_ecdf(geom = "step", color = "blue", size = 1.2) +
  stat_function(fun = pexp, args = list(rate = 1), 
                color = "red", size = 1.2) +
  labs(title = "Función de distribución empírica vs teórica",
       x = "x", y = "F(x)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,1))


lambdaM <- 1/mean(datos$x)


ggplot(datos, aes(x = x)) +
  stat_ecdf(geom = "step", color = "blue", size = 1.2) +
  stat_function(fun = pexp, args = list(rate = 1), 
                color = "red", size = 1.2) +
  stat_function(fun = function(x) pexp(x, rate = lambdaM),
                color = "darkgreen", linetype = "dashed", size = 1.2) +
  labs(title = "Función de distribución empírica vs teórica",
       x = "x", y = "F(x)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,1))


#####n=1000


set.seed(123)
datos <- data.frame(x= rexp(1000, rate = 1))


ggplot(datos, aes(x = x)) +
  stat_ecdf(geom = "step", color = "blue", size = 1.2) +
  stat_function(fun = pexp, args = list(rate = 1), 
                color = "red", size = 1.2) +
  labs(title = "Función de distribución empírica vs teórica",
       x = "x", y = "F(x)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,1))



lambdaM <- 1/mean(datos$x)



ggplot(datos, aes(x = x)) +
  stat_ecdf(geom = "step", color = "blue", size = 1.2) +
  stat_function(fun = pexp, args = list(rate = 1), 
                color = "red", size = 1.2) +
  stat_function(fun = function(x) pexp(x, rate = lambdaM),
                color = "darkgreen", linetype = "dashed", size = 1.2) +
  labs(title = "Función de distribución empírica vs teórica",
       x = "x", y = "F(x)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0,1))



###########Ejercicio 15#########

###a

#i

n <- 6
theta0 <- 3

datos <- data.frame(x= runif(n, min= 0, max = theta0 ))


#ii

#Momentos:

thetaM <- 2*mean(datos$x)

#Maxima Verosimilitud:

thetaMv <- max(datos$x)

#Modificado:

thetaMod <- (n+1)/n * thetaMv

#iii

k <- 1000

estimadoresM <- numeric(k)
estimadoresMv <- numeric(k)
estimadoresMod <- numeric(k)

for(i in 1:k) {
  muestra <- runif(n, min = 0, max = theta0)
  
  estimadoresM[i] <- 2*mean(muestra)
  estimadoresMv[i] <- max(muestra)
  estimadoresMod[i] <- (n+1)/n * (max(muestra))
}


#iv


theta0 <- 3
k <- 1000
ns <- c(6, 10, 20, 40, 80, 200)

# Lista para guardar resultados
resultados <- list()

for (n in ns) {
  simulaciones <- replicate(k, {
    muestra <- runif(n, min = 0, max = theta0)
    c(MV  = max(muestra),
      MOM = 2 * mean(muestra),
      MOD = (n+1)/n * max(muestra))
  })
  
  # Guardar en lista con nombre del n
  resultados[[paste0("n=", n)]] <- list(
    theta_MV  = simulaciones["MV", ],
    theta_mom = simulaciones["MOM", ],
    theta_mod = simulaciones["MOD", ]
  )
}

#v

ECM_resultados <- data.frame()

for (n_name in names(resultados)) {
  # Extraer los vectores de cada estimador
  sim <- resultados[[n_name]]
  
  ECM_MV  <- mean((sim$theta_MV  - theta0)^2)
  ECM_MOM <- mean((sim$theta_mom - theta0)^2)
  ECM_MOD <- mean((sim$theta_mod - theta0)^2)
  
  # Guardar en data frame
  ECM_resultados <- rbind(ECM_resultados,
                          data.frame(n = n_name,
                                     Estimador = c("EMV", "Momento", "Modificado"),
                                     ECM = c(ECM_MV, ECM_MOM, ECM_MOD)))
}

ECM_resultados



######b


#i

# Preparar datos para boxplot
df_EMV <- lapply(names(resultados), function(n_name) {
  data.frame(
    n = as.numeric(sub("n=", "", n_name)),
    EMV = resultados[[n_name]]$theta_MV
  )
}) %>%
  bind_rows()

theta0 <- 3

# Boxplot con línea horizontal en theta0
ggplot(df_EMV, aes(x = factor(n), y = EMV)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  geom_hline(yintercept = theta0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Boxplots de EMV vs tamaño de muestra n",
       x = "Tamaño de muestra n",
       y = "EMV") +
  theme_minimal()


#ii

# Preparar datos para boxplot
df_EM <- lapply(names(resultados), function(n_name) {
  data.frame(
    n = as.numeric(sub("n=", "", n_name)),
    EM = resultados[[n_name]]$theta_mom
  )
}) %>%
  bind_rows()

theta0 <- 3

# Boxplot con línea horizontal en theta0
ggplot(df_EM, aes(x = factor(n), y = EM)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  geom_hline(yintercept = theta0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Boxplots de EM vs tamaño de muestra n",
       x = "Tamaño de muestra n",
       y = "EM") +
  theme_minimal()

#iii

# Preparar datos para boxplot
df_EMOD <- lapply(names(resultados), function(n_name) {
  data.frame(
    n = as.numeric(sub("n=", "", n_name)),
    EMOD = resultados[[n_name]]$theta_mod
  )
}) %>%
  bind_rows()

theta0 <- 3

# Boxplot con línea horizontal en theta0
ggplot(df_EMOD, aes(x = factor(n), y = EMOD)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  geom_hline(yintercept = theta0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Boxplots de EMOD vs tamaño de muestra n",
       x = "Tamaño de muestra n",
       y = "EMV") +
  theme_minimal()


#iv

# Asegurarse de que n sea numérico
ECM_resultados <- ECM_resultados %>%
  mutate(n = as.numeric(gsub("n=", "", n)))

# Graficar
ggplot(ECM_resultados, aes(x = n, y = ECM, color = Estimador)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("EMV" = "black", 
                                "Momento" = "red", 
                                "Modificado" = "blue")) +
  labs(title = "ECM estimado de los tres estimadores vs n",
       x = "Tamaño de muestra n",
       y = "ECM") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#Tiene sentido, el de maxima verosimilitud y modificado son mas chicos que el de momentos.
#Y tambien son los que van mas rapido a 0.



#######Ejercicio 16##########

tormentas <- read.table("tormenta.txt")


#a

#Metodo de momentos:

prom <- mean(tormentas$V1)

S2 <- mean((tormentas$V1 - prom)^2)

alphaMomentos <- prom^2 / S2

lambdaMomentos <- prom / S2


#Maxima Verosimilitud:

library(MASS)

fit <- fitdistr(tormentas$V1, densfun = "gamma")

alpha_MLE <- fit$estimate["shape"]
lambda_MLE <- fit$estimate["rate"]

#b

#P(X>20) = 1 - F(20).

#Maxima Verosimilitud (teorema invarianza):

prob_MLE <- 1 - pgamma(20, shape = alpha_MLE, rate = lambda_MLE)

#Momentos:

prob_MM <- 1 - pgamma(20, shape = alphaMomentos, rate = lambdaMomentos)

#Empirica:

prob_empirica <- sum(tormentas$V1 > 20) / nrow(tormentas)
prob_empirica <- mean(tormentas > 20)

#c

ggplot(tormentas, aes(x = V1)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightgray", color = "black") +
  geom_density(color = "black", size = 1.2, linetype = "solid") +  # KDE no paramétrica
  stat_function(fun = dgamma, args = list(shape = alphaMomentos, rate = lambdaMomentos),
                color = "blue", size = 1.2, linetype = "dashed") +
  stat_function(fun = dgamma, args = list(shape = alpha_MLE, rate = lambda_MLE),
                color = "red", size = 1.2, linetype = "dotted") +
  labs(title = "Histograma de lluvias con densidades superpuestas",
       x = "Lluvia (mm)",
       y = "Densidad") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



#Se observa que las estimaciones de momentos y maxima verosimilitud son similares y se ajustan
#bien a la densidad empirica. Por lo tanto, se podría confirmar que estos datos se distribuyen
#como una gamma. 


######Estimacion de la densidad########


######Ejercicio 21#######

cpu <- scan("cpu.txt")

n <- length(cpu)
s <- sd(cpu)
iqr <- IQR(cpu)
hsil <- 1.06 * min(s,iqr/1.349) * n^(-1/5)


plot(density(cpu, bw = hsil, kernel = "gaussian"),
     main = "Gráfico de densidades con hsil y h default", col = "blue")

lines(density(cpu, kernel = "gaussian"), col = "red")

legend("topleft",
       legend = c("hsil", "h default"),
       col = c("blue", "red"),
       lwd = 1,
       cex = 0.5) 


######Ejercicio 22######

Debernardi <- read.csv(
  "Debernardi.csv"
)

#a

#1
datos <- Debernardi[Debernardi$diagnosis == "1",]$LYVE1
n1 <- length(datos)
s1 <- sd(datos)
iqr1 <- IQR(datos)
hsil1 <- 1.06 * min(s1,iqr1/1.349) * n1^(-1/5)

#2
datos2 <- Debernardi[Debernardi$diagnosis == "2",]$LYVE1
n2 <- length(datos2)
s2 <- sd(datos2)
iqr2 <- IQR(datos2)
hsil2 <- 1.06 * min(s2,iqr2/1.349) * n^(-1/5)

#3
datos3 <- Debernardi[Debernardi$diagnosis == "3",]$LYVE1
n3<- length(datos3)
s3 <- sd(datos3)
iqr3 <- IQR(datos3)
hsil3 <- 1.06 * min(s3,iqr3/1.349) * n^(-1/5)


#b

plot(density(datos, bw = hsil1, kernel = "gaussian"),
     main = "Gráfico de densidades para LYVE1 segun diagnosis con hsil", col = "blue")

lines(density(datos2, kernel = "gaussian"), col = "red")
lines(density(datos3, kernel = "gaussian"), col = "green")

legend("topleft",
       legend = c("1", "2", "3"),
       col = c("blue", "red","green"),
       lwd = 1,
       cex = 0.5) 


#c
plot(density(datos, bw = hsil1, kernel = "epanechnikov"),
     main = "Densidades para LYVE1 segun diagnosis con hsil (epanechnikov)", col = "blue")

lines(density(datos2, kernel = "epanechnikov"), col = "red")
lines(density(datos3, kernel = "epanechnikov"), col = "green")

legend("topleft",
       legend = c("1", "2", "3"),
       col = c("blue", "red","green"),
       lwd = 1,
       cex = 0.5) 
