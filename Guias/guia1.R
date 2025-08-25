#Práctica 1 - Estadística descriptiva.

library(ggplot2)
library(tidyr)
library(moments)
library(readxl)

######Ejercicio 1###########

Debernardi <- read.csv(
  "Debernardi.csv"
)

View(Debernardi)

#a
tabla_abs <- table(Debernardi$diagnosis) #frecuencia absoluta

tabla_rel <- prop.table(tabla_abs) #frecuencia relativa

#b

barplot(tabla_rel,
        main = "Frecuencia relativa diagnosis",
        xlab = "Diagnosis",
        ylab = "Frecuencia",
        col = "skyblue")   


#########Ejercicio 2################

titanic <- read.csv(
  "datos_titanic.csv"
)

#a

#P(mujer|sobrevivio)

titanic_s <- titanic[titanic$Survived == 1, ]

sobrevivientes <- sum(titanic$Survived==1)

sobrevivientes_mujeres <- sum(titanic_s$Sex == "female" )

proba <- sobrevivientes_mujeres / sobrevivientes 

#P(mujer)

total <- nrow(titanic)

cant_mujeres <- sum(titanic$Sex == "female")

probaM <- cant_mujeres/total

#b

tabla_contingencia <- table(titanic$Pclass, titanic$Survived)


#P(sobrevivir|Clase)

prop_tabla <- prop.table(tabla_contingencia, margin =1)

#c 

#vamos a hacer un grafico de barras con la proba de sobrevivir siendo de cada clase.

prop_tabla1 <- prop_tabla[,"1"]

barplot(prop_tabla1,
        main = "Proba sobrevivir dada la clase",
        xlab = "Clases",
        ylab = "Proba",
        col = "skyblue")   



##########Ejercicio 3############

iridio <- read.table("iridio.txt", header = TRUE)
rodio <- read.table("rodio.txt", header = TRUE)

###a

#Histogramas:


hist(iridio[,1], 
     main = "Sublimación Iridio",  
     xlab = "Temperatura",         
     ylab = "Frecuencia",             
     col = rgb(0,0,1,0.5),                  
     border = "black",                 
     breaks = 10,                      
     freq = FALSE)

hist(rodio[,1], 
     main = "Sublimación Rodio",  
     xlab = "Temperatura",         
     ylab = "Frecuencia",             
     col = rgb(1,0,0,0.5),                  
     border = "black",                 
     breaks = 10,                      
     freq = FALSE)

#Boxplots:

boxplot(iridio[,1], rodio[,1],
        names = c("Iridio", "Rodio"),
        col = c("skyblue", "salmon"),
        main = "Boxplots de sublimación",
        ylab = "Temperatura")

###b

#Medias

mediaIridio <- mean(iridio[,1])
mediaRodio <- mean(rodio[,1])

#Mediana

medianaIridio <- median(iridio[,1])
medianaRodio <- median(rodio[,1])

#Medias Podadas

#10%
mediapIridio <- mean(iridio[,1], trim = 0.1)
mediapRodio <- mean(rodio[,1], trim = 0.1)

#20%
mediapIridio <- mean(iridio[,1], trim = 0.2)
mediapRodio <- mean(rodio[,1], trim = 0.2)


###c

#Desvio estandar:

dsIridio <- sd(iridio[,1])
dsRodio <- sd(rodio[,1])


#IQR

iqrIridio <- IQR(iridio[,1])
iqrRodio <- IQR(rodio[,1])

#MAD

madIridio <- mad(iridio[,1])
madRodio <- mad(rodio[,1])

###d

#Cuantiles 

cuantilesIridio <- quantile(iridio[,1], probs = c(0.9, 0.75, 0.5, 0.2, 0.1))

cuantilesRodio <- quantile(rodio[,1], probs = c(0.9, 0.75, 0.5, 0.2, 0.1))


####Ejercicio 4#####

###a

salchichasA <- read.table("salchichas_A.txt", header = TRUE)
salchichasB <- read.table("salchichas_B.txt", header = TRUE)
salchichasC <- read.table("salchichas_C.txt", header = TRUE)


salchichasA$tipo <- "A"
salchichasB$tipo <- "B"
salchichasC$tipo <- "C"

names(salchichasA)[names(salchichasA) == "CALORIAS.A"]<- "CALORIAS"
names(salchichasB)[names(salchichasB) == "CALORIAS.B"]<- "CALORIAS"
names(salchichasC)[names(salchichasC) == "CALORIAS.C"]<- "CALORIAS"

names(salchichasA)[names(salchichasA) == "SODIO.A"]<- "SODIO"
names(salchichasB)[names(salchichasB) == "SODIO.B"]<- "SODIO"
names(salchichasC)[names(salchichasC) == "SODIO.C"]<- "SODIO"

salchichas <- rbind(salchichasA, salchichasB, salchichasC)

###b

#Histograma A

hist(salchichasA[,1], 
     main = "Calorias salchichas A",  
     xlab = "Calorias",         
     ylab = "Frecuencia",             
     col = rgb(0,0,1,0.5),                  
     border = "black",                 
     breaks = 10,                      
     freq = TRUE)

#Histograma B

hist(salchichasB[,1], 
     main = "Calorias salchichas B",  
     xlab = "Calorias",         
     ylab = "Frecuencia",             
     col = "skyblue",                  
     border = "black",                 
     breaks = 10,                      
     freq = TRUE)

#Histograma C

hist(salchichasC[,1], 
     main = "Calorias salchichas C",  
     xlab = "Calorias",         
     ylab = "Frecuencia",             
     col = "salmon",                  
     border = "black",                 
     breaks = 10,                      
     freq = TRUE)



#En los graficos A y B se observan grupos. Se podria considerar que hay dos grupos.
#Se observan candidatos a datos atípicos en los gráficos A y B. El histograma C tiene
#observaciones a lo largo de la recta. 

###c

boxplot(salchichasA[,1], salchichasB[,1],salchichasC[,1],
        names = c("A", "B","C"),
        col = c(rgb(0,0,1,0.5),"skyblue", "salmon"),
        main = "Calorías en salchichas de distintos tipos.",
        ylab = "Calorias")


#Como se observan grupos en un boxplot? ayuda para el analisis.


###d 

#Sodio

#Histograma A

hist(salchichasA[,2], 
     main = "Sodio salchichas A",  
     xlab = "Sodio",         
     ylab = "Frecuencia",             
     col = rgb(0,0,1,0.5),                  
     border = "black",                 
     breaks = 10,                      
     freq = TRUE)

#Histograma B

hist(salchichasB[,2], 
     main = "Sodio salchichas B",  
     xlab = "Sodio",         
     ylab = "Frecuencia",             
     col = "skyblue",                  
     border = "black",                 
     breaks = 10,                      
     freq = TRUE)

#Histograma C

hist(salchichasC[,2], 
     main = "Sodio salchichas C",  
     xlab = "Sodio",         
     ylab = "Frecuencia",             
     col = "salmon",                  
     border = "black",                 
     breaks = 10,                      
     freq = TRUE)


#Analisis

#Boxplot

boxplot(salchichasA[,2], salchichasB[,2],salchichasC[,2],
        names = c("A", "B","C"),
        col = c(rgb(0,0,1,0.5),"skyblue", "salmon"),
        main = "Sodio en salchichas de distintos tipos.",
        ylab = "Sodio")



###Ejercicio 5#####

estudiantes <- read.table("estudiantes.txt", header = TRUE)

###a

datos_grupo1 <- estudiantes[,1]
datos_grupo2 <- estudiantes[,2]


#Grupo 1

#Uno simple:

hist(datos_grupo1, 
     freq = FALSE,           
     col = "lightblue", 
     border = "white",
     main = "Histograma datos Grupo 1 con curva normal",
     xlab = "Valores",
     ylab = "Densidad")


curve(dnorm(x, mean = mean(datos_grupo1), sd = sd(datos_grupo1)), 
      col = "red", lwd = 2, add = TRUE)



#Ahora uno con ggplot (Chat gpt!)

# Convertir el vector en data frame (ggplot trabaja mejor con data.frames)
df1 <- data.frame(x = datos_grupo1)

# Histograma + curva normal
ggplot(df1, aes(x = x)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 8,           # podés cambiar el número de bins
                 fill = "lightblue", 
                 color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df1$x), sd = sd(df1$x)), 
                color = "red", size = 1.2) +
  labs(title = "Histograma datos Grupo 1 con curva normal",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()




#Grupo 2

df2 <- data.frame(x = datos_grupo2)

ggplot(df2, aes(x = x)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 8,           
                 fill = "salmon", 
                 color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df2$x), sd = sd(df2$x)), 
                color = "red", size = 1.2) +
  labs(title = "Histograma datos Grupo 1 con curva normal",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()



#QQ-PLOTS

#Grupo 1

qqnorm(datos_grupo1, 
       main = "QQ-plot Grupo 1",
       col = "blue", pch = 19)   # puntos en azul
qqline(datos_grupo1, 
       col = "red", lwd = 2)     # recta en rojo

#Uno con gg-plot


ggplot(df1, aes(sample = x)) +
  stat_qq(color = "blue", size = 2) +           
  stat_qq_line(color = "red", lwd = 1.2) +     
  labs(title = "QQ-plot Grupo 1",
       x = "Cuantiles teóricos",
       y = "Cuantiles muestrales") +
  theme_minimal()

#Grupo 2

ggplot(df2, aes(sample = x)) +
  stat_qq(color = "blue", size = 2) +           
  stat_qq_line(color = "red", lwd = 1.2) +     
  labs(title = "QQ-plot Grupo 2",
       x = "Cuantiles teóricos",
       y = "Cuantiles muestrales") +
  theme_minimal()

#Analisis:En los histogramas se puede ver una cierta correlacion entre la curva normal y los datos
#Sin embargo, por el qqplot, se puede observar que los datos del grupo 2 se asemejan a una normal,
#mientras que los datos del grupo 2 no. 


###b
summary(datos_grupo1)
summary(datos_grupo2)
sd(datos_grupo1)
sd(datos_grupo2)

#A partir de esto parece que los dos grupos estan midiendo cosas distintas. Los valores del grupo
#2 son mayores y varian mas. 

#Boxplots (gg-plot)

# Convertir a formato largo
df_long <- estudiantes %>%
  pivot_longer(cols = c(GRUPO1, GRUPO2),
               names_to = "grupo",
               values_to = "valor")

# Boxplots paralelos
ggplot(df_long, aes(x = grupo, y = valor, fill = grupo)) +
  geom_boxplot() +
  labs(title = "Boxplots paralelos de Grupo 1 y Grupo 2",
       x = "Grupo",
       y = "Valores") +
  scale_fill_manual(values = c("salmon", "lightblue")) +
  theme_minimal()



######Ejercicio 6########

nubes <- read.table("nubes.txt", header = TRUE)

###a

df_nubes <- nubes %>%
  pivot_longer(cols = c(CONTROLES, TRATADAS),
               names_to = "tipoNube",
               values_to = "agua")

ggplot(df_nubes, aes(x = tipoNube, y = agua, fill = tipoNube)) +
  geom_boxplot() +
  labs(title = "Boxplots paralelos nubes controladas y tratadas",
       x = "Tipo nube",
       y = "Agua caida") +
  scale_fill_manual(values = c("salmon", "lightblue")) +
  theme_minimal()


###b (Normalidad)

#Controles

df_controles <- data.frame(x = nubes$CONTROLES)

ggplot(df_controles, aes(x = x)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 10,           
                 fill = "lightblue", 
                 color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df_controles$x), sd = sd(df_controles$x)), 
                color = "red", size = 1.2,
                xlim = c(min(df_controles$x), max(df_controles$x))) +
  labs(title = "Histograma nubes controles + curva normal",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()


ggplot(df_controles, aes(sample = x)) +
  stat_qq(color = "blue", size = 2) +           
  stat_qq_line(color = "red", lwd = 1.2) +     
  labs(title = "QQ-plot Controles",
       x = "Cuantiles teóricos",
       y = "Cuantiles muestrales") +
  theme_minimal()



#Tratadas

df_tratadas <- data.frame(x = nubes$TRATADAS)

ggplot(df_tratadas, aes(x = x)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 10,           
                 fill = "salmon", 
                 color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df_tratadas$x), sd = sd(df_tratadas$x)), 
                color = "red", size = 1.2) +
  labs(title = "Histograma nubes tratadas + curva normal",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()

ggplot(df_tratadas, aes(sample = x)) +
  stat_qq(color = "blue", size = 2) +           
  stat_qq_line(color = "red", lwd = 1.2) +     
  labs(title = "QQ-plot Tratadas",
       x = "Cuantiles teóricos",
       y = "Cuantiles muestrales") +
  theme_minimal()


#En los histogramas no parece que sean normales. Pero en los qq-plots en algunas zonas se pega a
#recta.



###c

#Controles

df_controlesLog <- log(df_controles)

ggplot(df_controlesLog, aes(x = x)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 10,           
                 fill = "lightblue", 
                 color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df_controlesLog$x), sd = sd(df_controlesLog$x)), 
                color = "red", size = 1.2,
                xlim = c(min(df_controlesLog$x), max(df_controlesLog$x))) +
  labs(title = "Histograma nubes controlesLog + curva normal",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()


ggplot(df_controlesLog, aes(sample = x)) +
  stat_qq(color = "blue", size = 2) +           
  stat_qq_line(color = "red", lwd = 1.2) +     
  labs(title = "QQ-plot ControlesLog",
       x = "Cuantiles teóricos",
       y = "Cuantiles muestrales") +
  theme_minimal()


#Tratadas

df_tratadasLog <- log(df_tratadas)

ggplot(df_tratadasLog, aes(x = x)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 10,           
                 fill = "lightblue", 
                 color = "white") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(df_tratadasLog$x), sd = sd(df_tratadasLog$x)), 
                color = "red", size = 1.2,
                xlim = c(min(df_tratadasLog$x), max(df_tratadasLog$x))) +
  labs(title = "Histograma nubes tratadasLog + curva normal",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()


ggplot(df_tratadasLog, aes(sample = x)) +
  stat_qq(color = "blue", size = 2) +           
  stat_qq_line(color = "red", lwd = 1.2) +     
  labs(title = "QQ-plot tratadasLog",
       x = "Cuantiles teóricos",
       y = "Cuantiles muestrales") +
  theme_minimal()


#Ahora con los datos transformados los histogramas se ajustan mas a una normal. Y los qq-plot
#tienen un comportamiento parecido al de antes.

##############################PREGUNTAR################################

###d

df_nubes$agua <- log(df_nubes$agua)

ggplot(df_nubes, aes(x = tipoNube, y = agua, fill = tipoNube)) +
  geom_boxplot() +
  labs(title = "Boxplots paralelos nubes controladas y tratadas (log)",
       x = "Tipo nube",
       y = "log(agua caida)") +
  scale_fill_manual(values = c("salmon", "lightblue")) +
  theme_minimal()


#######Ejercicio 7##########

############################PREGUNTAR################################

data_credit <- read.csv("data_credit_card.csv")


###a (Funcion de distribucion empírica)

#purchases

purchases <- data_credit$purchases
df_purchases <- data.frame(purchases)

ggplot(df_purchases, aes(purchases)) +
  stat_ecdf(geom = "step") +
  labs(title="Función de distribución empírica purchases", y="F(x)")

#Continua

#credit limit

credit_limit <- data_credit$credit_limit
df_credit_lim <- data.frame(credit_limit)

ggplot(df_credit_lim, aes(credit_limit)) +
  stat_ecdf(geom = "step") +
  labs(title="Función de distribución empírica credit_limit", y="F(x)")

#Discreta?

#purchase freq

purchases_freq <- data_credit$purchases_freq
df_purchasesf <- data.frame(purchases_freq)

ggplot(df_purchasesf, aes(purchases_freq)) +
  stat_ecdf(geom = "step") +
  labs(title="Función de distribución empírica purchases_freq", y="F(x)")

#Discreta

#tenure

tenure <- data_credit$tenure
df_tenure <- data.frame(tenure)

ggplot(df_tenure, aes(tenure)) +
  stat_ecdf(geom = "step") +
  labs(title="Función de distribución empírica tenure", y="F(x)")

#Discreta


###b


ggplot(df_credit_lim, aes(x = credit_limit)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 10,           
                 fill = "lightblue", 
                 color = "white") +
  labs(title = "Histograma credit_limit",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()


plot(density(df_credit_lim$credit_limit), main ="Grafico de densidad credit_limit", xlab = "credit_limit", ylab = "Densidad")

#Para tenure no pues para modelar este caso se usa una variable discreta. 
#Para purchase si me parece adecuado. 


###c

tenure_absoluta <- table(df_tenure)
tenure_relativa <- prop.table(tenure_absoluta)


barplot(tenure_relativa,
        main = "Tenure",
        xlab = "Meses restantes",
        ylab = "Frecuencia",
        col = c("pink", "lightblue","#77DD77", "#AEC6CF", "#CBAACB", "#FFB347", "#FFFACD"),
)

#Se observa que 12 es el valor que mas se repite entre los datos y los demás tienen una 
#frecuencia baja y similar entre ellos.


###d 

for(var in names(data_credit)){
  print(paste("Media", var, ":", mean(data_credit[[var]])))
  print(paste("Mediana", var, ":", median(data_credit[[var]])))
  print(paste("Media 0,1 podada", var, ":", mean(data_credit[[var]], trim = 0.1, na.rm = TRUE)))
  print("")
}

###########################PREGUNTAR#####################
  

###e

for(var in names(data_credit)){
  print(paste("Cuantil 0.25", var, ":", quantile(data_credit[[var]], probs = 0.25)))
  print(paste("Cuantil 0.75", var, ":", quantile(data_credit[[var]], probs = 0.75)))
  print(paste("IQR", var, ":", IQR(data_credit[[var]])))
  print(paste("MAD", var, ":", mad(data_credit[[var]])))
  print("")
}


#purchases

ggplot(df_purchases, aes(y = purchases)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot purchases", y = "Valores") +
  theme_minimal()

#parece un IQR chico pero es por los outliers tan grandes.

#credit_limit

ggplot(df_credit_lim, aes(y = credit_limit)) +
  geom_boxplot(fill = "#77DD77", color = "black") +
  labs(title = "Boxplot credit_limit", y = "Valores") +
  theme_minimal()

#varios outliers.

#purchases_freq

ggplot(df_purchasesf, aes(y = purchases_freq)) +
  geom_boxplot(fill = "#CBAACB", color = "black") +
  labs(title = "Boxplot purchases_freq", y = "Valores") +
  theme_minimal()

#no hay outliers, tiene sentido porque los datos no superan 1.


#tenure

ggplot(df_tenure, aes(y = tenure)) +
  geom_boxplot(fill = "#FFFACD", color = "black") +
  labs(title = "Boxplot tenure", y = "Valores") +
  theme_minimal()

#IQR = 0 , pues como se observo antes la gran mayoria de los datos se acumulan en el valor 12.



###f 

for(var in names(data_credit)){
  print(paste("Desvio estandar", var, ":", sd(data_credit[[var]])))
  print(paste("Coeficiente de asimetria", var, ":", skewness(data_credit[[var]])))
  print(paste("Kurtosis", var, ":", kurtosis(data_credit[[var]])))
  print("")
}


###g

#Datos atipicos:
#ya no puedo mas, pero calculo los bigotes y los que sea mas chicos o mas grandes son outliers.

# Calcular cuartiles e IQR
Q1 <- quantile(x, 0.25)
Q3 <- quantile(x, 0.75)
IQR <- Q3 - Q1

limite_inf <- Q1 - 1.5*IQR
limite_sup <- Q3 + 1.5*IQR

# Identificar outliers
outliers <- x[x < limite_inf | x > limite_sup]
outliers

#No deberian excluirse!


#####Ejercicio 8##########

ciclocombinado <- read_excel("ciclocombinado.xlsx")

###a

PE <- ciclocombinado$PE
df_PE <- data.frame(PE)


ggplot(df_PE, aes(x = PE)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 10,           
                 fill = "#FFB347", 
                 color = "white") +
  labs(title = "Histograma PE(MW)",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()

plot(density(PE), main ="Grafico de densidad PE", xlab = "PE(MW)", ylab = "Densidad", col = "#FFB347")


#Se observan dos picos en 440 aprox y 450. Y un rango entre 420 y 500.


###b

#temp > 20.

mayorA20 <- ciclocombinado[ciclocombinado$HighTemp == 1, 1]
plot(density(mayorA20$PE), main ="Grafico de densidad PE (HighTemp > 20)", xlab = "PE(MW)", ylab = "Densidad", col = "#77DD77")

#temp < 20.

menorA20 <- ciclocombinado[ciclocombinado$HighTemp == 0, 1]
plot(density(menorA20$PE), main ="Grafico de densidad PE (HighTemp < 20)", xlab = "PE(MW)", ylab = "Densidad", col = "salmon")


#Grafico juntos:

plot(density(mayorA20$PE), main ="Grafico de densidad PE", xlab = "PE(MW)",
     ylab = "Densidad",col = "#77DD77", xlim = c(420,500))
lines(density(menorA20$PE), xlab = "PE(MW)",
      ylab = "Densidad",col = "salmon", add = TRUE)
legend("topright", legend = c("HighTemp > 20", "HighTemp < 20"),
       col = c("#77DD77", "salmon"), lwd = 2)

#Se observa que la primer "montaña" de la densidad estimada para los datos corresponde a los
#dias en la que la temperatura promedio mas de 20 grados y la segunda a los dias en que se
#promedio menos de 20 grados. Es decir, cuando hubo menor se temperatura se entrego mas potencia.


###c

mayorA20$PE < 300
mean(mayorA20$PE < 300)

#no hay ningun dato de HighTemp = 1 que cumpla que PE < 300

mean(menorA20$PE < 450)


###d
mean(ciclocombinado$PE < 450)


###e
#Uso cuantil 0.10, el 90% de los datos esta por delante.

pe_min <- quantile(mayorA20$PE, probs = 0.10)

###f

pe_min <- quantile(ciclocombinado$PE, probs = 0.10)

###################PREGUNTAR########################



#####Ejercicio 9######


###a

#Diagnosis = 1.

datos_diag1 <- Debernardi[Debernardi$diagnosis == 1, ]

ggplot(datos_diag1, aes(x = LYVE1)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 10,           
                 fill = "#CBAACB", 
                 color = "white") +
  labs(title = "Histograma LYVE1 (diag = 1)",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()

#la gran mayoria de datos se encuentra en el primer bin con valores entre 0 y 0.9 aprox
#y los demas se distribuyen hacia adelante.


#Diagnosis = 2

datos_diag2 <- Debernardi[Debernardi$diagnosis == 2, ]

ggplot(datos_diag2, aes(x = LYVE1)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 10,           
                 fill = "#FFB347", 
                 color = "white") +
  labs(title = "Histograma LYVE1 (diag = 2)",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()

#En este caso la gran mayoria de datos se encuentra distribuida en los primeros 4 bins.


#Diagnosis = 3.

datos_diag3 <- Debernardi[Debernardi$diagnosis == 3, ]

ggplot(datos_diag3, aes(x = LYVE1)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 8,           
                 fill = "#AEC6CF", 
                 color = "white") +
  labs(title = "Histograma LYVE1 (diag = 3)",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()

#Para diagnosis = 3 cambia rotundamente la forma del histograma. La mayoria de los datos no 
#se encuentra al comienzo, sino en valores del rango entre 3 y 8 aproximandamente. Y se
#puede obsevar un outlier.


###b


ggplot() +
  stat_ecdf(data = datos_diag1, aes(x = LYVE1, color = "Grupo 1"), geom = "step") +
  stat_ecdf(data = datos_diag2, aes(x = LYVE1, color = "Grupo 2"), geom = "step") +
  stat_ecdf(data = datos_diag3, aes(x = LYVE1, color = "Grupo 3"), geom = "step") +
  labs(
    title = "Funciones de distribución empírica LYVE1",
    x = "LYVE1",
    y = "F(x)",
    color = "Grupo"
  ) +
  theme_minimal()

#“los valores de la variable LYVE1
#tienden a ser mas altos entre quienes tienen cancer de pancreas que entre quienes sufren
#otras enfermedades asociadas al pancreas”


#Es verdadero. Pues los datos correspondientes a diag = 3 demuestran niveles de LYVE1 más altos
#que para diagnosis 1 o 2. Esto se puede observar en la distribucion empírica, pues la linea
#del grupo 3 esta pr debajo de las demas, eso quiere decir que para cada valor la proba de que
#LYVE1 sea menor a ese valor es mayor para los grupos 1 y 2.



###c

#Diagnosis = 1.

ggplot(Debernardi, aes(x = diagnosis, y = LYVE1, fill = sex)) +
  geom_boxplot() +
  labs(title = "Boxplots paralelos LYVE1 según diagnosis y diferenciando sexo",
       x = "diagnosis",
       y = "LYVE1") +
  scale_fill_manual(values = c("pink", "lightblue")) +
  theme_minimal()

ggplot(Debernardi, aes(x = factor(diagnosis), y = LYVE1, fill = factor(sex))) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(title = "Boxplots paralelos LYVE1 según diagnosis y sexo",
       x = "Diagnosis",
       y = "LYVE1") +
  scale_fill_manual(values = c("pink", "lightblue"),
                    name = "Sexo",
                    labels = c("Mujer", "Hombre")) +
  theme_minimal()


#Parece que para las categoria de diagnosis 1 y 2 parece que el nivel de LYVE1 es mayor en
#hombres. Pero, para las personas que tienen cáncer de páncreas, el nivel de LYVE1 aumenta
#y es similar en ambos sexos.


###d

plot(density(datos_diag1$LYVE1), main ="Grafico de densidad LYVE1", xlab = "LYVE1(ng/ml)",
     ylab = "Densidad",col = "#CBAACB", xlim= c(-1,18))
lines(density(datos_diag2$LYVE1), xlab = "LYVE1(ng/ml)",
      ylab = "Densidad",col = "#FFB347", add = TRUE)
lines(density(datos_diag3$LYVE1), xlab = "LYVE1(ng/ml)",
      ylab = "Densidad",col = "#AEC6CF", add = TRUE)
legend("topright", legend = c("Diag = 1", "Diag = 2", "Diag = 3"),
       col = c("#CBAACB", "#FFB347", "#AEC6CF"), lwd = 2)

#Se puede observar como las densidades correspondientes a diagnosis 1 y 2 tienen un pico
#en los valores mas chicos y luego se acercan al 0. Mientras que la densidad correspondiente
#a diag 3 no tiene picos como las otras pero se extiende hasta el valor 15 aproximandamente.



###e

#Primero a:

#Diagnosis = 1.

df1log <- data.frame(LYVE1 = log(Debernardi$LYVE1[Debernardi$diagnosis == 1]))



ggplot(df1log, aes(x = LYVE1)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 10,           
                 fill = "#CBAACB", 
                 color = "white") +
  labs(title = "Histograma log(LYVE1) (diag = 1)",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()


#Diagnosis = 2

df2log <- data.frame(LYVE1 = log(Debernardi$LYVE1[Debernardi$diagnosis == 2]))

ggplot(df2log, aes(x = LYVE1)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 10,           
                 fill = "#FFB347", 
                 color = "white") +
  labs(title = "Histograma log(LYVE1) (diag = 2)",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()



#Diagnosis = 3.

df3log <- data.frame(LYVE1 = log(Debernardi$LYVE1[Debernardi$diagnosis == 3]))

ggplot(df3log, aes(x = LYVE1)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 8,           
                 fill = "#AEC6CF", 
                 color = "white") +
  labs(title = "Histograma log(LYVE1) (diag = 3)",
       x = "Valores", 
       y = "Densidad") +
  theme_minimal()


#En los primeros dos se observan dos grupos. Pero para diag = 3, se juntan los datos unicamente
#en valores positivos.


#Ahora d:

plot(density(df1log$LYVE1), main ="Grafico de densidad log(LYVE1)", xlab = "log(LYVE1) (ng/ml)",
     ylab = "Densidad",col = "#CBAACB", xlim= c(-8,5), ylim = c(0, 0.7))
lines(density(df2log$LYVE1), xlab = "log(LYVE1) (ng/ml)",
      ylab = "Densidad",col = "#FFB347", add = TRUE)
lines(density(df3log$LYVE1), xlab = "log(LYVE1) (ng/ml)",
      ylab = "Densidad",col = "#AEC6CF", add = TRUE)
legend("topright", legend = c("Diag = 1", "Diag = 2", "Diag = 3"),
       col = c("#CBAACB", "#FFB347", "#AEC6CF"), lwd = 2)


#Se observa como 1 y 2 se extienden en los negativos y 3 unicamente en los positivos con
#un pico notorio.


