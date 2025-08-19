#Práctica 1 - Estadística descriptiva.

######Ejercicio 1.###########

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
