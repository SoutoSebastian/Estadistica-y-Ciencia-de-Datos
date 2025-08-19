#Empezando:

2+3

3*3

10/2

2^3

peso <- 6.5
peso2 <- 4
peso+peso2


vector <- c()
vector2 <- c(3,6,2,1,0,-9)
vector2 + 2
vector2^2
vector2[1]
vector
vector2[-1] #todos menos el primero
vector <- c(5,vector) #concatenar
vector


vector2[2:4] 

sort(vector2)
length(vector2)
sum(vector2)
mean(vector2)
min(vector2)
max(vector2)



#operadores logicos
# y = &, o = |, negacion !

a <-3
b <- 2
(b>a) | (a == 3)
(b>a) & (a == 3)
!(b==a)

#Funciones

nombre <- function(variable){
  # Cuerpo de la funcion
}


sumatoria <- function(vec){
  acum <- 0
  n <- length(vec)
  for (i in 1:n){
    acum <- acum + vec[i]
  }
  return(acum)
}

sumatoria(vector2)

#Graficos

seq(-3, 3,  by=1)
seq(-3, 3,  length.out=100)

xs <- seq(-3, 3,  length.out=100)
ys <- xs^2

plot(xs, ys, type="l")

#DataFrames

datos <- read.csv(
  "nombre.txt", sep=" "
)


View(mtcars)

mtcars[1:2,2:4]
ncol(mtcars)
nrow(mtcars)

#Guia de actividades

#Ejercicio 1

#a

suma <- 0
for (i in 1:1000){
  suma <- suma + i
}
suma  

#b
sum <- 0
i <- 1
while(sum<=10000){
  sum <- sum +i
  i <- i +1
}
i

#c

positivas <- function(vec){
  res <- 0
  n <- length(vec)
  for(i in 1:n){
    if(vec[i] > 0){
      res <- res + vec[i]
    }
  }
  return(res)
}

positivas(vector2)

#Ejercicio 2

ejex <- seq(0, 1, by=0.02)
ejey <- ejex*(1-ejex)

plot(ejex, ejey)


#Ejercicio 3

valores <- seq(0, 2*pi, length.out = 100)

plot(valores, sin(valores), type = "l", col="blue",
     xlab="Eje x", ylab="Eje y",
     main="Funciones trigonométricas")

lines(valores, cos(valores), col="red")

lines(valores, cos(valores^2), col="green")

#Ejercicio 4

datos <- read.csv(
  "autos.txt", sep=" "
)

View(datos)

#a
datos[3,]

#b
datos[,2]

#c
min(datos[,2])

#d
sum(datos[1:4,1])

#e
apply(datos, 1, sum)

apply(datos, 2,sum)

#f

ejex <- datos[,1]
ejey <- datos[,2]

plot(ejex, ejey, xlab="Precio", ylab="Calidad",)

#g
autos_ordenados <- datos[order(datos$precio),]
View(autos_ordenados)

#Ejercicio 5

View(mtcars)

#a
autos_g4 <- mtcars[mtcars$gear==4,]

rownames(autos_g4)


#b

autos_g4m <- mtcars[mtcars$gear == 4 & mtcars$am == 1, ]

rownames(autos_g4m)

#c
autos_g4m <- mtcars[mtcars$gear == 4 | mtcars$am == 1, ]

rownames(autos_g4m)

#d

table(mtcars$am)

mtcars$am <- factor(mtcars$am, labels = c("Automática", "Manual"))
View(mtcars)


#Ejercicio 6

#a

x <- rnorm(1000)

#histograma
hist(x, 
     main = "Normal",   # Título del gráfico
     xlab = "Realizaciones",         # Nombre del eje X
     ylab = "Frecuencia",             # Nombre del eje Y
     col = "skyblue",                  # Color de las barras
     border = "black",                 # Color del borde de las barras
     breaks = 10,                      # Cantidad de intervalos
     freq = FALSE)                      


#boxplot

boxplot(x,
        main = "Normal",             # Título
        xlab = "Realizaciones",      # Nombre del eje X
        ylab = "Valor",              # Nombre del eje Y
        col = "skyblue")             # Color de la caja


#qq-plot

qqnorm(x)                 # dibuja los cuantiles observados vs teóricos
qqline(x, col = "red")     # agrega la línea de referencia



#b

x <- rbinom(1000,10,0.4) #binomial

x <- runif(1000, 4,8) #uniforme

x <- rt(1000)   #t-student

x <- rchisq(1000,50) #chi-cuadrado

x <- rf(1000,90,40) #F de Snedecor

x <- rgamma(1000,0.7) #Gamma


