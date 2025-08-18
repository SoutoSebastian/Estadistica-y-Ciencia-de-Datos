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

#1
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



