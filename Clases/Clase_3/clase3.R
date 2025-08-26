#####Clase 3#####

###Ej 4 ####

#a
est1 <- function(v){
  mom <- 1/mean(v)
  return (mom)
}

#b

est2 <- function(v){
  s <- sum(v^2)
  num <- (-1 + sqrt( 1 + 8 * (1/length(v)) * s))
  den <- ((2/length(v)) * s)
  return (num/den)
}


#c

datos1 <- rgeom(10000, prob = 0.1) + 1
est1(datos1)
est2(datos1)


datos2 <- rgeom(10000, prob = 0.4) + 1
est1(datos2)
est2(datos2)








