#####Ej4#######


#a

datos <- c(1.52, 1.65, 1.72, 1.65, 1.72, 1.83, 1.62, 1.75, 1.72, 1.68,
           1.51, 1.65, 1.58, 1.65, 1.61, 1.70, 1.60, 1.73, 1.61, 1.52,
           1.81, 1.72, 1.50, 1.82, 1.65)

n <- length(datos)

prom <- mean(datos)

S <- sd(datos)

cuantil <- qt(1-0.025, n-1)
termino <- cuantil * S/sqrt(n)
intervalo <- c(prom -termino, prom + termino)

#b

cuantil1 <- qchisq(0.05, 24)
cuantil2 <- qchisq(1 - 0.05, 24)


izq <- (24*S^2)/cuantil2
der <- (24*S^2)/cuantil1

intervalob = c(izq,der)

#c



intervaloc <- exp(-intervalo)


########EJ5###########


#d

datos5 <- c(25, 45, 50, 61, 39, 40, 45, 47, 38, 39,
           54, 60, 39, 46, 39, 50, 42, 50, 62, 50)

cuantilInf <- qchisq(0.005,40)
cuantilSup <- qchisq(1-0.005,40)

izq <- cuantilInf/(2*sum(datos5))
der <- cuantilSup/(2*sum(datos5))

intervaloc <- c(izq, der)


######EJ 11####

metodo1 <- function(nivel, datos){
  cuantil <- qnorm(1 - (1-nivel)/2)
  prom <- mean (datos)
  n <- length(datos)
  
  termino <- cuantil * sqrt((prom*(1-prom)/n))
  izq = prom - termino
  der = prom + termino
  
  return(c(izq,der))
}

resolvente <- function(a,b,c){
  raiz <- sqrt(b^2-4*a*c)
  x1 <- (-b + raiz)/(2*a)
  x2 <- (-b - raiz)/(2*a)
  return(sort(c(x1, x2)))
}


metodo2 <- function(nivel, datos){
  cuantil <- qnorm(1 - (1-nivel)/2)
  prom <- mean(datos)
  n <- length(datos)
  
  a <- 1 + (cuantil^2)/n
  b <- -2*prom - (cuantil^2)/n
  c <- prom ^2
  
  return(resolvente(a,b,c))
}



k <- 2000

ns <- c(20, 50, 100)
ps<- c(0.10, 0.50)

resultados <- list()

for (n in ns){
  resultados[[paste0("n =", n)]] <- list()
  
  for (p in ps){
    intervalos1 <- vector("list",k)
    intervalos2 <- vector("list",k)
    
    long1 <- vector("numeric", k)
    long2 <- vector("numeric", k)
    
    cubre1 <- vector("numeric", k)
    cubre2 <- vector("numeric", k)
    
    for (i in 1:k){
      ma <- rbinom(n,1,p)
      
      intervalo1 <- metodo1(0.95, ma)
      l1 <- intervalo1[2] - intervalo1[1]
      intervalos1[[i]] <- intervalo1
      long1[i] <- l1
      cubre1[i] <- intervalo1[1] <= p && p <= intervalo1[2]
      
      intervalo2 <- metodo2(0.95, ma)
      l2 <- intervalo2[2] - intervalo2[1]
      intervalos2[[i]] <- intervalo2
      long2[i] <- l2
      cubre2[i] <- intervalo2[1] <= p && p <= intervalo2[2]
      
    }
    
    resultados[[paste0("n =", n)]][[paste0("p =", p)]] <- list(
      ic1 = intervalos1,
      ic2 = intervalos2,
      l1 = long1,
      l2 = long2,
      c1 = cubre1,
      c2 = cubre2
    )
  }
}



#ITEMS A Y B


# Inicializamos el data.frame para resultados resumidos
summary_results <- data.frame()

for (nname in names(resultados)) {
  for (pname in names(resultados[[nname]])) {
    res <- resultados[[nname]][[pname]]
    
    # longitud esperada
    mean_len1 <- mean(res$l1)
    mean_len2 <- mean(res$l2)
    
    # cubrimiento empírico
    cover1 <- mean(res$c1)
    cover2 <- mean(res$c2)
    
    # extraer valores numéricos de los nombres
    n_val <- as.numeric(gsub("[^0-9]", "", nname))
    p_val <- as.numeric(gsub("[^0-9\\.]", "", pname))
    
    summary_results <- rbind(summary_results,
                             data.frame(n = n_val, p = p_val,
                                        mean_len1 = mean_len1,
                                        mean_len2 = mean_len2,
                                        cover1 = cover1,
                                        cover2 = cover2))
  }
}





