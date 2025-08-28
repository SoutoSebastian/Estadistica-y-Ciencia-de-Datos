library(ggplot2)
library(plotly)
library(EnvStats)

#Ej 1
c <- 0.5
xs <- seq(0,1, length.out = 400)
  
  # Ayuda ver qué hace dtri()
  plot(xs, 
       dtri(xs,0,1,c),
       type = "l", lwd = 2,
       xlab = "x", ylab = "Density",
       main = sprintf("Densidad triangular sobre [0,1] con c = %.2f", c))

  

#Ej 2
  
log_likelihood <- function(c, data) {
    
  left_group <- data[data <= c]
  right_group <- data[data > c]
  
  log_likelihood_left <- sum(log(2 * left_group)) - length(left_group) * log(c)
  log_likelihood_right <- sum(log(2 * (1 - right_group))) - length(right_group) * log(1 - c)
  
  
  log_likelihood_total <- log_likelihood_left + log_likelihood_right
  return(log_likelihood_total)
  
}  
  
  
  