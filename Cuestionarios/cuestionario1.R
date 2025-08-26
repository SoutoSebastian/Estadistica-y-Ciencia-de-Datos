####Pregunta 1####

library(Lock5Data)
data("FloridaLakes")
help("FloridaLakes")

#12 variables

###Pregunta 2####

#53 observaciones

###Pregunta 3####

#cuantitativa

###Pregunta 4####

histAlcalinidad <- hist(FloridaLakes$Alkalinity, freq = FALSE)
names(histAlcalinidad)

round(max(histAlcalinidad$density), 4)


####Pregunta 7#### 
#trampa

round(mean(FloridaLakes$Alkalinity), 4)
round(median(FloridaLakes$Alkalinity), 4)

####Pregunta 10#### 
round(mean(FloridaLakes$Alkalinity, trim = 0.2), 4)

####Pregunta 12####

cantidad <- sum(FloridaLakes$Alkalinity <= 40)
prob <- cantidad / 53
round(prob, 4)


####Pregunta 13####
library(ggplot2)

ggplot(FloridaLakes, aes(y = Alkalinity)) +
  geom_boxplot(fill = "#77DD77", color = "black") +
  labs(title = "Boxplot alcalinidad", y = "Valores") +
  theme_minimal()

####Pregunta 14####

plot(density(FloridaLakes$Alkalinity))




