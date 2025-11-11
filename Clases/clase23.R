library(ggplot2)
library(tidyr)

datos <- read.table("rdpercentGDP.txt", header = TRUE)


#a

datos_long <- datos%>%
  pivot_longer(cols = -year,  
               names_to = "Country",  
               values_to = "rd_percent")  

ggplot(datos_long, aes(x = year, y = rd_percent, color = Country)) +
  geom_line() +
  labs(
    title = "Gasto en I+D (% del PIB) por país a lo largo del tiempo",
    x = "Año",
    y = "Gasto en I+D (% del PIB)",
    color = "País"
  ) +
  theme_minimal() + 
  theme(
    legend.position = "right",  # Mantener la leyenda a la derecha
    plot.title = element_text(hjust = 0.5),  # Centrar el título
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar etiquetas del eje X
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(min(datos_long$year), max(datos_long$year), by = 5))


#b
set.seed(123)  

indice_entrenamiento <- sample(1:nrow(datos), size = 0.8 * nrow(datos))


datos_train <- datos[indice_entrenamiento, ]
datos_test  <- datos[-indice_entrenamiento, ]


#c
modelo <- lm(usa ~ argen + ger + china + japan + france + uk + finl, data = datos_train)
summary(modelo)

predicciones <- predict(modelo, newdata = datos_test)

ECM <- mean((datos_test$usa - predicciones)^2)

#d

covariables <- datos[, c("argen", "ger", "china", "japan", "france", "uk", "finl")]

correlacion <- cor(covariables)





