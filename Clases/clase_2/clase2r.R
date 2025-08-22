######Clase 2###########


###Ej 1###

datos_bebes <- read.table("ENNyS_menorA2.txt", header = TRUE)

#a

#Sexo.
table_abs_Sexo <- table(datos_bebes$Sexo)
table_rel_Sexo <- prop.table(table_abs_Sexo)

#Tipo de embarazo.
table_abs_Embarazo <- table(datos_bebes$Tipo_embarazo)
table_rel_Embarazo <- prop.table(table_abs_Embarazo)

#b GRAFICO DE BARRAS

#Sexo

barplot(table_rel_Sexo,
        main = "Frecuencia relativa Sexo",
        xlab = "Sexo",
        ylab = "Frecuencia",
        col = c("pink", "lightblue")) 

#Tipo de embarazo

barplot(table_rel_Embarazo,
        main = "Frecuencia relativa tipo de embarazo",
        xlab = "Tipo de embarazo",
        ylab = "Frecuencia",
        col = c("pink", "lightblue")) 


#c GRAFICO DE TORTAS

#Sexo

pie(table_rel_Sexo, 
    main = "Frecuencia Géneros", 
    col = c("pink", "lightblue"))


#Tipo de embarazo


pie(table_rel_Embarazo, 
    main = "Frecuencia tipo de embarazo", 
    col = c("pink", "lightblue"))



###Ej 2###

cont_SexoEmbarazo <- table(datos_bebes$Sexo, datos_bebes$Tipo_embarazo)

barplot(cont_SexoEmbarazo,
        beside = TRUE,
        legend = TRUE,
        main = "Distribucion por Sexo y Tipo",
        xlab = "Tipo de embarazo",
        ylab = "Frecuencia",
        col = c("pink", "lightblue") )


