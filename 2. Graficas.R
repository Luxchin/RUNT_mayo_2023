
#install.packages("ggplot2")  # Si aún no tienes instalada la librería dplyr
library(ggplot2)



## Graficar elementos de clase

# Calcular la frecuencia de cada clase
frecuencia <- table(datos_concatenados$CLASE)

# Calcular los porcentajes
porcentajes <- prop.table(frecuencia) * 100

# Crear un vector con las etiquetas de cada sector de la torta
etiquetas <- paste(names(porcentajes), "(", round(porcentajes, 2), "%)", sep = "")

# Configurar parámetros gráficos
par(mar = c(1, 1, 1, 1))  # Margen mínimo
colors <- rainbow(length(frecuencia))  # Colores para cada sector

# Graficar la torta con porcentajes
pie(frecuencia, labels = NA, main = "Distribución por Clase", col = colors)  # No mostrar etiquetas por defecto

# Agregar leyenda fuera de la torta
legend("right", legend = etiquetas, cex = 0.4, fill = colors, xpd = TRUE)

################################################################################

# Esperar la entrada del usuario para continuar
cat("Presione cualquier tecla para continuar...")
readLines(n = 1)

# Limpiar la gráfica actual
dev.off()

################################################################################

# Identificar los porcentajes menores al 1%
umbral <- 1
porcentajes_otros <- porcentajes[porcentajes < umbral]
otros <- sum(porcentajes_otros)
porcentajes_agrupados <- c(porcentajes[porcentajes >= umbral], otros)

# Crear un vector con las etiquetas de cada sector de la torta
etiquetas <- paste(names(porcentajes_agrupados), "(", round(porcentajes_agrupados, 2), "%)", sep = "")

# Configurar parámetros gráficos
par(mar = c(1, 1, 1, 1))  # Margen mínimo
colors <- rainbow(length(porcentajes_agrupados))  # Colores para cada sector

# Graficar la torta con porcentajes
pie(porcentajes_agrupados, labels = NA, main = "Distribución por Clase", col = colors)  # No mostrar etiquetas por defecto

# Agregar leyenda fuera de la torta
legend("right", legend = etiquetas, cex = 0.8, fill = colors, xpd = TRUE)

######################################################################################

# Esperar la entrada del usuario para continuar
cat("Presione cualquier tecla para continuar...")
readLines(n = 1)

# Limpiar la gráfica actual
dev.off()

######################################################################################

# Filtrar los registros de la variable "MODELO" en el rango de 2000 a 2022
datos_filtrados <- livianos[livianos$MODELO >= 2000 & livianos$MODELO <= 2022, ]

# Calcular la frecuencia de cada modelo
tabla_modelo <- table(datos_filtrados$MODELO)

# Crear el gráfico de tendencia con línea punteada
grafico <- ggplot(data = data.frame(Modelo = names(tabla_modelo), Frecuencia = as.numeric(tabla_modelo)), 
                  aes(x = Modelo, y = Frecuencia, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  labs(x = "Modelo", y = "Frecuencia") +
  ggtitle("Tendencia de modelos de vehículos livianos") +
  theme_minimal()

# Mostrar el gráfico
print(grafico)

###################################################################################

# Esperar la entrada del usuario para continuar
cat("Presione cualquier tecla para continuar...")
readLines(n = 1)

# Limpiar la gráfica actual
dev.off()

###################################################################################

# Filtrar los registros de la variable "MODELO" en el rango de 2000 a 2022 para AUTOMOVIL
datos_filtrados_automovil <- livianos[livianos$MODELO >= 2000 & livianos$MODELO <= 2022 & livianos$CLASE == "AUTOMOVIL", ]

# Filtrar los registros de la variable "MODELO" en el rango de 2000 a 2022 para CAMIONETA
datos_filtrados_camioneta <- livianos[livianos$MODELO >= 2000 & livianos$MODELO <= 2022 & livianos$CLASE == "CAMIONETA", ]

# Calcular la frecuencia de cada modelo para AUTOMOVIL
tabla_modelo_automovil <- table(datos_filtrados_automovil$MODELO)

# Calcular la frecuencia de cada modelo para CAMIONETA
tabla_modelo_camioneta <- table(datos_filtrados_camioneta$MODELO)

# Combinar los datos de frecuencia para ambos grupos
datos_combinados <- rbind(data.frame(Modelo = names(tabla_modelo_automovil), Frecuencia = as.numeric(tabla_modelo_automovil), Grupo = "AUTOMOVIL"),
                          data.frame(Modelo = names(tabla_modelo_camioneta), Frecuencia = as.numeric(tabla_modelo_camioneta), Grupo = "CAMIONETA"))

# Crear el gráfico de tendencia con línea punteada para ambos grupos
grafico <- ggplot(data = datos_combinados, aes(x = Modelo, y = Frecuencia, color = Grupo, group = Grupo)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  labs(x = "Modelo", y = "Frecuencia") +
  ggtitle("Tendencia de modelos de AUTOMOVIL y CAMIONETA") +
  theme_minimal()

# Mostrar el gráfico
print(grafico)