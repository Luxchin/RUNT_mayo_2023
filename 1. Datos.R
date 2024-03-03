###************************************************************************
###************************************************************************
###                                                                     ***
###                               Código:                               ***
###                   Evaluación RUNT Mintrasporte                      ***
###                   Eficicencia energetica                            ***
###                                                                     ***
###************************************************************************
###************************************************************************

##*************************************************************************
##  @project    Eficiencia energetica                                     *
##  @created   	19 de junio, 2023                                         *
##  @updated   	Febrero, 2024                                             *
##  @revised   	, 2023                                                    *
##  @R version  R version 1.0 (2023-06-19)                                *
##  @OS system  Windows 11                                                *
##  @author     Pinilla Luis - MITS                                       *
##  @email      LGPinillar.trabajo@gmail.com                              *
##*************************************************************************



##****************************************************************
##  1. Lectura datos                                      **----
##****************************************************************


# Limpiar todos los objetos y liberar la memoria
rm(list = ls())


# Verificar que todos los objetos hayan sido eliminados
ls()
#install.packages("dplyr")  # Si aún no tienes instalada la librería dplyr
#update.packages("dplyr")
library(dplyr)


# Solicitar al usuario el número de bases de datos
n_bases <- as.integer(readline("Ingrese el número de bases de datos a leer: "))

# Verificar si el valor ingresado es válido
while (is.na(n_bases) || n_bases <= 0) {
  cat("Número de bases de datos inválido. Inténtelo nuevamente.\n")
  n_bases <- as.integer(readline("Ingrese el número de bases de datos a leer: "))
}

# Crear una lista para almacenar las bases de datos
bases <- list()

# Solicitar al usuario que seleccione las rutas de los archivos txt para las bases de datos
for (i in 1:n_bases) {
  ruta <- file.choose()
  cat("Leyendo archivo:", ruta, "\n")
  bases[[i]] <- read.table(ruta, sep = "|", header = TRUE,fileEncoding = "UTF-8")

}

# Concatenar las bases de datos conservando los encabezados de solo la primera base
datos_concatenados <- do.call(rbind, bases)

##****************************************************************
##  2. Filtrar datos                                            **
##****************************************************************

#################################################################################

# 2.1 Contar elementos vacíos por variable en el total del RUNT
elementos_vacios <- colSums(is.na(datos_concatenados))
# Crear tabla con la variable y la cantidad de elementos vacíos
tabla_elementos_vacios <- data.frame(Variable = names(elementos_vacios), Cantidad = elementos_vacios)

#################################################################################

# 2.1.1 Contar los elementos por clase de vehículo en el total del RUNT

tabla_clase <- table(datos_concatenados$CLASE)
tabla_clase <- as.data.frame(tabla_clase)

# 2.1.2 Frecuancia de los registros esperados

df_frecuencias <- data.frame(Campo = character(),
                             Registros = integer(),
                             Registros_Esperados = integer(),
                             Porcentaje_Efectivo = numeric(),
                             stringsAsFactors = FALSE)

# 2.1.2.1 Iterar sobre cada columna de datos_concatenados
for (col_name in colnames(datos_concatenados)) {
  # Calcular el número total de registros esperados para esta columna
  total_registros_esperados <- nrow(datos_concatenados)
  
  # Contar la cantidad de registros válidos en la columna actual (excluyendo los espacios vacíos)
  num_registros <- sum(!is.na(datos_concatenados[[col_name]]) & datos_concatenados[[col_name]] != "")
  
  # Calcular el porcentaje de registros efectivos para esta columna
  porcentaje_efectivo <- round((num_registros / total_registros_esperados) * 100, 2)
  
  # Agregar los resultados al data frame
  df_frecuencias <- rbind(df_frecuencias, list(Campo = col_name, Registros = num_registros, Registros_Esperados = total_registros_esperados, Porcentaje_Efectivo = porcentaje_efectivo))
}

  # Mostrar el data frame de frecuencias
print(df_frecuencias)

#################################################################################

# 2.3 Tipos de registros

# Crear una lista para almacenar los tipos de respuesta por campo
lista_tipos <- list()

# Lista de campos de interés
campos_interes <- c("MODELO", "ESTADO_VEHICULO", "CLASE", "MARCA", "MODALIDAD", "COMBUSTIBLE", "SERVICIO")

# Iterar sobre cada campo de interés
for (campo in campos_interes) {
  # Obtener los tipos de respuesta únicos para el campo actual
  tipos_respuesta <- unique(datos_concatenados[[campo]])
  
  # Ordenar los tipos de respuesta alfabéticamente de la A a la Z o de menor a mayor según corresponda
  tipos_respuesta <- sort(tipos_respuesta, na.last = TRUE)
  
  # Reemplazar los NA con espacios vacíos
  tipos_respuesta[is.na(tipos_respuesta)] <- ""
  
  # Agregar los tipos de respuesta a la lista
  lista_tipos[[campo]] <- tipos_respuesta
}

# Determinar el máximo de tipos de respuesta para alinear las columnas correctamente
max_tipos <- max(sapply(lista_tipos, length))

# Rellenar la lista de tipos de respuesta con espacios vacíos para alinear las columnas correctamente
for (campo in names(lista_tipos)) {
  lista_tipos[[campo]] <- c(lista_tipos[[campo]], rep("", max_tipos - length(lista_tipos[[campo]])))
}

# Crear la tabla con los tipos de respuesta
tabla_tipos <- as.data.frame(lista_tipos)

# Mostrar la tabla de tipos de respuesta
#print(tabla_tipos)

################################################################################

## 2.4 # Filtrar la matriz para dejar solo los registros con peso menor o igual a 3500
#categoria_m1 <- datos_concatenados[datos_concatenados[, "PESO"] <= 3500, ]



##Filtrar registros por CLASE "AUTOMOVIL" o "CAMIONETA"
##livianos <- subset(datos_concatenados, CLASE %in% c("AUTOMOVIL", "CAMIONETA"))

#################################################################################

## Filtrar la tabla datos_concatenados de acuerdo a las condiciones especificadas en CATEGORIA M1
#Categoria_M1_CLASE_PESO_PASAJERO <- datos_concatenados[datos_concatenados$PESO <= 3500 | datos_concatenados$CLASE %in% c("Automovil", "Camioneta", "Campero"), ]


## 2.5 Crear tabla con los elementos de la variable "MARCA" de la tabla "livianos"
##tabla_marca <- as.data.frame(table(livianos$MARCA))

## Renombrar las columnas de la tabla
##colnames(tabla_marca) <- c("MARCA", "CANTIDAD")

#################################################################################

## 2.6 Contar elementos vacíos por variable en livianos
#elementos_vacios_liv <- colSums(is.na(livianos))
## Crear tabla con la variable y la cantidad de elementos vacíos
#tabla_elementos_vacios_liv <- data.frame(Variable = names(elementos_vacios_liv), Cantidad = elementos_vacios_liv)

################################################################################
# Filtrar la tabla datos_concatenados para crear la tabla M1
M1 <- datos_concatenados[datos_concatenados$CLASE %in% c("AUTOMOVIL", "CAMIONETA", "CAMPERO"), ]


##########################################################
# Contar la cantidad de registros para cada tipo de vehículo (CLASE) año a año
frecuencia_por_año <- table(M1$ANIO_REGISTRO, M1$CLASE)

# Convertir la tabla de frecuencia en un data frame
tabla_frecuencia <- as.data.frame.matrix(frecuencia_por_año)

# Renombrar las columnas
colnames(tabla_frecuencia) <- c("CAMIONETA", "AUTOMOVIL", "CAMPERO")

# Agregar la columna de año
tabla_frecuencia$AÑO <- rownames(tabla_frecuencia)

# Reorganizar las columnas para tener el año primero
tabla_frecuencia <- tabla_frecuencia[, c("AÑO", "CAMIONETA", "AUTOMOVIL", "CAMPERO")]
#####################################################################################

library(ggplot2)

# Filtrar los datos desde el año 2000
datos_desde_2000 <- subset(tabla_frecuencia, AÑO >= 2000)

# Convertir AÑO a factor para evitar que se trate como variable continua
datos_desde_2000$AÑO <- as.factor(datos_desde_2000$AÑO)

# Reestructurar los datos en un formato largo (tidy)
datos_long <- tidyr::gather(datos_desde_2000, key = "Tipo", value = "Cantidad", -AÑO)

# Graficar la tendencia
ggplot(datos_long, aes(x = AÑO, y = Cantidad, color = Tipo, group = Tipo)) +
  geom_line() +
  labs(title = "Tendencia de vehículos desde el año 2000",
       y = "Cantidad",
       color = "Tipo de vehículo") +
  theme_minimal()

# Cargar la biblioteca de pronóstico
library(forecast)

# Convertir AÑO a numérico para el modelo
datos_desde_2000$AÑO <- as.numeric(as.character(datos_desde_2000$AÑO))

# Ajustar un modelo de regresión lineal para cada tipo de vehículo
modelo_camioneta <- lm(CAMIONETA ~ AÑO, data = datos_desde_2000)
modelo_automovil <- lm(AUTOMOVIL ~ AÑO, data = datos_desde_2000)
modelo_campero <- lm(CAMPERO ~ AÑO, data = datos_desde_2000)

# Crear una secuencia de años desde 2023 hasta 20 años después
nuevos_años <- seq(2023, 2023 + 20, by = 1)

# Predecir la cantidad de vehículos para cada tipo usando los modelos
pred_camioneta <- predict(modelo_camioneta, newdata = data.frame(AÑO = nuevos_años))
pred_automovil <- predict(modelo_automovil, newdata = data.frame(AÑO = nuevos_años))
pred_campero <- predict(modelo_campero, newdata = data.frame(AÑO = nuevos_años))

# Crear un data frame con las predicciones
predicciones <- data.frame(AÑO = nuevos_años, CAMIONETA = pred_camioneta, AUTOMOVIL = pred_automovil, CAMPERO = pred_campero)

# Unir las predicciones con los datos originales
datos_completos <- rbind(datos_desde_2000, predicciones)

# Convertir AÑO a factor para evitar que se trate como variable continua en la gráfica
datos_completos$AÑO <- as.factor(datos_completos$AÑO)

# Reestructurar los datos en un formato largo (tidy)
datos_long <- tidyr::gather(datos_completos, key = "Tipo", value = "Cantidad", -AÑO)

# Graficar la tendencia con la proyección
ggplot(datos_long, aes(x = AÑO, y = Cantidad, color = Tipo, group = Tipo)) +
  geom_line() +
  labs(title = "Tendencia de vehículos con proyección desde 2000 hasta 2043",
       y = "Cantidad",
       color = "Tipo de vehículo") +
  theme_minimal()
