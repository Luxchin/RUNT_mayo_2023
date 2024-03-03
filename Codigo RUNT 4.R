library(dplyr)

# Obtener la cantidad de bases de datos a leer
n_bases <- as.integer(readline("Ingrese la cantidad de bases de datos a leer: "))

# Leer las bases de datos y almacenarlas en una lista
bases_datos <- list()

for (i in 1:n_bases) {
  ruta_archivo <- file.choose()
  datos <- read.delim(ruta_archivo, sep = "|", header = FALSE,fill = TRUE, na.strings = "")
  
  # Verificar si la base de datos tiene encabezados
  if (nrow(datos) > 0) {
    # Asignar los encabezados de la primera base de datos
    if (is.null(names(datos))) {
      colnames(datos) <- c("ID_VEHICULO", "VIN", "ANIO_REGISTRO", "ORIREGIS", "MODELO", "ESTADO_VEHICULO", "CLASE", "MARCA", "LINEA", "CAPACIDAD_CARGA", "CAPACIDAD_PASAJEROS", "CAPACIDAD_PASAJ_SENTADOS", "MODALIDAD", "ORGANISMO_TRANSITO", "DEPARTAMENTO", "MUNICIPIO", "PESO", "POTENCIA", "CARROCERIA", "CILINDRAJE", "COMBUSTIBLE", "COLOR", "SERVICIO", "ANTIGUO_CLASICO", "REPOTENCIADO", "EJES", "TIPO_TRANSMISION", "NIVEL_EMISIONES", "TIPO_ASPIRACION", "MOTIVO_CANCELACION", "FECHADESIN", "APLICA_RTM", "RTM_VIGENTE", "CANTIDAD_RTM_EN_5_ANIOS", "SOAT_VIGENTE", "CANTIDAD_SOAT_EN_5_ANIOS")
    }
    
    # Convertir columnas problemáticas al tipo de datos adecuado
    datos$V5 <- as.numeric(datos$V5)
    datos$V3 <- as.character(datos$V3)
    datos$V10 <- as.numeric(datos$V10)
    datos$V10 <- as.numeric(datos$V11)
    bases_datos[[i]] <- datos
  }
}


# Concatenar las bases de datos
datos_concatenados <- bind_rows(bases_datos)
# Asignar nombres a las variables
colnames(datos_concatenados) <- c("ID_VEHICULO", "VIN", "ANIO_REGISTRO", "ORIREGIS", "MODELO", "ESTADO_VEHICULO", "CLASE", "MARCA", "LINEA", "CAPACIDAD_CARGA", "CAPACIDAD_PASAJEROS", "CAPACIDAD_PASAJ_SENTADOS", "MODALIDAD", "ORGANISMO_TRANSITO", "DEPARTAMENTO", "MUNICIPIO", "PESO", "POTENCIA", "CARROCERIA", "CILINDRAJE", "COMBUSTIBLE", "COLOR", "SERVICIO", "ANTIGUO_CLASICO", "REPOTENCIADO", "EJES", "TIPO_TRANSMISION", "NIVEL_EMISIONES", "TIPO_ASPIRACION", "MOTIVO_CANCELACION", "FECHADESIN", "APLICA_RTM", "RTM_VIGENTE", "CANTIDAD_RTM_EN_5_ANIOS", "SOAT_VIGENTE", "CANTIDAD_SOAT_EN_5_ANIOS")

