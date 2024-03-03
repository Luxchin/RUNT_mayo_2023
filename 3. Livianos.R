
##****************************************************************
##  3. Procesamiento livianos                              **----
##****************************************************************
##*

#Solicitar al usuario la ruta del archivo
 ruta_base <- file.choose()
 
# Leer la base de datos con delimitador de tabulaciones y codificación UTF-8
 datos_base_livi <- read.table(ruta_base, sep = "\t", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
 
# Obtener la lista de nombres de marcas
 nombres_marca_base <- unique(datos_base_livi$Marca)
# Imprimir la lista de nombres de marcas
 print(nombres_marca_base)

# Filtrar la base de datos "livianos" por las marcas de la lista
 livianos_filtrados_CMM <- livianos[livianos$MARCA %in% nombres_marca_base, ]
# Imprimir la nueva base de datos filtrada
 print(livianos_filtrados__CMM)
 
# Filtrar la tabla por el campo MODELO igual a 2019
 livianos_filtrados_2019 <- livianos_filtrados_CMM[livianos_filtrados_CMM$MODELO == 2019, ]
 
 # Agregar la columna CONSUMO a la tabla livianos_filtrados_2019
 livianos_filtrados_2019 <- livianos_filtrados_2019 %>%
   mutate(CONSUMO = NA_real_)
 
 
 


