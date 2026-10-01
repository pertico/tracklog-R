# La Implementación en R
# El ecosistema de R es uno de los más potentes del mundo para la ciencia 
# de datos geoespaciales y el procesamiento tabular de alta velocidad. 
# Para este problema, la combinación estándar en la industria es:
#
# data.table: El paquete de manipulación tabular en memoria más rápido y eficiente de R (el equivalente en rendimiento a Polars/C++).
#
# sf (Simple Features): La librería geoespacial de referencia en R, construida sobre las librerías nativas C++ GDAL, GEOS y PROJ.

# Opción 1: Enfoque data.table (Aproximación por Cuadrícula)
# data.table utiliza una sintaxis muy concisa basada en la estructura DT[i, j, by] 
# y ejecuta búsquedas por índice (binary search joins) ultrarrápidas en C.

library(arrow)
library(data.table)

# 1. Cargar Parquet a data.table de forma nativa
dt <- setDT(read_parquet("C:\\Users\\perti\\Onedrive\\Documentos\\GPS\\tracklog.parquet"))

# 2. Generar columnas de cuadrícula (4 decimales)
dt[, `:=`(
  lat_grid = round(lat, 4),
  lon_grid = round(lon, 4)
)]

# 3. Crear tabla de referencia con la media de elevación por celda
ref <- dt[!is.na(ele), .(ele_imputed = round(mean(ele), 2)), by = .(lat_grid, lon_grid)]

# 4. In-place Update Join (Modifica en memoria directamente sin duplicar el objeto)
dt[ref, ele_imputed := i.ele_imputed, on = .(lat_grid, lon_grid)]

# 5. Imputar valores nulos
dt[is.na(ele), ele := ele_imputed]

# 6. Limpieza de columnas auxiliares
dt[, c("lat_grid", "lon_grid", "ele_imputed") := NULL]

# --- Print de Validación ---
total_filas <- nrow(dt)
nulos_restantes <- sum(is.na(dt$ele))
pct_nulos <- (nulos_restantes / total_filas) * 100

cat("=============================================\n")
cat(" RESULTADOS R: DATA.TABLE (CUADRÍCULA)\n")
cat("=============================================\n")
cat(sprintf("Total registros:             %s\n", format(total_filas, big.mark = ",")))
cat(sprintf("Elevaciones nulas restantes: %s\n", format(nulos_restantes, big.mark = ",")))
cat(sprintf("Porcentaje de nulos:         %.2f%%\n", pct_nulos))
cat("=============================================\n")
