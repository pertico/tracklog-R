# La Implementación en R
# El ecosistema de R es uno de los más potentes del mundo para la ciencia 
# de datos geoespaciales y el procesamiento tabular de alta velocidad. 
# Para este problema, la combinación estándar en la industria es:
#
# data.table: El paquete de manipulación tabular en memoria más rápido y eficiente de R (el equivalente en rendimiento a Polars/C++).
#
# sf (Simple Features): La librería geoespacial de referencia en R, construida sobre las librerías nativas C++ GDAL, GEOS y PROJ.


# Opción 2: Enfoque sf (Aproximación Espacial por Radio / R-Tree)
# El paquete sf integra un índice espacial nativo. 
# Usaremos st_join con la función de distancia st_is_within_distance 
# para emular el radio exacto alrededor de cada punto.

library(arrow)
library(sf)
library(dplyr)

# 1. Cargar y convertir a objeto espacial SF (EPSG:4326)
df <- read_parquet("C:\\Users\\perti\\Onedrive\\Documentos\\GPS\\tracklog.parquet")
sf_data <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# 2. Separar conjuntos con y sin elevación
sf_con_ele <- sf_data[!is.na(sf_data$ele), ]
sf_sin_ele <- sf_data[is.na(sf_data$ele), ]

# 3. Búsqueda espacial por radio (0.00005° ≈ 5.5 metros)
# st_join usa el índice espacial R-Tree interno de GEOS
joined <- st_join(
  sf_sin_ele[, c("time")], 
  sf_con_ele[, c("ele")], 
  join = st_is_within_distance, 
  dist = 0.00005
)

# 4. Promediar elevaciones si un punto encuentra múltiples vecinos en el radio
imputed_values <- joined %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarise(ele_imputed = round(mean(ele, na.rm = TRUE), 2))

# 5. Fusionar resultados en el dataset original
sf_data <- sf_data %>%
  left_join(imputed_values, by = "time") %>%
  mutate(ele = coalesce(ele, ele_imputed)) %>%
  select(-ele_imputed)

# --- Print de Validación ---
total_filas <- nrow(sf_data)
nulos_restantes <- sum(is.na(sf_data$ele))
pct_nulos <- (nulos_restantes / total_filas) * 100

cat("=============================================\n")
cat(" RESULTADOS R: SF / R-TREE (SPATIAL RADIO)\n")
cat("=============================================\n")
cat(sprintf("Total registros:             %s\n", format(total_filas, big.mark = ",")))
cat(sprintf("Elevaciones nulas restantes: %s\n", format(nulos_restantes, big.mark = ",")))
cat(sprintf("Porcentaje de nulos:         %.2f%%\n", pct_nulos))
cat("=============================================\n")
