library(arrow)
# library(tidyverse) # Carga dplyr, tidyr, ggplot2, etc. a la vez
library(dplyr)
library(tidyr)


# Opción A: Usando read_parquet() directamente
# tracklog <- read_parquet("/workdir/data/tracklog.parquet")

# Opción B: Si prefieres la sintaxis de pipeline (%>%)
tracklog <- read_parquet("/workdir/data/tracklog.parquet") %>% 
  as_tibble()

"
Una gran ventaja al combinar arrow con dplyr es no necesitar cargar todo el 
dataset en la memoria RAM si el fichero es muy grande. Puedes aplicar los 
filtros y agregaciones con sintaxis dplyr directamente sobre el fichero en 
disco y traer a memoria solo el resultado final con collect():

# Abre una conexión al dataset en disco (no consume memoria)
tracklog_arrow <- open_dataset('/workdir/data/tracklog.parquet')

# Realizas el filtrado/agregación con sintaxis dplyr y luego traes los datos
tracklog_dplyr <- tracklog_arrow %>%
  filter(velocidad > 50) %>%
  group_by(id_vehiculo) %>%
  summarise(distancia_total = sum(distancia, na.rm = TRUE)) %>%
  collect()  # Aquí es donde se ejecuta la consulta y se carga en RAM


# Abre una conexión al dataset en disco (no consume memoria)
tracklog_arrow <- open_dataset('/workdir/data/tracklog.parquet')

# Realizas el filtrado/agregación con sintaxis dplyr y luego traes los datos
tracklog_dplyr <- tracklog_arrow %>%
  filter(velocidad > 50) %>%
  group_by(id_vehiculo) %>%
  summarise(distancia_total = sum(distancia, na.rm = TRUE)) %>%
  collect()  # Aquí es donde se ejecuta la consulta y se carga en RAM
"

# -----
# Eliminar filas con time = NA
#
# Opción A: Usando filter() con is.na()
# tracklog <- tracklog %>%
#   filter(!is.na(time))

# Opción B: Usando drop_na() de tidyr (muy limpia y legible)
tracklog <- tracklog %>%
  drop_na(time)

# Detección de tracks duplicados
summary <- tracklog %>%
  arrange(time) %>% # Asegurar que los puntos estén ordenados para que el hash sea consistente
  group_by(track_uid) %>%
  summarise(
    # digest() calcula el hash sobre la cadena completa concatenada de puntos del track
    hash_geom  = digest(paste(geometry, collapse = ""), algo = "md5"),
    num_puntos = n(),
    inicio     = min(time, na.rm = TRUE),
    fin        = max(time, na.rm = TRUE),
    .groups    = "drop"
  )

# Identificar tracks duplicados filtrando aquellos con hash repetido
duplicados_dplyr <- track_summary_dplyr %>%
  group_by(hash_geom) %>%
  filter(n() > 1) %>%
  arrange(hash_geom)


# -----
# Convertimos a nulo los valores negativos y superiores a 4000
tracklog <- tracklog %>%
  mutate(ele = if_else(ele < 0 | ele > 4000, NA_real_, ele))
"
# Alternativa con case_when() para rangos y reglas complejas
tracklog_dplyr <- tracklog_dplyr %>%
  mutate(ele = case_when(
    ele < 0 | ele > 4000 ~ NA_real_,
    TRUE ~ ele # Mantiene el valor original para todo lo demás
  ))
"

# -----
# Actualización de valores NA de altura basado en un grid 10x10 de los datos
#
# Step 1: Crear las columnas del grid
tracklog <- tracklog %>%
  mutate(
    lat_grid = round(lat, 4),
    lon_grid = round(lon, 4)
  )

# Step 2: Calcular el grid de medias
grid <- tracklog %>%
  filter(!is.na(ele)) %>%
  group_by(lat_grid, lon_grid) %>%
  summarise(ele_grid = round(mean(ele), 3), .groups = "drop")

# Step 3: Unir, imputar y limpiar
tracklog <- tracklog %>%
  left_join(grid, by = c("lat_grid", "lon_grid")) %>%
  mutate(ele = coalesce(ele, ele_grid)) %>%
  select(-lat_grid, -lon_grid, -ele_grid)
