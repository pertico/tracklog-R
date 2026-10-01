library(arrow)
library(data.table)
library(openssl)

# Cargar el dataset Parquet
tracklog_file <- "C:\\Users\\perti\\Onedrive\\Documentos\\GPS\\tracklog.parquet"
tracklog <- setDT(read_parquet(tracklog_file))
# setkey(tracklog, track_name, track_fid, time)

# Eliminar registros que no tengan marca de tiempo válida
tracklog <- tracklog[!is.na(time)]

# Genera un UUID MD5 determinista estilo standard (8-4-4-4-12)
make_track_uuid <- function(source, file, fid) {
  raw_hash <- openssl::md5(paste(source, file, fid, sep = "::"))
  # Formatear a estándar UUID (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx)
  paste0(
    substr(raw_hash, 1, 8), "-",
    substr(raw_hash, 9, 12), "-",
    substr(raw_hash, 13, 16), "-",
    substr(raw_hash, 17, 20), "-",
    substr(raw_hash, 21, 32)
  )
}

# 1. Crear la clave única por segmento/track
tracklog[, track_uid := make_track_uuid(source, source_file, track_fid), 
   by = .(source, source_file, track_fid)]

# 2. Definir la CLAVE PRIMARIA y ordenar los datos físicamente por UID + Tiempo
#    setkeyv ordena la tabla en memoria por estas columnas
setkeyv(tracklog, c("track_uid", "time"))

# Anulamos valores de altura negativos y > 4000
tracklog[ele < 0 | ele > 4000, ele := NA_real_]

# 1. Crear la tabla 'grid' de referencia
grid <- tracklog[!is.na(ele), 
                 .(ele = round(mean(ele), 3)), 
                 by = .(lat_grid = round(lat, 4), 
                        lon_grid = round(lon, 4))]

# 2. Generar claves temporales in-place en tracklog
tracklog[, `:=`(lat_grid = round(lat, 4), 
                lon_grid = round(lon, 4))]

# 3. Join por referencia e inyectar ele_grid
tracklog[grid, ele_grid := i.ele, on = .(lat_grid, lon_grid)]

# Imputar valores nulos
tracklog[is.na(ele), ele := ele_grid]

# 4. Eliminar inmediatamente las columnas temporales in-place
tracklog[, c("lat_grid", "lon_grid", "ele_grid") := NULL]


#  Interpolación lineal respetando bordes (na.approx de zoo)
library(zoo)
tracklog[, ele_interpolated := na.approx(ele_clean, na.rm = FALSE), by = .(track_name, track_fid, track_seg_id)]
tracklog[, ele_interpolated := na.locf(ele_clean, na.rm = FALSE),  by = .(track_name, track_fid, track_seg_id)] # Forward fill bordes
tracklog[, ele_interpolated := na.locf(ele_clean, fromLast = TRUE, na.rm = FALSE),  by = .(track_name, track_fid, track_seg_id)] # Backward fill bordes

# Detección de outliers

##  Calcular variables relativas por punto dentro de cada track
tracklog[, `:=`(
  ### Tiempo transcurrido en segundos con el punto anterior
  delta_time = as.numeric(difftime(time, shift(time, type = "lag"), units = "secs")),
  
  ### Salto de elevación absoluto
  delta_ele = abs(ele_clean - shift(ele_clean, type = "lag")),
  
  ### Mediana local en una ventana de 11 puntos (5 atrás, 5 adelante)
  ele_median_local = rollapply(ele_interpolated, width = 11, FUN = median, partial = TRUE, align = "center")
), by = .(track_name, track_fid, track_seg_id)]

## Calcular la velocidad vertical (m/s)
tracklog[, vertical_speed := delta_ele / delta_time]

## Definir las Reglas de Anomalía:
###    - Tasa de ascenso/descenso > 3 m/s (Imposible a pie/bici)
###    - Desviación > 25m respecto a la mediana local contigua
tracklog[, is_outlier := FALSE]
tracklog[vertical_speed > 3.0 | abs(ele - ele_median_local) > 25, is_outlier := TRUE]

### Marcarlos como NA para que entren en la interpolación posterior
tracklog[is_outlier == TRUE, ele_clean := NA]
tracklog[is_outlier == FALSE, ele_clean := ele]

# Re-interpolar los valores eliminados
tracklog[, ele_final := na.approx(ele_clean, na.rm = FALSE), by = .(track_name, track_fid, track_seg_id)]
tracklog[, ele_final := na.locf(ele_final, na.rm = FALSE), by = .(track_name, track_fid, track_seg_id)]
tracklog[, ele_final := na.locf(ele_final, formLast = TRUE, na.rm = FALSE), by = .(track_name, track_fid, track_seg_id)]

# --------------------------


# Agrupar por 'time' deduplicando y calculando las medias
dt_dedup <- dt[, .(
  # Metadatos: se conserva el primer registro del grupo
  source             = first(source),
  track_name         = first(track_name),
  track_type         = first(track_type),
  track_fid          = first(track_fid),
  track_seg_id       = first(track_seg_id),
  track_seg_point_id = first(track_seg_point_id),
  source_file        = first(source_file),
  
  # Coordenadas e elevación: media de los registros duplicados en ese segundo
  lat                = mean(lat, na.rm = TRUE),
  lon                = mean(lon, na.rm = TRUE),
  ele                = mean(ele, na.rm = TRUE)
), by = .(time)]

# Reordenar cronológicamente el resultado
setkey(dt_dedup, time)

# --- Print de Diagnóstico ---
filas_originales <- nrow(dt)
filas_unicas     <- nrow(dt_dedup)
duplicados_elim  <- filas_originales - filas_unicas

cat("=============================================\n")
cat(" DEDUPLICACIÓN POR TIMESTAMP EN R (data.table)\n")
cat("=============================================\n")
cat(sprintf("Filas originales:   %s\n", format(filas_originales, big.mark = ".")))
cat(sprintf("Filas deduplicadas: %s\n", format(filas_unicas, big.mark = ".")))
cat(sprintf("Puntos consolidados:%s\n", format(duplicados_elim, big.mark = ".")))
cat("=============================================\n")