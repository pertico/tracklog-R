library(arrow)
library(data.table)
library(openssl)
library(sf)

# Genera un UUID MD5 determinista estilo standard (8-4-4-4-12)
make_track_uuid <- function(source, file, name, fid) {
  raw_hash <- openssl::md5(paste(source, file, name, fid, sep = "::"))
    # Formatear a estándar UUID (xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx)
  paste0(
    substr(raw_hash, 1, 8), "-",
    substr(raw_hash, 9, 12), "-",
    substr(raw_hash, 13, 16), "-",
    substr(raw_hash, 17, 20), "-",
    substr(raw_hash, 21, 32)
  )
}

# Genera digest del contenido de cada track para detectar duplicados
make_content_digest <- function(lat, lon, ele, time) {
  # Formatear la geometría y tiempo en una cadena de texto única por track
  # Se usan 5 decimales en coords (~1m de precisión) para tolerar leves variaciones de redondeo
  content_string <- paste(
    # round(lat, 5), 
    # round(lon, 5), 
    # round(ele, 1), 
    as.numeric(time), 
    collapse = "::"
  )
  
  # Generar el hash MD5 del contenido completo
  as.character(openssl::md5(content_string))
}


# Genera resumen de tracks
make_track_summary <- function (tracklog) {
  summary <- tracklog[, .(
    start_time  = min(time, na.rm = TRUE),
    end_time    = max(time, na.rm = TRUE),
    duration_m  = round(as.numeric(difftime(max(time, na.rm = TRUE), min(time, na.rm = TRUE), units = "mins")), 2),
    distance_m  = sum(distancia_m, na.rm = TRUE),
    points    = .N
    # d_ele <- diff(ele), # Calcular diferencias de elevación punto a punto
    # ele_min     = min(ele, na.rm = TRUE),
    # ele_max     = max(ele, na.rm = TRUE),
    # ele_start   = first(ele),
    # ele_end     = last(ele),
    # ele_gain    = round(sum(d_ele[d_ele > 0], na.rm = TRUE), 1), # Desnivel +
    # ele_loss    = round(abs(sum(d_ele[d_ele < 0], na.rm = TRUE)), 1) # Desnivel -
  ), by = .(track_uid)]
  
  summary [, avg_speed_kmh  := round(distance_m / (duration_m * 60) * 3.6, 2)
    , by = .(track_uid)]

  setkeyv(summary, c("track_uid"))
}

calculate_deltas <- function(tracklog) {
  # Calculamos delta time
  tracklog <- tracklog[, time_delta := time - shift(time), by = track_uid][
    , last_lat := shift(lat), by = track_uid][
      , last_lon := shift(lon), by = track_uid]
  
  # Eliminamos puntos con delta time = 0
  tracklog <- tracklog[is.na(time_delta) | time_delta > 0]
  
  # 1. Identificamos qué filas no tienen NAs en el origen ni en el destino
  filas_con_trayecto <- tracklog[!is.na(last_lon) & !is.na(lon), which = TRUE]
  
  # 2. Inicializamos la columna de distancia en el tracklog original
  tracklog[, distancia_m := NA_real_]
  
  # 3. Si hay datos válidos, calculamos directo sin mutar la tabla original
  if (length(filas_con_trayecto) > 0) {
    
    p_actuales <- st_as_sf(tracklog[filas_con_trayecto], coords = c("lon", "lat"), crs = 4326)
    p_pasados  <- st_as_sf(tracklog[filas_con_trayecto], coords = c("last_lon", "last_lat"), crs = 4326)
    
    # Asignamos el resultado exactamente en las filas correspondientes por referencia
    tracklog[filas_con_trayecto, distancia_m := as.numeric(
      st_distance(p_actuales, p_pasados, by_element = TRUE)
    )]
  }
  
  # Calculamos velocidad del tramo
  tracklog[, speed_kmh := distancia_m/as.numeric(time_delta)*3.6]
  
}


# Cargar Parquet a data.table de forma nativa
cat("Loading data...\n")
tracklog <- setDT(read_parquet("./data/tracklog.parquet"))

# Eliminar filas con time = NA, calcular track_uid y eliminar geometry
cat("Cleaning data (phase 1)...\n")
tracklog <- tracklog[!is.na(time)][
  ,track_uid := make_track_uuid(source, source_file, track_name, track_fid)][
    ,geometry := NULL]

# 1. Crear la clave única por segmento/track
# tracklog[, track_uid := make_track_uuid(source, source_file, track_name, track_fid)]

# 2. Definir la CLAVE PRIMARIA y ordenar los datos físicamente por UID + Tiempo
#    setkeyv ordena la tabla en memoria por estas columnas
setkeyv(tracklog, c("track_uid", "time"))

# 3. Crear el contador de tiempo en segundos (t_sec) dentro de cada track único
# tracklog[, t_sec := as.numeric(difftime(time, min(time), units = "secs")), by = track_uid]

# Crear resumen
cat('Create track summary...\n')
tracklog <- calculate_deltas(tracklog)
summary <- make_track_summary(tracklog)
# Calculo digest de resumen y contenido
summary[, summary_digest := as.character(
  openssl::md5(
    paste(
      as.numeric(start_time), 
      as.numeric(end_time), 
      points, 
      sep = "::")))][, content_digest := tracklog[, .(
  digest = make_content_digest(lat, lon, ele, time)
), by = .(track_uid)]$digest]

# Conservar tracks únicos en summary
# summary <- unique(summary, by = "content_digest")

# Eliminar tracks duplicados de tracklog
cat('Remove duplicates...\n')
# Obtener los track_uid que se deben conservar (primeros únicos por digest)
valid_uids <- summary[!duplicated(content_digest), track_uid]
# Filtrar tracklog conservando únicamente los track_uid válidos
tracklog <- tracklog[track_uid %in% valid_uids]
# Reindexar la clave primaria en memoria
setkeyv(tracklog, c("track_uid", "time"))

# Recreamos summary
cat('Recreate track summary...\n')
tracklog <- calculate_deltas(tracklog)
summary <- make_track_summary(tracklog)

# Mostrar tracks que comparten el mismo summary_digest
"
  En R, la función nativa duplicated() evalúa un vector de arriba a abajo. 
  Por defecto, la primera vez que ve un valor devuelve FALSE, y solo devuelve 
  TRUE a partir de la segunda vez que lo encuentra.
  Para no perder la primera ocurrencia de un track duplicado, se combinan 
  dos evaluaciones con el operador lógico | (OR):
  
  duplicated(summary_digest): Escanea de inicio a fin (de la fila 1 a la N). 
  Marca como TRUE las copias (2ª, 3ª, etc.), pero deja el registro original 
  como FALSE.
  
  duplicated(summary_digest, fromLast = TRUE): Escanea de fin a inicio 
  (de la fila N a la 1). Marca como TRUE las ocurrencias anteriores, incluyendo 
  la que para el primer escaneo era la 'original'.
  
  | (OR): Al unir ambos vectores con un OR, cualquier fila que forme parte 
  de un grupo de duplicados evaluará a TRUE. Si un summary_digest es único 
  en toda la tabla, evaluará a FALSE en ambos lados y será descartado.
"
#summary[duplicated(summary_digest) | duplicated(summary_digest, fromLast = TRUE), 
#     .(track_uid, start_time, end_time, points, duration_m, content_digest)][order(content_digest)]


# Non-Equi Join de summary contra sí misma para buscar inclusiones temporales
cat('Remove subtracks...\n')
subtrack_ids <- summary[
  summary, 
  on = .(start_time <= start_time, end_time >= end_time),
  nomatch = NULL
][
  # Filtrar para excluir auto-coincidencias y exigir mayor duración en el track contenedor
  track_uid != i.track_uid & duration_m > i.duration_m,
  unique(i.track_uid) # i.track_uid es el ID del sub-track (el track más corto)
]
summary[, is_subtrack := track_uid %in% subtrack_ids]

# Extraer e inspeccionar todos los tracks con la misma hora de inicio exacta
# summary[duplicated(start_time) | duplicated(start_time, fromLast = TRUE),
#         .(track_uid, start_time, end_time, points, duration_m)][order(start_time)]

# Extraer e inspeccionar todos los tracks con la misma hora de fin exacta
# summary[duplicated(end_time) | duplicated(end_time, fromLast = TRUE),
#         .(track_uid, start_time, end_time, points, duration_m)][order(start_time)]

# summary[(duplicated(start_time) | duplicated(start_time, fromLast = TRUE)) |
#           (duplicated(end_time) | duplicated(end_time, fromLast = TRUE)),
#         .(track_uid, is_subtrack, start_time, end_time, points, duration_m, summary_digest)][order(start_time,-points)]

# Eliminamos subtracks del tracklog
tracklog <- tracklog[!subtrack_ids]
setkeyv(tracklog, c("track_uid", "time"))

cat('Recreate track summary...\n')
tracklog <- calculate_deltas(tracklog)
summary <- make_track_summary(tracklog)

# Eliminamos track de menos de n puntos
# valid_tracks <- summary[points > 2]$track_uid
# tracklog <- tracklog[track_uid %in% valid_tracks]




# Ampliamos campos de summary
"
summary <- tracklog[, .(
  start_time  = min(time, na.rm = TRUE),
  end_time    = max(time, na.rm = TRUE),
  duration_m  = round(as.numeric(difftime(max(time, na.rm = TRUE), min(time, na.rm = TRUE), units = 'mins'')), 2),
  points    = .N,
  mean_distance = mean(distancia_m, na.rm = TRUE), 
  mean_time_delta = mean(time_delta, na.rm = TRUE),
  mean_speed = mean(distancia_m/as.numeric(time_delta), na.rm = TRUE),
  median_distance = median(distancia_m, na.rm = TRUE), 
  median_time_delta = median(time_delta, na.rm = TRUE),
  median_speed = median(distancia_m/as.numeric(time_delta), na.rm = TRUE),
  sd_distance = sd(distancia_m, na.rm = TRUE), 
  sd_time_delta = sd(time_delta, na.rm = TRUE),
  sd_speed = sd(distancia_m/as.numeric(time_delta), na.rm = TRUE),
  q1_distance = quantile(distancia_m, 0.25, na.rm = TRUE), 
  q1_time_delta = quantile(time_delta, 0.25, na.rm = TRUE),
  q1_speed = quantile(distancia_m/as.numeric(time_delta), 0.25, na.rm = TRUE),
  q3_distance = quantile(distancia_m, 0.75, na.rm = TRUE), 
  q3_time_delta = quantile(time_delta, 0.75, na.rm = TRUE),
  q3_speed = quantile(distancia_m/as.numeric(time_delta), 0.75, na.rm = TRUE),
  max_distance = max(distancia_m, na.rm = TRUE),
  max_speed = max(distancia_m/as.numeric(time_delta), na.rm = TRUE)
  ), 
  track_uid]
"

# Asignamos posibles splits de tracks
cat('Calculate splits...\n')
tracklog[, split := FALSE]
tracklog[time_delta > 3600, split := TRUE]
#tracklog[time_delta > 3600 & distancia_m > 250, split := TRUE]

# Generar el nuevo track_uid recalculado in-place
tracklog[, track_uid := {
  # cumsum(split) crea un contador incremental (0, 0, 1, 1, 2...) que sube en cada split
  segmento <- cumsum(split) + 1L
  
  # Si el track original solo tiene 1 segmento, conserva su track_uid original;
  # si se dividió, le añade la etiqueta de segmento (ej. UUID_seg1, UUID_seg2)
  if (max(segmento) == 1L) {
    track_uid
  } else {
    paste0(track_uid, "_seg", segmento)
  }
}, by = .(track_uid)]

cat('Recreate track summary...\n')
tracklog <- calculate_deltas(tracklog)
summary <- make_track_summary(tracklog)

cat('Calculate track geometry...\n')
# 1. Asegurar el orden cronológico
setorder(tracklog, track_uid, time)

# 2. Filtrar tracks que tengan al menos 2 puntos (requisito mínimo para un LINESTRING)
valid_tracks <- tracklog[, .N, by = track_uid][N >= 2, track_uid]

# 3. Construir las geometrías LINESTRING agrupando directamente por track_uid
track_geom <- tracklog[track_uid %in% valid_tracks, .(
  geometry = list(st_linestring(cbind(lon, lat)))
), by = .(track_uid)]

# 4. Asignar el sistema de coordenadas (CRS 4326 - WGS84) a la columna de listas
track_geom_sf <- st_sf(
  track_uid = track_geom$track_uid,
  geometry  = st_sfc(track_geom$geometry, crs = 4326)
)

# 5. Unir con la tabla summary
tracks <- merge(summary, track_geom_sf, by = "track_uid", all.x = TRUE)
tracks <- st_as_sf(tracks)
# Eliminamos geometrías vacías
tracks <- tracks[!st_is_empty(tracks$geometry) & !is.na(st_geometry(tracks)), ]

# Guardar tracks directamente en formato GeoParquet
st_write(tracks, "./data/tracks.gpkg", driver = "GPKG", delete_dsn = TRUE)


"
R (Quick Plots): Puedes visualizar rápidamente subconjuntos o filtrar tracks 
atípicos directamente con plot(summary_sf['duration_m']) o librerías 
interactiva como mapview::mapview(summary_sf).
"

cat('TODO: Cleaning short tracks...')
#invalid_tracks <- summary[duration_m == 0, track_uid]
