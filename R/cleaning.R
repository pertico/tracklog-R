library(arrow)
library(data.table)
library(openssl)

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
    points    = .N
    # d_ele <- diff(ele), # Calcular diferencias de elevación punto a punto
    # ele_min     = min(ele, na.rm = TRUE),
    # ele_max     = max(ele, na.rm = TRUE),
    # ele_start   = first(ele),
    # ele_end     = last(ele),
    # ele_gain    = round(sum(d_ele[d_ele > 0], na.rm = TRUE), 1), # Desnivel +
    # ele_loss    = round(abs(sum(d_ele[d_ele < 0], na.rm = TRUE)), 1) # Desnivel -
  ), by = .(track_uid)]

  setkeyv(summary, c("track_uid"))
  # Calculo digest de resumen  
  summary[, summary_digest := as.character(
    openssl::md5(
      paste(
        as.numeric(start_time), 
        as.numeric(end_time), 
        points, 
        sep = "::"
      ))
  )]
  # Calculo digest de contenido
  summary[, content_digest := tracklog[, .(
    digest = make_content_digest(lat, lon, ele, time)
  ), by = .(track_uid)]$digest]
  
}

# Cargar Parquet a data.table de forma nativa
tracklog <- setDT(read_parquet("./data/tracklog.parquet"))

# Eliminar filas con time = NA y calcular track_uid
tracklog <- tracklog[!is.na(time)][, track_uid := make_track_uuid(source, source_file, track_name, track_fid)]

# 1. Crear la clave única por segmento/track
# tracklog[, track_uid := make_track_uuid(source, source_file, track_name, track_fid)]

# 2. Definir la CLAVE PRIMARIA y ordenar los datos físicamente por UID + Tiempo
#    setkeyv ordena la tabla en memoria por estas columnas
setkeyv(tracklog, c("track_uid", "time"))

# 3. Crear el contador de tiempo en segundos (t_sec) dentro de cada track único
# tracklog[, t_sec := as.numeric(difftime(time, min(time), units = "secs")), by = track_uid]

# Crear resumen
summary <- make_track_summary(tracklog)

# Conservar tracks únicos en summary
# summary <- unique(summary, by = "content_digest")

# Eliminar tracks duplicados de tracklog
# Obtener los track_uid que se deben conservar (primeros únicos por digest)
valid_uids <- summary[!duplicated(content_digest), track_uid]
# Filtrar tracklog conservando únicamente los track_uid válidos
tracklog <- tracklog[track_uid %in% valid_uids]
# Reindexar la clave primaria en memoria
setkeyv(tracklog, c("track_uid", "time"))

# Recreamos summary
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
summary[duplicated(summary_digest) | duplicated(summary_digest, fromLast = TRUE), 
     .(track_uid, start_time, end_time, points, duration_m, content_digest)][order(content_digest)]


# Non-Equi Join de summary contra sí misma para buscar inclusiones temporales
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
summary <- make_track_summary(tracklog)

# Calculamos delta time
tracklog <- tracklog[, time_delta := time - shift(time), by = track_uid]
