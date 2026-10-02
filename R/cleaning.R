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
    round(lat, 3), 
    round(lon, 3), 
#    round(ele, 1), 
#    as.numeric(time), 
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
tracklog <- setDT(read_parquet("/workdir/data/tracklog.parquet"))

# Eliminar filas con time = NA
## Opción A: Filtrado estándar (crea una copia de las filas filtradas)
tracklog <- tracklog[!is.na(time)]
## Opción B: Si prefieres la función na.omit específica para ciertas columnas
##tracklog <- na.omit(tracklog, cols = "time")

# 1. Crear la clave única por segmento/track
tracklog[, track_uid := make_track_uuid(source, source_file, track_name, track_fid)]

# 2. Definir la CLAVE PRIMARIA y ordenar los datos físicamente por UID + Tiempo
#    setkeyv ordena la tabla en memoria por estas columnas
setkeyv(tracklog, c("track_uid", "time"))

# 3. Crear el contador de tiempo en segundos (t_sec) dentro de cada track único
# tracklog[, t_sec := as.numeric(difftime(time, min(time), units = "secs")), by = track_uid]

# Crear resumen
summary <- make_track_summary(tracklog)

# Eliminar tracks duplicados de tracklog
# Obtener los track_uid que se deben conservar (primeros únicos por digest)
valid_uids <- summary[!duplicated(content_digest), track_uid]
# Filtrar tracklog conservando únicamente los track_uid válidos
tracklog <- tracklog[track_uid %in% valid_uids]
# Reindexar la clave primaria en memoria
setkeyv(tracklog, c("track_uid", "time"))

# Recreamos summary
summary <- make_track_summary(tracklog)


# Mostrar tracks que comparten el mismo digest
summary[duplicated(summary_digest) | duplicated(summary_digest, fromLast = TRUE), 
        .(track_uid, start_time, end_time, points, duration_m, content_digest)][order(content_digest)]
# Conservar tracks únicos en summary
# summary <- unique(summary, by = "content_digest")
