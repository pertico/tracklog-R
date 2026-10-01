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

# Cargar Parquet a data.table de forma nativa
tracklog <- setDT(read_parquet("./data/tracklog.parquet"))

# Eliminar filas con time = NA
## Opción A: Filtrado estándar (crea una copia de las filas filtradas)
tracklog <- tracklog[!is.na(time)]
## Opción B: Si prefieres la función na.omit específica para ciertas columnas
##tracklog <- na.omit(tracklog, cols = "time")



# 1. Crear la clave única por segmento/track
tracklog[, track_uid := make_track_uuid(source, source_file, fcoalesce(track_name), fcoalesce(track_fid))]

# 2. Definir la CLAVE PRIMARIA y ordenar los datos físicamente por UID + Tiempo
#    setkeyv ordena la tabla en memoria por estas columnas
setkeyv(dt, c("track_uid", "time"))

# 3. Crear el contador de tiempo en segundos (t_sec) dentro de cada track único
dt[, t_sec := as.numeric(difftime(time, min(time), units = "secs")), by = track_uid]




nrow(tracklog)

length(tracklog$source)
length(tracklog$source_file)
length(tracklog$track_name)
length(tracklog$track_fid)
