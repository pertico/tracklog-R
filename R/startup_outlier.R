library(data.table)
library(zoo)

# Presuponemos que 'dt' tiene orden temporal e incluye lat/lon/time/ele
setkey(dt, track_fid, time)

# 1. Calcular el tiempo transcurrido en segundos desde el inicio de CADA track
dt[, t_sec := as.numeric(difftime(time, min(time), units = "secs")), by = .(track_name,track_fid, track_seg_id)]

# 2. Obtener la mediana de elevación del "bloque estable" (minutos 2 a 5: 120s a 300s)
#    Si la ruta dura menos de 5 minutos, hace fallback a la mediana de todo el track.
dt[, ele_ref_estable := {
  puntos_estables <- ele[t_sec >= 120 & t_sec <= 300 & !is.na(ele)]
  if (length(puntos_estables) > 0) {
    median(puntos_estables, na.rm = TRUE)
  } else {
    median(ele[!is.na(ele)], na.rm = TRUE)
  }
}, by = .(track_name,track_fid, track_seg_id)]

# 3. Marcar outliers de arranque (Gradient Jump) en los primeros 2 minutos (t_sec < 120)
#    Criterio: Desviación > 30m con respecto a la referencia estable
dt[, is_startup_outlier := FALSE]
dt[t_sec < 120 & abs(ele - ele_ref_estable) > 30, is_startup_outlier := TRUE]

# 4. Limpieza: Asignar NA a los puntos de arranque erróneos
dt[is_startup_outlier == TRUE, ele_clean := NA]
dt[is_startup_outlier == FALSE, ele_clean := ele]

# 5. Re-interpolar el inicio usando Backward Fill (fromLast = TRUE)
dt[, ele_fixed := na.locf(
  na.approx(ele_clean, na.rm = FALSE, maxgap = Inf), 
  fromLast = TRUE, 
  na.rm = FALSE
), by = .(track_name, track_fid, track_seg_id)]