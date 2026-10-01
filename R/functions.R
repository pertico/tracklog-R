library(data.table)
library(zoo)

# -----------------------------------------------------------------------------
# FUNCIÓN 1: Limpieza de Rangos Físicos Básicos (Negativos y Nulos)
# -----------------------------------------------------------------------------
filter_basic_bounds <- function(ele) {
  # Convierte valores negativos (< 0) en NA
  ifelse(ele < 0, NA_real_, ele)
}

# -----------------------------------------------------------------------------
# FUNCIÓN 2: Detección de Errores de Arranque / Estabilización (Gradient Jump)
# Evaluamos los primeros 120s contra el bloque estable (minutos 2 a 5)
# -----------------------------------------------------------------------------
filter_startup_outliers <- function(ele, t_sec, threshold_m = 30) {
  ele_clean <- ele
  
  # Identificar la mediana del bloque estable (120s a 300s)
  puntos_estables <- ele[t_sec >= 120 & t_sec <= 300 & !is.na(ele)]
  
  if (length(puntos_estables) > 0) {
    ele_ref <- median(puntos_estables, na.rm = TRUE)
  } else {
    ele_ref <- median(ele[!is.na(ele)], na.rm = TRUE) # Fallback si dura < 2 min
  }
  
  # Marcar como NA los puntos de la salida que difieran más del umbral
  is_startup_outlier <- (t_sec < 120) & (abs(ele - ele_ref) > threshold_m)
  ele_clean[which(is_startup_outlier)] <- NA_real_
  
  return(ele_clean)
}

# -----------------------------------------------------------------------------
# FUNCIÓN 3: Detección de Picos Locales y Saltos Imposibles
# (Velocidad vertical > 3 m/s O desviación > 25m de la mediana móvil adaptativa)
# -----------------------------------------------------------------------------
filter_vertical_spikes <- function(ele, t_sec, max_v_vert = 3.0, max_dev_m = 25) {
  ele_clean <- ele
  
  # 1. Delta de tiempo y altura con el punto anterior
  dt_sec <- c(NA, diff(t_sec))
  d_ele <- c(NA, abs(diff(ele)))
  v_vertical <- d_ele / dt_sec
  
  # 2. Mediana local con ventana adaptativa (partial = TRUE evita NAs en bordes)
  ele_mediana_local <- rollapply(
    ele, 
    width = 11, 
    FUN = median, 
    align = "center", 
    partial = TRUE, 
    na.rm = TRUE
  )
  
  # 3. Detectar anomalías
  is_spike <- (!is.na(v_vertical) & v_vertical > max_v_vert) | 
    (abs(ele - ele_mediana_local) > max_dev_m)
  
  ele_clean[which(is_spike)] <- NA_real_
  return(ele_clean)
}

# -----------------------------------------------------------------------------
# FUNCIÓN DE INTERPOLACIÓN: Resuelve centro y bordes sin errores
# -----------------------------------------------------------------------------
repair_elevation_vector <- function(ele) {
  if (all(is.na(ele))) return(ele) # Caso borde: Track 100% corrupto
  
  # 1. Interpolación lineal del cuerpo
  x_interp <- na.approx(ele, na.rm = FALSE, maxgap = Inf)
  # 2. Forward fill para el final
  x_ffill  <- na.locf(x_interp, na.rm = FALSE)
  # 3. Backward fill para la salida
  x_final  <- na.locf(x_ffill, fromLast = TRUE, na.rm = FALSE)
  
  return(x_final)
}

setkey(dt, track_fid, time)

# Paso 0: Crear columna de tiempo en segundos desde el inicio de cada track
dt[, t_sec := as.numeric(difftime(time, min(time), units = "secs")), by = .(track_fid, track_seg_id)]

# Aplicación en Cascada
dt[, ele_step1 := filter_basic_bounds(ele)]
dt[, ele_step2 := filter_startup_outliers(ele_step1, t_sec), by = .(track_fid, track_seg_id)]
dt[, ele_step3 := filter_vertical_spikes(ele_step2, t_sec),  by = .(track_fid, track_seg_id)]

# Reparación Final
dt[, ele_final := repair_elevation_vector(ele_step3),        by = .(track_fid, track_seg_id)]

