# summary.R

summary <- tracklog[, .(
  # Atributos informativos del track (tomando el primer valor)
  # source      = first(source),
  # source_file = first(source_file),
  # track_name  = first(track_name),
  
  # Métricas temporales
  time_start  = min(time, na.rm = TRUE),
  time_end    = max(time, na.rm = TRUE),
  duration_m  = round(as.numeric(difftime(max(time, na.rm = TRUE), min(time, na.rm = TRUE), units = "mins")), 2),
  
  # Métricas de puntos
  n_points    = .N,
  
  # Métricas de elevación (usando la elevación final limpia ele_final)
  ele_min     = min(ele_final, na.rm = TRUE),
  ele_max     = max(ele_final, na.rm = TRUE),
  ele_start   = first(ele_final),
  ele_end     = last(ele_final),
  ele_gain    = round(sum(d_ele[d_ele > 0], na.rm = TRUE), 1), # Desnivel +
  ele_loss    = round(abs(sum(d_ele[d_ele < 0], na.rm = TRUE)), 1) # Desnivel -
  ), by = .(track_uid)]

# Definir la clave primaria en el resumen
setkey(summary, track_uid)
