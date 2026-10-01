# Paquete en R + box / cli (Aproximación Modular Ligera)
# Si no quieres un orquestador que guarde caché y solo buscas 
# organizar tu código de forma limpia y mantenible:
# 
# box: Te permite importar funciones de forma modular 
# (similar a los import de Python), evitando tener scripts gigantes cargados con source().
# 
# cli: Proporciona una interfaz visual elegante para la consola en R, 
# imprimiendo barras de progreso, iconos y estados de ejecución de cada paso.

library(box)
library(cli)

# Importar módulos propios
box::use(
  modules/io[read_tracklog, write_geopackage],
  modules/cleaning[impute_grid_ele, interpolate_negatives]
)

main <- function() {
  cli_h1("Iniciando Pipeline de Procesamiento GPS")
  
  cli_process_start("Cargando archivo Parquet...")
  df <- read_tracklog("data/tracklog.parquet")
  cli_process_done()
  
  cli_process_start("Ejecutando imputación espacial por cuadrícula...")
  df_grid <- impute_grid_ele(df)
  cli_process_done()
  
  cli_process_start("Corrigiendo elevaciones negativas mediante interpolación...")
  df_clean <- interpolate_negatives(df_grid)
  cli_process_done()
  
  cli_process_start("Generando GeoPackage final...")
  write_geopackage(df_clean, "output/rutas_procesadas.gpkg")
  cli_process_done()
  
  cli_alert_success("¡Pipeline completado con éxito!")
}

main()