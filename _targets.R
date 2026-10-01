# targets (El Estándar de Oro en R)
# El paquete targets es el orquestador oficial del ecosistema R 
# (mantenido dentro del colectivo rOpenSci). Es el equivalente directo 
# a dbt o Kedro, pero diseñado específicamente para R.

library(targets)
library(tarchetypes)

# Cargar las funciones personalizadas de la carpeta R/
tar_source()

# Definir opciones del pipeline y paquetes requeridos
tar_option_set(
  packages = c("arrow", "data.table", "zoo", "sf")
)

# Definición del flujo de trabajo (Graph-based Pipeline)
list(
  # 1. Entrada de datos
  tar_target(
    file_parquet,
    "data/tracklog.parquet",
    format = "file"
  ),
  tar_target(
    raw_data,
    read_parquet(file_parquet)
  ),
  
  # 2. Paso de Imputación por Cuadrícula (Grid)
  tar_target(
    data_grid,
    imputar_elevacion_grid(raw_data)
  ),
  
  # 3. Paso de Interpolación Temporal para Negativos
  tar_target(
    data_interp,
    interpolar_negativos(data_grid)
  ),
  
  # 4. Exportación a GeoPackage
  tar_target(
    gpkg_output,
    exportar_geopackage(data_interp, "output/mis_rutas.gpkg"),
    format = "file"
  )
)