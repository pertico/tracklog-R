# Interpolacion temporal de valores de elevación negativo
#
# Implementación en R (zoo y data.table)
# En R, el paquete zoo provee la función na.approx(), 
# que realiza la interpolación lineal directamente sobre vectores con marcas de tiempo

library(data.table)
library(arrow)
library(zoo)

dt <- setDT(read_parquet("tracklog.parquet"))
setkey(dt, time)

# 1. Convertir negativos a NA
dt[ele < 0, ele := NA]

# 2. Interpolación lineal basada en tiempo (x = time)
dt[, ele := round(na.approx(ele, x = time, na.rm = FALSE), 2)]

# En caso de que queden NA al inicio o final del dataset (sin vecinos)
cat("Negativos o NAs restantes:", sum(is.na(dt$ele)), "\n")