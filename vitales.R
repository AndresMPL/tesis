
#---------------Estadísticas Vitales----

# Directorio

setwd("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/MSPS_Estadisticas_Vitales")

# Ruta y nombres de las tablas

vitales <- "C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/MSPS_Estadisticas_Vitales"
lista_archivos_txt <- list.files(path = vitales, pattern = "\\.txt$", full.names = FALSE)

# Ver tablas cargadas

lista_archivos_txt %>% as.data.frame()

# Función para procesar cada tabla

procesar_tabla <- function(nombre_archivo, lista_ese) {
  tabla <- read_delim(nombre_archivo, delim = "|", escape_double = FALSE, trim_ws = TRUE)
  tabla <- tabla %>%
    mutate(COD_CHIP = substr(cod_ips, 0, 10)) %>%
    rename(VIGENCIA = AÑO) %>%
    filter(COD_CHIP %in% lista_ese$cod_habilitacion)
  return(tabla)
  }

# Lista de nombres de archivos: "lista_archivos_txt"

# Lista donde se almacenarán las tablas procesadas

tablas_vitales <- lapply(lista_archivos_txt, procesar_tabla, lista_ese)

#Haremos unc copia de la lista debido a su tamaño para no reprocesar
deep_copy <- function(x) {
  if (is.list(x)) {
    return(lapply(x, deep_copy))
  } else {
    return(x)
  }
  }

tablas_vitales_copia <- deep_copy(tablas_vitales)

# Opción de restaurar la lista original
tablas_vitales <- deep_copy(tablas_vitales_copia)

#-------------------------------------------------------------------------------

# Limpiar el formato numérico de la "EDAD"

limpiar_columna_numerica <- function(x) {
    x %>%
    filter(grepl("^\\d+$", EDAD)) %>%  # Filtra filas que tengan solo números
    mutate(EDAD = as.numeric(EDAD)) # Convierte la columna a formato numérico
    }

tablas_vitales_limpia <- lapply(tablas_vitales, limpiar_columna_numerica)

#-------------------------------------------------------------------------------

# Volver todo una sola tabla

estad_vitales <- bind_rows(tablas_vitales_limpia)
estad_vitales <- bind_rows(tablas_vitales)

#-------------------------------------------------------------------------------

# Verificar que todas las listas tengan la misma estructura (mismo número de columnas)

verificar_estructura <- function(x) {
  longitudes <- sapply(x, length)
  if(length(unique(longitudes)) != 1) {
    stop("Error: No todas las listas tienen el mismo número de columnas.")
  }
  }

verificar_estructura(tablas_vitales)

df_combinado <- do.call(rbind, lapply(tablas_vitales, as.data.frame))

# Verificamos si hay columnas con tipos de datos diferentes

verificar_tipos <- function(x) {
  tipos_columnas <- sapply(x, class)
  for(i in seq_along(tipos_columnas)) {
    if(length(unique(tipos_columnas[i])) != 1) {
      stop(paste("Error: La columna", names(x)[i], "tiene tipos de datos inconsistentes."))
    }
  }
}

verificar_tipos(df_combinado)

