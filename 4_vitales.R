
#---------------Estadísticas Vitales----

# Directorio
setwd("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/MSPS_Estadisticas_Vitales")

# Ruta y nombres de las tablas
vitales <- "C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/MSPS_Estadisticas_Vitales"
lista_archivos_txt <- list.files(path = vitales, pattern = "\\.txt$", full.names = FALSE)
list_eliminar <- c(6,7,8)
lista_archivos_txt <- lista_archivos_txt[-list_eliminar]

# Ver tablas cargadas
lista_archivos_txt %>% as.data.frame()

#------------------ Procesamos una tabla

tabla_DxPPal1617 <- read_delim("DxPPal16_17.txt", delim = "|", escape_double = FALSE, trim_ws = TRUE)

tabla_DxPPal1617_copy <- tabla_DxPPal1617 %>%
  mutate(COD_CHIP = substr(cod_ips, 0, 10)) %>% 
  rename(VIGENCIA = AÑO) %>% 
  filter(COD_CHIP %in% lista_ese$cod_habilitacion)

tabla_DxPPal1617_copy <- tabla_DxPPal1617_copy %>% 
  mutate(
    VIGENCIA = as.numeric(VIGENCIA),
    atenciones = as.numeric(atenciones)   
  ) %>%
  filter(!is.na(VIGENCIA)) %>%
  filter(!is.na(atenciones)) %>%
  left_join(dx_tabla, by = join_by("COD_DIAG_PRIN"=="codigo_cie10"), keep = FALSE)

tabla_DxPPal1617_n <- tabla_DxPPal1617_copy %>% group_by(VIGENCIA, COD_CHIP, nombre, TipoAtencion, capitulo_ajustado) %>% 
  summarise(atenciones = sum(atenciones), .groups = "drop")

as.data.frame(table(addNA(tabla_DxPPal1617_copy$VIGENCIA)))

#--------------------------------------

# Función para procesar cada tabla

procesar_tabla <- function(x, lista_ese, dx_tabla) {
  tabla <- read_delim(x, delim = "|", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  
  tabla <- tabla %>%
    mutate(COD_CHIP = substr(cod_ips, 0, 10)) %>% 
    rename(VIGENCIA = AÑO) %>% 
    filter(COD_CHIP %in% lista_ese$cod_habilitacion)
  
  tabla <- tabla %>% 
    mutate(
          VIGENCIA = as.numeric(VIGENCIA),
          atenciones = as.numeric(atenciones)   
          ) %>%
    filter(!is.na(VIGENCIA)) %>%
    filter(!is.na(atenciones)) %>%
    left_join(dx_tabla, by = join_by("COD_DIAG_PRIN"=="codigo_cie10"), keep = FALSE)
  
  tabla_resumida <- tabla %>% group_by(VIGENCIA, COD_CHIP, nombre, TipoAtencion, capitulo_ajustado) %>% 
                              summarise(atenciones = sum(atenciones), .groups = "drop")
  
  return(tabla_resumida)
  }

# Lista de nombres de archivos: "lista_archivos_txt"

# Lista donde se almacenarán las tablas procesadas

tablas_vitales <- lapply(lista_archivos_txt, procesar_tabla, lista_ese, dx_tabla)

#-------------------------------------- Back Up Seguridad

#Haremos una copia de la lista debido a su tamaño para no reprocesar
deep_copy <- function(x) {
  if (is.list(x)) {
    return(lapply(x, deep_copy))
  } else {
    return(x)
  }
}

tablas_vitales_copia <- deep_copy(tablas_vitales)

# Opción de restaurar la lista original
# tablas_vitales <- deep_copy(tablas_vitales_copia)

#-------------------------------------------------------------------------------

# Volver todo una sola tabla
estad_vitales <- bind_rows(tablas_vitales)

# Tipo de Atenciones
estad_vitales_aten <- as.data.frame(sort(table(estad_vitales$TipoAtencion),decreasing=TRUE))

#-------------------------------------------------------------------------------

# Frecuencia por diagnósticos



#-------------------------------------------------------------------------------

# Generamos la tabla de los diagnósticos que utilizaremos

