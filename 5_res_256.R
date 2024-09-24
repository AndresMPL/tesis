
#---------------Resolución 256--------------------------------------------------

setwd("C:/Users/andre/OneDrive - Universidad de los andes/Tesis/data")

#---Tabla Indicadores Efectividad (efec)----

# Cargamos el archivo
efec_i <- read_csv("MSPS_Calidad/IndicadoresIPS-Efectividad-Resol256Total.csv", skip = 3)

# Ajustamos campos, transformamos fecha de año y dejamos solo las ESE en los años que necesitamos
efec <- efec_i %>% rename(indicador_r = Textbox2) %>% 
                                mutate(cod_chip=substr(IPS, 0, 10)) %>% 
                                mutate(vigencia = substr(Fecha,0,4)) %>% 
                                filter(vigencia < 2020, cod_chip %in% lista_ese$cod_habilitacion)

# Generamos un solo resultado de ESE/Año/Indicador (tomamos Dic de cada año)
# Intentamos generar el mayor valor pero los resultados NA perturban más el resultado
efec_tabla <- efec %>% group_by(cod_chip, Indicador, UnidadMedida, vigencia) %>% 
                      filter(Fecha == max(Fecha)) %>% summarise(indicador_r = first(indicador_r), .groups = "drop")

# Resumen por año de los que presentaron o no el reporte de cada indicador *
efec_resumen <- efec_tabla %>% group_by(vigencia) %>% summarise(total = n(), 
                                                                count_numeric = sum(!is.na(indicador_r)), 
                                                                count_na = sum(is.na(indicador_r)), 
                                                                prueba = total- count_numeric - count_na, 
                                                                porc_reporte = sprintf("%.2f%%",((count_numeric/total)*100)))
write.table(efec_resumen, file = "tabla_reportes_efec.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Generamos una tabla con solo resultados reportados
efec_valores <- efec_tabla %>% filter(!is.na(indicador_r)) 

# Ajustamos puntos y comas para los cálculos
efec_valores <- efec_valores %>% mutate(indicador_i = as.numeric(gsub(",",".", gsub("\\.","", indicador_r))))

# Tabla de indicadors por ESE/Año *
write.table(efec_valores, file = "tabla_ind_ese_efec.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Prueba de que eliminamos los NA
sum(is.na(efec_valores$indicador_i)) #Esta suma debe ser cero (0)

# Tabla del promedio de cada indicador por año *
efect_valores_tabla <- efec_valores %>%  group_by(vigencia, Indicador, UnidadMedida) %>% summarise(promedio = mean(indicador_i), .groups="drop")
write.table(efect_valores_tabla, file = "tabla_ind_efec.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

#  Comparamos los indicadores que nos quedaron
indicadores_inicial_efec <- table(efec_i$Indicador) %>%  as.data.frame() # Total indicadores inicial
indicadores_ese_efec <- table(efec_tabla$Indicador) %>%  as.data.frame() # Indicadores que aplican a las ESE
indicadores_final_efec <- table(efect_valores_tabla$Indicador) %>%  as.data.frame() # Indicadores ESE - reportados


#---Tabla Indicadores Experiencia Atencion (exper)----

# Cargamos el archivo
exper_i <- read_csv("MSPS_Calidad/IndicadoresIPS-ExperienciaAtencion-Resol256Total.csv", skip = 3)

# Ajustamos campos, transformamos fecha de año y dejamos solo las ESE en los años que necesitamos
exper <- exper_i %>% rename(indicador_r = Textbox11) %>% 
  mutate(cod_chip=substr(IPS, 0, 10)) %>% 
  mutate(vigencia = substr(Fecha,0,4)) %>% 
  filter(vigencia < 2020, cod_chip %in% lista_ese$cod_habilitacion)

# Generamos un solo resultado de ESE/Año/Indicador (tomamos Dic de cada año)
exper_tabla <- exper %>% group_by(cod_chip, Indicador, UnidadMedida, vigencia) %>% 
  filter(Fecha == max(Fecha)) %>% summarise(indicador_r = first(indicador_r), .groups = "drop")

# Resumen por año de los que presentaron o no el reporte de cada indicador *
exper_resumen <- exper_tabla %>% group_by(vigencia) %>% summarise(total = n(), 
                                                                count_numeric = sum(!is.na(indicador_r)), 
                                                                count_na = sum(is.na(indicador_r)), 
                                                                prueba = total- count_numeric - count_na, 
                                                                porc_reporte = sprintf("%.2f%%",((count_numeric/total)*100)))
write.table(exper_resumen, file = "tabla_reportes_exper.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Generamos una tabla con solo resultados reportados
exper_valores <- exper_tabla %>% filter(!is.na(indicador_r)) 

# Ajustamos puntos y comas para los cálculos
exper_valores <- exper_valores %>% mutate(indicador_i = as.numeric(gsub(",",".", gsub("\\.","", indicador_r))))

# Tabla de indicadors por ESE/Año *
write.table(exper_valores, file = "tabla_ind_ese_exper.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Prueba de que eliminamos los NA
sum(is.na(exper_valores$indicador_i)) #Esta suma debe ser cero (0)

# Tabla del promedio de cada indicador por año *
exper_valores_tabla <- exper_valores %>%  group_by(vigencia, Indicador, UnidadMedida) %>% summarise(promedio = mean(indicador_i), .groups="drop")
write.table(exper_valores_tabla, file = "tabla_ind_exper.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

#Comparamos los indicadores que nos quedaron
indicadores_inicial_exper <- table(exper_i$Indicador) %>%  as.data.frame() # Total indicadores inicial
indicadores_ese_exper <- table(exper_tabla$Indicador) %>%  as.data.frame() # Indicadores que aplican a las ESE
indicadores_final_exper <- table(exper_valores_tabla$Indicador) %>%  as.data.frame() # Indicadores ESE - reportados


#---Tabla Indicadores Seguridad (segur)----

# Creamos lista de archivos de Seguridad
seguridad <- "C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/MSPS_Calidad"
lista_archivos_txt <- list.files(path = seguridad, pattern = "\\.csv$", full.names = FALSE)
lista_archivos_txt <- lista_archivos_txt[grep("Seguridad", lista_archivos_txt)]

# Función para procesar cada tabla
setwd("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/MSPS_Calidad")

procesar_tabla <- function(x) {
  tabla <- read_csv(x, skip = 3)
  return(tabla)
}

# Lista de nombres de archivos: "lista_archivos_txt"

# Lista donde se almacenarán las tablas procesadas
tabla_seguridad <- lapply(lista_archivos_txt, procesar_tabla)

# Volver todo una sola tabla
segur_i <- bind_rows(tabla_seguridad)

# Ajustamos campos, transformamos fecha de año y dejamos solo las ESE en los años que necesitamos
# Realizamos un ajuste adicional de eliminar las fechas terminadas en 30 o 31.
segur <- segur_i %>% rename(indicador_r = Textbox6) %>% 
  mutate(cod_chip=substr(IPS, 0, 10)) %>% 
  mutate(vigencia = substr(Fecha,0,4)) %>% 
  filter(vigencia < 2020, cod_chip %in% lista_ese$cod_habilitacion, !grepl("30$", Fecha), !grepl("31$", Fecha))

# Generamos un solo resultado de ESE/Año/Indicador (tomamos Dic de cada año)
segur_tabla <- segur %>% group_by(cod_chip, Indicador,UnidadMedida, vigencia) %>% 
  filter(Fecha == max(Fecha)) %>% summarise(indicador_r = first(indicador_r), .groups = "drop")

# Resumen por año de los que presentaron o no el reporte de cada indicador *
segur_resumen <- segur_tabla %>% group_by(vigencia) %>% summarise(total = n(), 
                                                                  count_numeric = sum(!is.na(indicador_r)), 
                                                                  count_na = sum(is.na(indicador_r)), 
                                                                  prueba = total- count_numeric - count_na, 
                                                                  porc_reporte = sprintf("%.2f%%",((count_numeric/total)*100)))
setwd("C:/Users/andre/OneDrive - Universidad de los andes/Tesis/data")
write.table(segur_resumen, file = "tabla_reportes_segur.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Generamos una tabla con solo resultados reportados
segur_valores <- segur_tabla %>% filter(!is.na(indicador_r)) 

# Ajustamos puntos y comas para los cálculos
segur_valores <- segur_valores %>% mutate(indicador_i = as.numeric(gsub(",",".", gsub("\\.","", indicador_r))))

# Tabla de indicadores por ESE/Año *
write.table(segur_valores, file = "tabla_ind_ese_segur.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Prueba de que eliminamos los NA
sum(is.na(segur_valores$indicador_i)) #Esta suma debe ser cero (0)

# Tabla del promedio de cada indicador por año *
segur_valores_tabla <- segur_valores %>%  group_by(vigencia, Indicador, UnidadMedida) %>% summarise(promedio = mean(indicador_i), .groups="drop")
write.table(segur_valores_tabla, file = "tabla_ind_segur.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Comparamos los indicadores que nos quedaron
indicadores_inicial_segur <- table(segur_i$Indicador) %>%  as.data.frame() # Total indicadores inicial
indicadores_ese_segur <- table(segur_tabla$Indicador) %>%  as.data.frame() # Indicadores que aplican a las ESE
indicadores_final_segur <- table(segur_valores_tabla$Indicador) %>%  as.data.frame() # Indicadores ESE - reportados


#----------------Tablas exportadas de indicadores reportados

# Tablas exportadas - Indicadores que quedaron general en las ESE - Cuenta los años por indicador
write.table(indicadores_final_efec, file = "indicadores_final_efec.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(indicadores_final_exper, file = "indicadores_final_exper.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(indicadores_final_segur, file = "indicadores_final_segur.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)


#---------------Matriz

# Datos que utilizaremos para el Panel de Datos
efec_valores
exper_valores
segur_valores
