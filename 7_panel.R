
# Creación Panel de Datos-------------------------------------------------------

setwd("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data")

# Tablas con columnas que necesitamos conservar

mx_vitales <- estad_vitales_unico %>% 
  select(VIGENCIA, COD_CHIP, capitulo_ajustado, VALOR) %>% 
  rename(RESULTADO = capitulo_ajustado) %>% 
  mutate(CAPITULO = "VITALES")

mx_efectividad <- efec_valores %>% 
  select(vigencia, cod_chip, Indicador, indicador_i) %>% 
  rename(RESULTADO = Indicador, VALOR = indicador_i, VIGENCIA = vigencia, COD_CHIP = cod_chip) %>% 
  mutate(CAPITULO = "EFECTIVIDAD")

mx_efectividad$VIGENCIA <- as.numeric(mx_efectividad$VIGENCIA)

mx_experiencia <- exper_valores %>% 
  select(vigencia, cod_chip, Indicador, indicador_i) %>% 
  rename(RESULTADO = Indicador, VALOR = indicador_i, VIGENCIA = vigencia, COD_CHIP = cod_chip) %>% 
  mutate(CAPITULO = "EXPERIENCIA")

mx_experiencia$VIGENCIA <- as.numeric(mx_experiencia$VIGENCIA)

mx_seguridad <- segur_valores %>% 
  select(vigencia, cod_chip, Indicador, indicador_i) %>% 
  rename(RESULTADO = Indicador, VALOR = indicador_i, VIGENCIA = vigencia, COD_CHIP = cod_chip) %>% 
  mutate(CAPITULO = "SEGURIDAD")

mx_seguridad$VIGENCIA <- as.numeric(mx_seguridad$VIGENCIA)

mx_ingresos <- ingresos_total %>% 
  select(año, codigo_habilitacion, porc_recaudo) %>% 
  rename(VALOR = porc_recaudo,  VIGENCIA = año, COD_CHIP = codigo_habilitacion) %>% 
  mutate(CAPITULO = "INGRESOS", RESULTADO ="PORCENTAJE DE RECAUDO")

mx_gastos <- gastos_total %>% 
  select(ano, codigo_habilitacion, porc_compromisos, concepto) %>% 
  filter(concepto == "TOTAL DE GASTOS") %>% 
  rename(CAPITULO = concepto, VALOR = porc_compromisos, VIGENCIA = ano, COD_CHIP = codigo_habilitacion) %>% 
  mutate(RESULTADO ="PORCENTAJE DE COMPROMISOS")

mx_compromisos_recaudo <- recaudo_compromisos %>% 
  select(año, codigo_habilitacion, compromisos_recaudo) %>% 
  rename(VALOR = compromisos_recaudo, VIGENCIA = año, COD_CHIP = codigo_habilitacion) %>% 
  mutate(CAPITULO = "NIVEL COMPROMISOS", RESULTADO ="PORCENTAJE COMPROMISOS SOBRE RECAUDO")

mx_cartera_60 <- cartera_total %>% 
  select(ano, codigo_habilitacion, porc_60) %>% 
  rename(VALOR = porc_60, VIGENCIA = ano, COD_CHIP = codigo_habilitacion) %>% 
  mutate(CAPITULO = "GASTOS", RESULTADO ="PORCENTAJE CARTERA A 60")

mx_cartera_61_90 <- cartera_total %>% 
  select(ano, codigo_habilitacion, porc_61_90) %>% 
  rename(VALOR = porc_61_90, VIGENCIA = ano, COD_CHIP = codigo_habilitacion) %>% 
  mutate(CAPITULO = "GASTOS", RESULTADO ="PORCENTAJE CARTERA 61 A 90")

mx_cartera_91_180 <- cartera_total %>% 
  select(ano, codigo_habilitacion, porc_91_180) %>% 
  rename(VALOR = porc_91_180, VIGENCIA = ano, COD_CHIP = codigo_habilitacion) %>% 
  mutate(CAPITULO = "GASTOS", RESULTADO ="PORCENTAJE CARTERA 91 A 180")

mx_cartera_181_360 <- cartera_total %>% 
  select(ano, codigo_habilitacion, porc_181_360) %>% 
  rename(VALOR = porc_181_360, VIGENCIA = ano, COD_CHIP = codigo_habilitacion) %>% 
  mutate(CAPITULO = "GASTOS", RESULTADO ="PORCENTAJE CARTERA 181 A 360")

mx_cartera_mayor361 <- cartera_total %>% 
  select(ano, codigo_habilitacion, porc_361) %>% 
  rename(VALOR = porc_361, VIGENCIA = ano, COD_CHIP = codigo_habilitacion) %>% 
  mutate(CAPITULO = "GASTOS", RESULTADO ="PORCENTAJE CARTERA MAYOR A 360")

mx_pasivos_anterior <- pasivos_total %>% 
  select(ano, codigo_habilitacion, porc_anterior) %>% 
  rename(VALOR = porc_anterior, VIGENCIA = ano, COD_CHIP = codigo_habilitacion) %>% 
  mutate(CAPITULO = "PASIVOS", RESULTADO ="PORCENTAJE PASIVOS VIGENCIA ANTERIOR")

mx_pasivos_actual <- pasivos_total %>% 
  select(ano, codigo_habilitacion, porc_actual) %>% 
  rename(VALOR = porc_actual, VIGENCIA = ano, COD_CHIP = codigo_habilitacion) %>% 
  mutate(CAPITULO = "PASIVOS", RESULTADO ="PORCENTAJE PASIVOS VIGENCIA ACTUAL")

mx_producción_total <- producción_total %>% 
  select(ano, codigo_habilitacion, concepto, total) %>% 
  rename(VALOR = total, RESULTADO = concepto, VIGENCIA = ano, COD_CHIP = codigo_habilitacion) %>% 
  mutate(CAPITULO = "PRODUCCION")

# Unimos las tablas

lista_df <- list(mx_vitales,
                 mx_efectividad,
                 mx_experiencia,
                 mx_seguridad,
                 mx_ingresos,
                 mx_gastos,
                 mx_compromisos_recaudo,
                 mx_cartera_60,
                 mx_cartera_61_90,
                 mx_cartera_91_180,
                 mx_cartera_181_360,
                 mx_cartera_mayor361,
                 mx_pasivos_anterior,
                 mx_pasivos_actual,
                 mx_producción_total
                 )

panel <- bind_rows(lista_df)
panel <- as.data.frame(panel)

panel$VALOR <- as.numeric(as.character(panel$VALOR))
#panel$VALOR[is.na(panel$VALOR)] <- 0 

temp <- panel %>%
  dplyr::summarise(n = dplyr::n(), .by = c(VIGENCIA, COD_CHIP, CAPITULO, RESULTADO)) %>%
  dplyr::filter(n > 1L) %>%  as.data.frame()

nombres <- data.frame(nombre = unique(panel$RESULTADO))

nombres <- data.frame(id = paste0("var", seq_along(nombres$nombre)),nombre = nombres$nombre)
write.table(nombres, file = "nombre_orden.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

nombres <- read_delim("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/R/orden_cuartiles.txt", 
                              delim = "\t", escape_double = FALSE,
                              trim_ws = TRUE)

panel <- panel %>% 
  left_join(nombres, by = join_by("RESULTADO"=="nombre"), keep = FALSE)

# Guardamos los nombres y capítulos

panel_indicadores <- panel %>% 
  group_by(CAPITULO, RESULTADO, orden) %>%  
  summarise(Total = n()) %>% select(-Total)

write.table(panel, file = "panel_original.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

panel <- panel %>%  
  select(-RESULTADO, -CAPITULO, -id, -tipo)

# Generamos un Matriz

panel_matrix <- panel %>% 
  pivot_wider(
    names_from = orden, 
    values_from = VALOR
    ) %>% 
  #replace(is.na(.), 0) %>%
  as.matrix()

panel_matrix <- as.data.frame(panel_matrix)

# Filtramos los años correctos

panel_matrix <- panel_matrix %>% 
  filter(VIGENCIA %in% vigencias$vigencia)

write.table(panel_matrix, file = "matrix.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

matrix <- read_delim("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/matrix.txt", 
                     delim = "\t", escape_double = FALSE,
                     trim_ws = TRUE)

# Tabla para estimar el nivel de reporte por año--------------------------------

nivel_reporte_matriz <- panel %>%
  group_by(VIGENCIA, COD_CHIP, orden) %>%
  summarise(Total = sum(!is.na(VALOR)), .groups = 'drop') %>%
  group_by(VIGENCIA, orden) %>%
  summarise(Total = sum(Total), .groups = 'drop') %>%
  mutate(Porce_Reporte = Total / nrow(lista_ese)) %>%
  select(-Total) %>%
  pivot_wider(names_from = VIGENCIA, values_from = Porce_Reporte) %>%
  mutate(Promedio_porc = round(rowMeans(across(`2012`:`2019`), na.rm = TRUE) * 100, 2)) %>%
  left_join(nombres, by = join_by("orden"), keep = FALSE) %>%
  left_join(panel_indicadores, by = join_by("orden")) %>%
  select(-nombre) %>%
  arrange(desc(Promedio_porc))

# Exporta la tabla del nivel promedio de reporte por indicador

write.table(nivel_reporte_matriz, file = "reporte_indicadores.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Generamos la tabla de los indicadores que superan el promedio

promedio_reporte <- mean(nivel_reporte_matriz$Promedio_porc, na.rm = TRUE)

tabla_reportes <- nivel_reporte_matriz %>% 
  filter(Promedio_porc >= promedio_reporte)

write.table(tabla_reportes, file = "ind_mayor_promedio.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

