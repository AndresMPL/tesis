
# Método Cuartiles--------------------------------------------------------------

# Debe cargarse un listado con el nombre de variables SELECCIONADAS
# Recomendación: Ir a carpeta para depurar los indicadores
# Evaluamos variables diferentes para el periodo 2012 a 2015 y 2016 a 2019

nombres_variables_15 <- read_delim("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/R/variables15.txt", 
                                delim = "\t", escape_double = FALSE,
                                trim_ws = TRUE) %>% select(id)

nombres_variables_15 <- nombres_variables_15 %>% 
  add_row(id="VIGENCIA") %>% 
  add_row(id="COD_CHIP")

nombres_variables_15 <- nombres_variables_15$id

nombres_variables_19 <- read_delim("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/R/variables19.txt", 
                                   delim = "\t", escape_double = FALSE,
                                   trim_ws = TRUE) %>% select(id)

nombres_variables_19 <- nombres_variables_19 %>% 
  add_row(id="VIGENCIA") %>% 
  add_row(id="COD_CHIP")

nombres_variables_19 <- nombres_variables_19$id

#-------------------------------------------------------------------------------

# La variable var60 - PORCENTAJE COMPROMISOS SOBRE RECAUDO
# no se evalúa en este conjunto
# para evitar colinealidad con Recaudo y Compromisos

#-------------------------------------------------------------------------------
# Análisis 2012 a 2015

matrix_data <- matrix %>% 
  select(all_of(nombres_variables_15))

matrix_data <- matrix_data %>% 
  left_join(lista_ese, by = join_by("COD_CHIP"=="cod_habilitacion"), keep = FALSE) %>% 
  filter(VIGENCIA < 2016)

matrix_cuartiles <- matrix_data %>%
  group_by(nivel, VIGENCIA, caracter) %>%
  mutate(across(starts_with("ascendente"), 
                list(Cuartil = ~ ntile(desc(.), 4)), # Ascendente con cuartiles inversos
                .names = "{.col}_cuartil")) %>%
  mutate(across(starts_with("descendente"), 
                list(Cuartil = ~ ntile(., 4)), # Descendente con cuartiles estándar
                .names = "{.col}_cuartil")) %>%
  ungroup() %>%
  rowwise() %>%
  #rename(test60 = var60_cuartil) %>%
  mutate(
    #test60 = if_else(orden_var60 <= 1, 1, 4),
    Promedio_Cuartil = mean(c_across(ends_with("cuartil")), na.rm = TRUE)
  )

matrix_cuartiles <- matrix_cuartiles %>%
  group_by(nivel, VIGENCIA, caracter) %>%
  mutate(Nuevo_Riesgo_Cuartil = ntile(Promedio_Cuartil, 4),
         Nuevo_Riesgo = case_when(
           Nuevo_Riesgo_Cuartil == 1 ~ "Sin riesgo",
           Nuevo_Riesgo_Cuartil == 2 ~ "Riesgo bajo",
           Nuevo_Riesgo_Cuartil == 3 ~ "Riesgo medio",
           Nuevo_Riesgo_Cuartil == 4 ~ "Riesgo alto"
         )) %>%
  ungroup()

# Tabla de resumen de resultados**

mx_cuartil_resumen <- matrix_cuartiles %>% 
  select(VIGENCIA, COD_CHIP, nombre_ese,nivel, caracter,Promedio_Cuartil, Nuevo_Riesgo_Cuartil, Nuevo_Riesgo)

mx_cuartil_tabla1 <- mx_cuartil_resumen[complete.cases(mx_cuartil_resumen), ]

#-------------------------------------------------------------------------------
# Análisis 2016 a 2019

matrix_data <- matrix %>% 
  select(all_of(nombres_variables_19))

matrix_data <- matrix_data %>% 
  left_join(lista_ese, by = join_by("COD_CHIP"=="cod_habilitacion"), keep = FALSE) %>% 
  filter(VIGENCIA > 2015)

matrix_cuartiles <- matrix_data %>%
  group_by(nivel, VIGENCIA, caracter) %>%
  mutate(across(starts_with("ascendente"), 
                list(Cuartil = ~ ntile(desc(.), 4)), # Ascendente con cuartiles inversos
                .names = "{.col}_cuartil")) %>%
  mutate(across(starts_with("descendente"), 
                list(Cuartil = ~ ntile(., 4)), # Descendente con cuartiles estándar
                .names = "{.col}_cuartil")) %>%
  ungroup() %>%
  rowwise() %>%
  #rename(test60 = var60_cuartil) %>%
  mutate(
    #test60 = if_else(var60 <= 1, 1, 4),
    Promedio_Cuartil = mean(c_across(ends_with("cuartil")), na.rm = TRUE)
  )

matrix_cuartiles <- matrix_cuartiles %>%
  group_by(nivel, VIGENCIA, caracter) %>%
  mutate(Nuevo_Riesgo_Cuartil = ntile(Promedio_Cuartil, 4),
         Nuevo_Riesgo = case_when(
           Nuevo_Riesgo_Cuartil == 1 ~ "Sin riesgo",
           Nuevo_Riesgo_Cuartil == 2 ~ "Riesgo bajo",
           Nuevo_Riesgo_Cuartil == 3 ~ "Riesgo medio",
           Nuevo_Riesgo_Cuartil == 4 ~ "Riesgo alto"
         )) %>%
  ungroup()

# Tabla de resumen de resultados**

mx_cuartil_resumen <- matrix_cuartiles %>% 
  select(VIGENCIA, COD_CHIP, nombre_ese,nivel, caracter,Promedio_Cuartil, Nuevo_Riesgo_Cuartil, Nuevo_Riesgo)

mx_cuartil_tabla2 <- mx_cuartil_resumen[complete.cases(mx_cuartil_resumen), ]

#-------------------------------------------------------------------------------

# Tabla para combinar los dos periodos

mx_cuartil_tabla <- bind_rows(mx_cuartil_tabla1, mx_cuartil_tabla2)

# Gráfico de caja Nuevos Riesgos

na_summary <- sapply(mx_cuartil_tabla, function(col) sum(is.na(col)))

na_columns <- na_summary[na_summary > 0]

# Figura Año / Nivel

figura3 <- ggplot(mx_cuartil_tabla, aes(x = factor(VIGENCIA), y = Promedio_Cuartil)) +
  geom_boxplot(fill = "#ABABAB", color = "black") +
  labs(title = "Gráfico de Caja: Distribución de Promedio Cuartiles por Año",
       x = "Año",
       y = "Promedio") +
  theme_minimal() +
  facet_wrap(~ nivel)

figura3

# Figura Año

figura4 <- ggplot(mx_cuartil_tabla, aes(x = factor(VIGENCIA), y = Promedio_Cuartil)) +
  geom_boxplot(aes(fill = factor(..group..)), color = "black") +
  labs(
    title = "Gráfico de Caja: Distribución de Promedio Cuartiles por Año",
    x = "Año",
    y = "Cuartil de Riesgo"
  ) +
  scale_fill_grey(start = 0.4, end = 0.8) +  
  theme_minimal() +
  coord_flip() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(), 
    plot.title = element_text(size = 9),
    axis.title.x = element_text(size = 9),  
    axis.title.y = element_text(size = 9),  
    axis.text = element_text(size = 9)
  )

figura4

# Generamos el histórico de riesgos

historico_resumen <- historico %>%  
  select(vigencia, cod_habilitacion,riesgo_austado)

mx_cuartil_tabla <- mx_cuartil_tabla %>% 
  left_join(historico_resumen, by = c("COD_CHIP"="cod_habilitacion","VIGENCIA"="vigencia")) %>% 
  rename(Riesgo_Anterior = riesgo_austado)

mx_cuartil_tabla <- mx_cuartil_tabla[complete.cases(mx_cuartil_tabla), ]

# Comparamos e identificamos casos diferentes

# Tabla con la comparación de casos**

write.table(mx_cuartil_tabla, file = "comparación.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Tabla agrupada de comparación de casos**

riesgos_diferentes2 <- table( 
  mx_cuartil_tabla$VIGENCIA, mx_cuartil_tabla$Nuevo_Riesgo, mx_cuartil_tabla$Riesgo_Anterior) %>%  
  as.data.frame() %>% 
  rename(Vigencia = Var1, Nuevo_Riesgo = Var2, Riesgo_Anterior = Var3, Total = Freq)

nuevo_riesgo <- table(mx_cuartil_tabla$VIGENCIA, mx_cuartil_tabla$nivel, mx_cuartil_tabla$Nuevo_Riesgo) %>%  
  as.data.frame() %>% 
  mutate(Grupo = "Nuevo Riesgo")

riesgo_anterior <- table(mx_cuartil_tabla$VIGENCIA, mx_cuartil_tabla$nivel,mx_cuartil_tabla$Riesgo_Anterior) %>%  
  as.data.frame() %>% 
  mutate(Grupo = "Riesgo Anterior")

# Tabla comparativa de las clasificaciones

riesgo_combinado <- bind_rows(nuevo_riesgo, riesgo_anterior) %>% 
  rename(VIGENCIA=Var1, Nivel=Var2, Riesgo=Var3, Total=Freq)

# Tabla y Figura de Riesgos Alto/Medio Antes/Ahora

riesgo_alto_medio <- riesgo_combinado %>% 
  filter(Riesgo == "Riesgo alto"| Riesgo == "Riesgo medio" ) %>%  
  group_by(VIGENCIA, Grupo, Nivel) %>% 
  summarise(Total = sum(Total))

figuras_list <- list()

niveles <- unique(riesgo_alto_medio$Nivel)

for (nivel in niveles) {
  datos_nivel <- riesgo_alto_medio %>% filter(Nivel == nivel)
  
  figura_nivel <- ggplot(datos_nivel, aes(x = VIGENCIA, y = Total, color = Grupo, group = Grupo)) + 
    geom_line(size = 1) + 
    geom_point(size = 2) + 
    labs(
      title = paste("Comparación Riesgo Alto/Medio - Nivel", nivel),
      x = "Vigencia",
      y = "Total ESE",
      color = "Grupo"
    ) +
    theme_minimal()
  
  figuras_list[[paste0("FiguraNivel", nivel)]] <- figura_nivel
}

# Mostrar las figuras almacenadas

print(figuras_list[["FiguraNivel1"]])
print(figuras_list[["FiguraNivel2"]])
print(figuras_list[["FiguraNivel3"]])

# Figuras por año

colores <- c("Nuevo Riesgo" = "#54BDC2", "Riesgo Anterior" = "#F88570")

riesgo_combinado_vigencia <- riesgo_combinado %>%  
  filter(VIGENCIA == "2019")

figura_riesgos_2019 <- ggplot(riesgo_combinado_vigencia, aes(x = Riesgo, y = Total, fill = Grupo)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = colores) + # Definir los colores para cada grupo
  labs(title = "Comparación de Categorías de Riesgos 2019",
       x = "Categoría de Riesgo",
       y = "Frecuencia",
       fill = "Grupo de Datos") +
  theme_minimal() +
  theme(
    text = element_text(size = 12), 
    axis.title = element_text(face = "bold"), 
    axis.title.x = element_text(margin = margin(t = 12)), 
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.title = element_text(face = "bold")
  )

#figura_riesgos_2012
#figura_riesgos_2013
#figura_riesgos_2014
#figura_riesgos_2015
#figura_riesgos_2016
#figura_riesgos_2017
#figura_riesgos_2018
#figura_riesgos_2019

# Tabla y gráfico de Riesgos promedio Total en todo el periodo------------------

tabla_promedio_1 <- mx_cuartil_tabla %>% 
  select(VIGENCIA, Nuevo_Riesgo, COD_CHIP) %>% 
  rename(Riesgo = Nuevo_Riesgo) %>% 
  group_by(VIGENCIA, Riesgo) %>% 
  summarise(Total = n()) %>% 
  group_by(Riesgo) %>% 
  summarise(promedio = mean(Total)) %>% 
  select(promedio, Riesgo) %>% 
  mutate(modelo = "Nueva")

tabla_promedio_2 <- mx_cuartil_tabla %>% 
  select(VIGENCIA, Riesgo_Anterior, COD_CHIP) %>% 
  rename(Riesgo = Riesgo_Anterior) %>% 
  group_by(VIGENCIA, Riesgo) %>% 
  summarise(Total = n()) %>% 
  group_by(Riesgo) %>% 
  summarise(promedio = mean(Total)) %>% 
  select(promedio, Riesgo) %>% 
  mutate(modelo = "Anterior")

tabla_promedio <- rbind(tabla_promedio_1, tabla_promedio_2)
write.table(tabla_promedio, file = "tabla_promedio.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

figura_promedio_anual <- ggplot(tabla_promedio, aes(x = Riesgo, y = promedio, fill = modelo)) + 
  geom_bar(stat = "identity", position = "dodge") +  
  geom_text(aes(label = round(promedio, 2)), vjust = -0.5, position = position_dodge(1), size = 3.5, color = "black") +
  scale_fill_manual(values = c("Nueva" = "#86CAE1", "Anterior" = "#A0A6A7")) + 
  labs(title = "Promedio anual de ESE por categoría de riesgo",
       fill = "Metodología", y = "Promedio anual", x = "Grupo Riesgo") +  
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "white", color = "white"),
    legend.position = "right",
    text = element_text(size = 10, color = "black"), 
    axis.title = element_text(size = 10, color = "black", face = "bold"),  
    axis.text = element_text(size = 10, color = "black"),   
    plot.title = element_text(size = 10, color = "black", face = "bold"),  
    legend.text = element_text(size = 10, color = "black"),  
    legend.title = element_text(size = 10, color = "black")  
  )
  
figura_promedio_anual

# Tabla de matrix con Riesgos Nuevo/Actual--------------------------------------

cambios_riesgos <- read_delim("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/R/cambios_riesgos.txt", 
                                   delim = "\t", escape_double = FALSE,
                                   trim_ws = TRUE)

panel_final <- matrix %>% 
  left_join(mx_cuartil_tabla, by = c("COD_CHIP"="COD_CHIP","VIGENCIA"="VIGENCIA")) %>% 
  filter(!is.na(nombre_ese)) %>% 
  left_join(cambios_riesgos, by = join_by("Nuevo_Riesgo", "Riesgo_Anterior"))

table(panel_final$Cambio, useNA = "always") # Esperamos NA=0

table(panel_final$Nuevo_Riesgo, panel_final$Riesgo_Anterior)

write.table(panel_final, file = "panel_matrix.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
