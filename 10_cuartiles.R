
# Método Cuartiles--------------------------------------------------------------

# Listado con el nombre de variables SELECCIONADAS
# Evaluamos variables diferentes para el periodo 2012 a 2015 y 2016 a 2019

nombres_variables_15 <- read_delim("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/R/variables15.txt", 
                                delim = "\t", escape_double = FALSE,
                                trim_ws = TRUE) %>% select(variable)

nombres_variables_15 <- nombres_variables_15 %>% 
  add_row(variable="VIGENCIA") %>% 
  add_row(variable="COD_CHIP")
nombres_variables_15 <- nombres_variables_15$variable


nombres_variables_19 <- read_delim("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/R/variables19.txt", 
                                   delim = "\t", escape_double = FALSE,
                                   trim_ws = TRUE) %>% select(variable)

nombres_variables_19 <- nombres_variables_19 %>% 
  add_row(variable="VIGENCIA") %>% 
  add_row(variable="COD_CHIP")
nombres_variables_19 <- nombres_variables_19$variable


#-------------------------------------------------------------------------------
# Análisis 2012 a 2015

matrix_data <- matrix %>% select(all_of(nombres_variables_15))
matrix_data <- matrix_data %>% left_join(lista_ese, by = join_by("COD_CHIP"=="cod_habilitacion"), keep = FALSE) %>% 
  filter(VIGENCIA < 2016)

matrix_cuartiles <- matrix_data %>%
  group_by(nivel, VIGENCIA) %>%
  mutate(across(starts_with("var"),            
                list(Cuartil = ~ ntile(., 4)),
                .names = "{.col}_cuartil")) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(Promedio_Cuartil = mean(c_across(ends_with("cuartil")), na.rm = TRUE)) 

matrix_cuartiles <- matrix_cuartiles %>%
  group_by(nivel, VIGENCIA) %>%
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

matrix_data <- matrix %>% select(all_of(nombres_variables_19))
matrix_data <- matrix_data %>% left_join(lista_ese, by = join_by("COD_CHIP"=="cod_habilitacion"), keep = FALSE) %>% 
  filter(VIGENCIA > 2015)

matrix_cuartiles <- matrix_data %>%
  group_by(nivel, VIGENCIA) %>%
  mutate(across(starts_with("var"),            
                list(Cuartil = ~ ntile(., 4)),
                .names = "{.col}_cuartil")) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(Promedio_Cuartil = mean(c_across(ends_with("cuartil")), na.rm = TRUE)) 

matrix_cuartiles <- matrix_cuartiles %>%
  group_by(nivel, VIGENCIA) %>%
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

mx_cuartil_tabla <- bind_rows(mx_cuartil_tabla1, mx_cuartil_tabla2) # Aquí juntamos los dos periodos evaluados

#-------------------------------------------------------------------------------
# Gráfico de caja Nuevos Riesgos

na_summary <- sapply(mx_cuartil_tabla, function(col) sum(is.na(col)))
na_columns <- na_summary[na_summary > 0]
print(na_columns)

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
  #geom_jitter(width = 0.1, size = 1, color = "black", alpha = 0.6) +  # Añadir puntos de datos
  labs(
    title = "Gráfico de Caja: Distribución de Promedio Cuartiles por Año",
    x = "Año",
    y = "Promedio"
  ) +
  scale_fill_grey(start = 0.4, end = 0.8) +  # Escala de grises sin nombre
  theme_minimal() +
  coord_flip() +  # Gráfico horizontal
  theme(
    legend.position = "none"  # Eliminar la leyenda
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

# Tabla con los casos específicamente diferentes**

riesgos_diferentes1 <- mx_cuartil_tabla %>%  
  filter(Nuevo_Riesgo != Riesgo_Anterior) %>% 
  select(VIGENCIA, COD_CHIP, nombre_ese, nivel, caracter,Nuevo_Riesgo, Riesgo_Anterior)

write.table(riesgos_diferentes1, file = "comparación.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Tabla agrupada de comparación de casos**

riesgos_diferentes2 <- table( 
  mx_cuartil_tabla$VIGENCIA, mx_cuartil_tabla$Nuevo_Riesgo, mx_cuartil_tabla$Riesgo_Anterior) %>%  
  as.data.frame() %>% 
  rename(Vigencia = Var1, Nuevo_Riesgo = Var2, Riesgo_Anterior = Var3, Total = Freq)

nuevo_riesgo <- table(mx_cuartil_tabla$VIGENCIA, mx_cuartil_tabla$nivel, mx_cuartil_tabla$Nuevo_Riesgo) %>%  as.data.frame() %>% mutate(Grupo = "Nuevo Riesgo")
riesgo_anterior <- table(mx_cuartil_tabla$VIGENCIA, mx_cuartil_tabla$nivel,mx_cuartil_tabla$Riesgo_Anterior) %>%  as.data.frame() %>% mutate(Grupo = "Riesgo Anterior")

# Gráfico comparativo de las clasificaciones

riesgo_combinado <- bind_rows(nuevo_riesgo, riesgo_anterior) %>% rename(VIGENCIA=Var1, Nivel=Var2, Riesgo=Var3, Total=Freq)

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

riesgo_combinado_vigencia <- bind_rows(nuevo_riesgo, riesgo_anterior) %>% rename(VIGENCIA=Var1, Nivel=Var2, Riesgo=Var3, Total=Freq)
riesgo_combinado_vigencia <- riesgo_combinado_vigencia %>%  filter(VIGENCIA == "2012")
colores <- c("Nuevo Riesgo" = "#54BDC2", "Riesgo Anterior" = "#F88570")

figura_riesgos_2012 <- ggplot(riesgo_combinado, aes(x = Riesgo, y = Total, fill = Grupo)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = colores) + # Definir los colores para cada grupo
  labs(title = "Comparación de Categorías de Riesgos",
       x = "Categoría de Riesgo",
       y = "Frecuencia",
       fill = "Grupo de Datos") +
  theme_minimal() +
  theme(
    text = element_text(size = 12), # Establecer la fuente y el tamaño de todo el gráfico
    axis.title = element_text(face = "bold"), # Hacer los títulos de los ejes en negrita
    axis.title.x = element_text(margin = margin(t = 12)), # Agregar margen superior al título del eje X
    axis.title.y = element_text(margin = margin(r = 12)), # Agregar margen derecho al título del eje Y
    legend.title = element_text(face = "bold") # Hacer el título de la leyenda en negrita
  )
