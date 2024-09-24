
# Modelo Clusters---------------------------------------------------------------

# Listado con el nombre de las variables
nombres_variables <- as.data.frame(variable.names(matrix))
names(nombres_variables) <- "variable"
nombres_variables <- nombres_variables[-c(1, 2), ]

panel_norm <- scale(matrix[, nombres_variables])

set.seed(123)  # Para aleatoriedad
kmeans_result <- kmeans(panel_norm, centers = 4, nstart = 25)

matrix$Riesgo <- factor(kmeans_result$cluster, labels = c("Sin Riesgo", "Bajo", "Medio", "Alto"))

plot(panel_norm, col = kmeans_result$cluster)
legend("topright", legend = levels(matrix$Riesgo), fill = 1:4)


# Método Cuartiles--------------------------------------------------------------

matrix_data <- matrix %>% left_join(lista_ese, by = join_by("COD_CHIP"=="cod_habilitacion"), keep = FALSE)


matrix_cuartiles <- matrix_data %>%
  group_by(nivel) %>%  # Agrupar por el nivel del hospital
  mutate(across(starts_with("var"),  # Para cada variable (todas las variables comienzan con "var")
                list(Cuartil = ~ ntile(., 4)), # Calcular cuartiles
                .names = "{.col}_cuartil")) %>% # Crear nuevas columnas con el nombre de la variable original + "_cuartil"
  ungroup() %>%
  rowwise() %>%
  mutate(Promedio_Cuartil = mean(c_across(ends_with("cuartil")), na.rm = TRUE)) # Calcular el promedio de los cuartiles
  
matrix_cuartiles <- matrix_cuartiles %>%
  group_by(nivel) %>%
  mutate(Riesgo_Cuartil = ntile(Promedio_Cuartil, 4), # Calcular cuartiles del promedio de cuartiles
         Cuartil_nombre = case_when( # Asignar etiquetas de riesgo
           Riesgo_Cuartil == 1 ~ "Sin riesgo",
           Riesgo_Cuartil == 2 ~ "Riesgo bajo",
           Riesgo_Cuartil == 3 ~ "Riesgo medio",
           Riesgo_Cuartil == 4 ~ "Riesgo alto"
         )) %>%
  ungroup()

mx_cuartil_resumen <- matrix_cuartiles %>% 
  select(VIGENCIA, COD_CHIP, nombre_ese,nivel, caracter,Promedio_Cuartil, Riesgo_Cuartil, Cuartil_nombre) # Seleccionar columnas relevantes

# Traemos el histórico de riesgos

historico_resumen <- historico %>%  select(vigencia, cod_habilitacion,riesgo_austado)
mx_cuartil_resumen <- mx_cuartil_resumen %>% 
  left_join(historico_resumen, by = c("COD_CHIP"="cod_habilitacion","VIGENCIA"="vigencia"))

# Comparamos e identificamos casos diferentes

riesgos_dif1 <- mx_cuartil_resumen %>% filter(Cuartil_nombre != riesgo_austado) %>% select(VIGENCIA, COD_CHIP, nombre_ese, nivel, caracter,Cuartil_nombre, riesgo_austado)

riesgos_dif2 <- table(mx_cuartil_resumen$VIGENCIA, mx_cuartil_resumen$Cuartil_nombre, mx_cuartil_resumen$riesgo_austado) %>%  
  as.data.frame() %>% 
  rename(Vigencia = Var1, Nuevo_Riesgo = Var2, Riesgo_Anterior = Var3)

nuevo_riesgo <- table(mx_cuartil_resumen$VIGENCIA, mx_cuartil_resumen$Cuartil_nombre) %>%  as.data.frame() %>% mutate(Grupo = "Nuevo Riesgo")
anterior_riesgo <- riesgo <- table(mx_cuartil_resumen$VIGENCIA,mx_cuartil_resumen$riesgo_austado) %>%  as.data.frame() %>% mutate(Grupo = "Anterior Riesgo")

riesgo_combinado <- bind_rows(nuevo_riesgo, anterior_riesgo)

riesgo_combinado_vigencia <- bind_rows(nuevo_riesgo, anterior_riesgo)
riesgo_combinado_vigencia <- riesgo_combinado_vigencia %>%  filter(Var1 == "2019")

# Gráfico comparativo de las clasificaciones------------------------------------

colores <- c("Nuevo Riesgo" = "#54BDC2", "Anterior Riesgo" = "#F88570")

figura_riesgos <- ggplot(riesgo_combinado, aes(x = Var2, y = Freq, fill = Grupo)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = colores) + # Definir los colores para cada grupo
  labs(title = "Comparación de Categorías de Riesgos 2019",
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

figura_riesgos
