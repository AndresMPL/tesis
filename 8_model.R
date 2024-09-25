
# Modelo Clusters---------------------------------------------------------------

nombres_cluster <- as.data.frame(variable.names(matrix)) # Listado con el nombre de TODAS las variables
names(nombres_cluster) <- "variable"
nombres_cluster <- nombres_cluster[-c(1, 2), ]

panel_norm <- scale(matrix[, nombres_cluster])

set.seed(123)  # Para aleatoriedad
kmeans_result <- kmeans(panel_norm, centers = 4, nstart = 25)

matrix$Riesgo <- factor(kmeans_result$cluster, labels = c("Sin Riesgo", "Bajo", "Medio", "Alto"))

plot(panel_norm, col = kmeans_result$cluster)
legend("topright", legend = levels(matrix$Riesgo), fill = 1:4)

# Modelo Clusters 91%-----------------------------------------------------------

# Normalizar los datos
panel_norm <- scale(matrix[, nombres_cluster])

# Aleatoriedad
set.seed(123)

# Clustering con k-means
kmeans_result <- kmeans(panel_norm, centers = 4, nstart = 25)

# Etiquetas de cluster a la matriz original
matrix$Riesgo <- factor(kmeans_result$cluster, labels = c("Sin Riesgo", "Bajo", "Medio", "Alto"))

# Calcular las distancias de cada punto a su centroide correspondiente
distances <- sapply(1:nrow(panel_norm), function(i) {
  cluster_idx <- kmeans_result$cluster[i]
  sum((panel_norm[i, ] - kmeans_result$centers[cluster_idx, ])^2)
})

# Umbral para eliminar outliers, como el percentil 90
threshold <- quantile(distances, 0.91)

# Filtrar los datos para eliminar outliers
matrix_filtered <- matrix[distances <= threshold, ]

# Verificar si existen valores NA, NaN o Inf y eliminarlos en cada paso
# Remover filas con valores NA
matrix_filtered <- na.omit(matrix_filtered)

# Remover filas con valores NaN o Inf
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
matrix_filtered <- matrix_filtered[!rowSums(is.nan.data.frame(matrix_filtered)), ]

is.infinite.data.frame <- function(x) do.call(cbind, lapply(x, is.infinite))
matrix_filtered <- matrix_filtered[!rowSums(is.infinite.data.frame(matrix_filtered)), ]

# Escalar nuevamente los datos filtrados
matrix_filtered_norm <- scale(matrix_filtered[, nombres_cluster])

# Comprobar si existen NA/NaN/Inf después del escalado
if (any(!complete.cases(matrix_filtered_norm))) {
  stop("Todavía hay valores NA/NaN/Inf después del escalado. Revisa los datos.")
}

# Realizar clustering en los datos filtrados
kmeans_result_filtered <- kmeans(matrix_filtered_norm, centers = 4, nstart = 25)

# Asignar etiquetas de cluster a la matriz filtrada
matrix_filtered$Riesgo <- factor(kmeans_result_filtered$cluster, labels = c("Sin Riesgo", "Bajo", "Medio", "Alto"))

# Graficar los resultados sin outliers
plot(matrix_filtered_norm, col = kmeans_result_filtered$cluster)
legend("topright", legend = levels(matrix_filtered$Riesgo), fill = 1:4)

# Opcional: mostrar datos eliminados
outliers <- matrix[distances > threshold, ]
print("Datos atípicos eliminados:")
print(outliers)


# Método Cuartiles--------------------------------------------------------------

# Listado con el nombre de variables SELECCIONADAS

nombres_variables <- read_delim("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/R/variables.txt", 
                     delim = "\t", escape_double = FALSE,
                     trim_ws = TRUE) %>% select(variable)

nombres_variables <- nombres_variables %>% 
  add_row(variable="VIGENCIA") %>% 
  add_row(variable="COD_CHIP")

nombres_variables <- nombres_variables$variable

matrix_data <- matrix %>% select(all_of(nombres_variables))

matrix_data <- matrix_data %>% left_join(lista_ese, by = join_by("COD_CHIP"=="cod_habilitacion"), keep = FALSE)

matrix_cuartiles <- matrix_data %>%
  group_by(nivel, VIGENCIA) %>%  # Agrupar por el nivel del hospital
  mutate(across(starts_with("var"),  # Para cada variable (todas las variables comienzan con "var")
                list(Cuartil = ~ ntile(., 4)), # Calcular cuartiles
                .names = "{.col}_cuartil")) %>% # Crear nuevas columnas con el nombre de la variable original + "_cuartil"
  ungroup() %>%
  rowwise() %>%
  mutate(Promedio_Cuartil = mean(c_across(ends_with("cuartil")), na.rm = TRUE)) # Calcular el promedio de los cuartiles
  
matrix_cuartiles <- matrix_cuartiles %>%
  group_by(nivel, VIGENCIA) %>%
  mutate(Nuevo_Riesgo_Cuartil = ntile(Promedio_Cuartil, 4), # Calcular cuartiles del promedio de cuartiles
         Nuevo_Riesgo = case_when( # Asignar etiquetas de riesgo
           Nuevo_Riesgo_Cuartil == 1 ~ "Sin riesgo",
           Nuevo_Riesgo_Cuartil == 2 ~ "Riesgo bajo",
           Nuevo_Riesgo_Cuartil == 3 ~ "Riesgo medio",
           Nuevo_Riesgo_Cuartil == 4 ~ "Riesgo alto"
         )) %>%
  ungroup()

# Tabla de resumen de resultados**

mx_cuartil_resumen <- matrix_cuartiles %>% 
  select(VIGENCIA, COD_CHIP, nombre_ese,nivel, caracter,Promedio_Cuartil, Nuevo_Riesgo_Cuartil, Nuevo_Riesgo)

mx_cuartil_resumen <- mx_cuartil_resumen[complete.cases(mx_cuartil_resumen), ]

# Gráfico de caja Nuevos Riesgos

na_summary <- sapply(mx_cuartil_resumen, function(col) sum(is.na(col)))
na_columns <- na_summary[na_summary > 0]
print(na_columns)

# Figura Año / Nivel
figura3 <- ggplot(mx_cuartil_resumen, aes(x = factor(VIGENCIA), y = Promedio_Cuartil)) +
  geom_boxplot(fill = "#ABABAB", color = "black") +
  labs(title = "Gráfico de Caja: Distribución de Promedio Cuartiles por Año",
       x = "Año",
       y = "Promedio") +
  theme_minimal() +
  facet_wrap(~ nivel)

figura3

# Figura Año
figura4 <- ggplot(mx_cuartil_resumen, aes(x = factor(VIGENCIA), y = Promedio_Cuartil)) +
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

mx_cuartil_resumen <- mx_cuartil_resumen %>% 
  left_join(historico_resumen, by = c("COD_CHIP"="cod_habilitacion","VIGENCIA"="vigencia")) %>% 
  rename(Riesgo_Anterior = riesgo_austado)

mx_cuartil_resumen <- mx_cuartil_resumen[complete.cases(mx_cuartil_resumen), ]

# Comparamos e identificamos casos diferentes

# Tabla con los casos específicamente diferentes**

riesgos_diferentes1 <- mx_cuartil_resumen %>%  
  filter(Nuevo_Riesgo != Riesgo_Anterior) %>% 
  select(VIGENCIA, COD_CHIP, nombre_ese, nivel, caracter,Nuevo_Riesgo, Riesgo_Anterior)

# Tabla agrupada de comparación de casos**

riesgos_diferentes2 <- table( 
  mx_cuartil_resumen$VIGENCIA, mx_cuartil_resumen$Nuevo_Riesgo, mx_cuartil_resumen$Riesgo_Anterior) %>%  
  as.data.frame() %>% 
  rename(Vigencia = Var1, Nuevo_Riesgo = Var2, Riesgo_Anterior = Var3, Total = Freq)

nuevo_riesgo <- table(mx_cuartil_resumen$VIGENCIA, mx_cuartil_resumen$nivel, mx_cuartil_resumen$Nuevo_Riesgo) %>%  as.data.frame() %>% mutate(Grupo = "Nuevo Riesgo")
riesgo_anterior <- table(mx_cuartil_resumen$VIGENCIA, mx_cuartil_resumen$nivel,mx_cuartil_resumen$Riesgo_Anterior) %>%  as.data.frame() %>% mutate(Grupo = "Riesgo Anterior")

# Gráfico comparativo de las clasificaciones

riesgo_combinado <- bind_rows(nuevo_riesgo, riesgo_anterior) %>% rename(VIGENCIA=Var1, Nivel=Var2, Riesgo=Var3, Total=Freq)

riesgo_combinado_vigencia <- bind_rows(nuevo_riesgo, riesgo_anterior) %>% rename(VIGENCIA=Var1, Nivel=Var2, Riesgo=Var3, Total=Freq)
riesgo_combinado_vigencia <- riesgo_combinado_vigencia %>%  filter(VIGENCIA == "2012")
colores <- c("Nuevo Riesgo" = "#54BDC2", "Riesgo Anterior" = "#F88570")

figura_riesgos_2012 <- ggplot(riesgo_combinado_vigencia, aes(x = Riesgo, y = Total, fill = Grupo)) +
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

# Figuras por año



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

