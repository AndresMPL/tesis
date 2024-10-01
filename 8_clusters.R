
# Modelo Clusters 100%----------------------------------------------------------

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

# Si hay más de dos variables, realizar PCA para reducir a dos componentes principales
if (ncol(matrix_filtered_norm) > 2) {
  pca_result <- prcomp(matrix_filtered_norm)
  matrix_filtered_pca <- as.data.frame(pca_result$x[, 1:2])  # Usar las dos primeras componentes
  colnames(matrix_filtered_pca) <- c("PC1", "PC2")
} else {
  matrix_filtered_pca <- as.data.frame(matrix_filtered_norm)
  colnames(matrix_filtered_pca) <- c("V1", "V2")  # Cambiar nombres a V1 y V2
}

# Asignar etiquetas de cluster a la matriz PCA filtrada
matrix_filtered_pca$Riesgo <- factor(kmeans_result_filtered$cluster, labels = c("Sin Riesgo", "Bajo", "Medio", "Alto"))

# Graficar los resultados con ggplot y añadir los círculos de cada clúster
ggplot(matrix_filtered_pca, aes(x = PC1, y = PC2, fill = Riesgo)) +
  geom_point(size = 3, shape = 21, color = "black", stroke = 1) +  # shape = 21 permite color de contorno y relleno
  stat_ellipse(aes(group = Riesgo, fill = Riesgo), 
               type = "norm", 
               geom = "polygon",  # Cambia la forma de la elipse a un polígono para el relleno
               alpha = 0.2,  # Ajusta la transparencia del relleno
               color = "black",  # Contorno negro para las elipses
               size = 1) +  # Grosor del contorno de la elipse
  theme_minimal() +
  labs(title = "Clustering de Riesgo con K-means (sin outliers)",
       x = "Componente 1", y = "Componente 2") +
  scale_color_manual(values = c("#65CCFF", "#99FF99", "#FFCC65", "#F76D5E")) +  # Color de las elipses y puntos
  scale_fill_manual(values = c("#65CCFF", "#99FF99", "#FFCC65", "#F76D5E"))  # Relleno de las elipses

# Graficar los resultados sin outliers
plot(matrix_filtered_norm, col = kmeans_result_filtered$cluster)
legend("topright", legend = levels(matrix_filtered$Riesgo), fill = 1:4)

# Opcional: mostrar datos eliminados
outliers <- matrix[distances > threshold, ]
print("Datos atípicos eliminados:")
print(outliers)

