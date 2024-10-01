
# Model PCA---------------------------------------------------------------------

matrix_pca <- matrix %>% left_join(lista_ese, by = join_by("COD_CHIP"=="cod_habilitacion"), keep = FALSE)
matrix_pca <- matrix_pca %>%  select(-municipio, -nombre_ese, -caracter, -VIGENCIA)
matrix_pca[is.na(matrix_pca)] <- 0 

matrix_pca_tipo <- matrix_pca %>% select(nivel, COD_CHIP)
matrix_pca_variables <- matrix_pca %>% select(-nivel, -COD_CHIP)

matrix_pca_escalados <- scale(matrix_pca_variables)

pca_resultado <- prcomp(matrix_pca_escalados, center = TRUE, scale. = TRUE)

datos_pca <- as.data.frame(pca_resultado$x[, 1:2])

set.seed(123) 
kmeans_resultado <- kmeans(datos_pca, centers = 3, nstart = 25)
datos_pca$cluster <- as.factor(kmeans_resultado$cluster)

datos_pca <- cbind(matrix_pca_tipo, datos_pca)

ggplot(datos_pca, aes(x = PC1, y = PC2, fill = cluster, label = COD_CHIP)) +
  geom_point(size = 3, shape = 21, color = "black", stroke = 1) +  # Contorno negro
  stat_ellipse(aes(color = cluster), type = "norm", size = 1) +  # Elipses para cada cluster
  #geom_text(vjust = 1.5, hjust = 1.5) +
  theme_minimal() +
  labs(title = "Clustering de Hospitales basado en PCA",
       x = "Componente Principal 1",
       y = "Componente Principal 2",
       fill = "Cluster",
       color = "Cluster") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +  # Colores de relleno para cada cluster
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

pca_resultado$rotation

fviz_pca_biplot(pca_resultado, repel = TRUE, label = "var", habillage = matrix_pca$nivel)