
#--------------Histograma

# Medidas para el Histograma: W=260, H=232

# Importar el archivo histograma

setwd("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/R")
histograma_todo <- read_excel("histograma.xlsx")
histograma_riesgo <- histograma %>% filter(histograma$total != 0)

# Resultados en un dataframe

count_df <- as.data.frame(table(histograma$total))

# Figura 1

figura1 <- ggplot(data = histograma_riesgo, mapping = aes(x = total))  + 
      geom_histogram(aes(y =after_stat(count)),
                 bins = nrow(count_df),
                 position = 'identity',
                 color="#424242", fill="#E3E3E3") +
    labs(
      #title = 'Figura: Distribución de años en Riesgo Alto/Medio',
       x = 'Años en Riesgo Alto o Medio',
       y = 'Número de ESE') + 
  geom_text(stat = "count", aes(label = stat(count)), position = position_stack(vjust = 0.5), size =3) +
  scale_x_continuous(breaks = seq(min(histograma_riesgo$total), max(histograma_riesgo$total), by = 1)) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 8),  # Tamaño de las etiquetas en los ejes
    axis.title.x = element_text(size = 8),  # Tamaño de la etiqueta del eje x
    axis.title.y = element_text(size = 8)  # Tamaño de la etiqueta del eje y
  )

figura1

# Figura 2

figura2 <- ggplot(data = histograma_riesgo, mapping = aes(x = total))  + 
  geom_histogram(aes(y =after_stat(density)),
                 bins = nrow(count_df),
                 position = 'identity',
                 color="#424242", fill="#F9F9F9") +
  stat_function(fun = dnorm, xlim = c(min(histograma_riesgo$total),max(histograma_riesgo$total)), colour="#65707A", linewidth=1,
                args = list(mean = mean(histograma_riesgo$total), 
                            sd = sd(histograma_riesgo$total))) + 
  labs(
    #title = 'Figura: Distribución de años en Riesgo Alto/Medio',
    x = 'Años en Riesgo Alto/Medio',
    y = 'Densidad de Probabilidad') + 
  scale_x_continuous(breaks = seq(min(histograma_riesgo$total), max(histograma_riesgo$total), by = 1)) +
  geom_vline(xintercept = mean(histograma_riesgo$total), linetype = "dashed", color = "#3E3E3E", linewidth = 1) + 
  theme_bw() +
  theme(
    axis.text = element_text(size = 8),  # Tamaño de las etiquetas en los ejes
    axis.title.x = element_text(size = 8),  # Tamaño de la etiqueta del eje x
    axis.title.y = element_text(size = 8)  # Tamaño de la etiqueta del eje y
  )

figura2