
# Histograma--------------------------------------------------------------------

# Medidas para el Histograma: W=260, H=232

# Importar el archivo histograma

histograma <- read_excel("histograma.xlsx")
histograma_riesgo <- histograma %>% filter(histograma$total != 0)

# Histograma en un df

count_df <- as.data.frame(table(histograma$total))

# Figura 1 - Histograma y distribucion probabilidad

figura1 <- ggplot(data = histograma_riesgo, mapping = aes(x = total))  + 
  geom_histogram(aes(y =after_stat(density)),
                 bins = nrow(count_df),
                 position = 'identity',
                 color="#424242", fill="#F9F9F9") +
  stat_function(fun = dnorm, xlim = c(min(histograma_riesgo$total),max(histograma_riesgo$total)), colour="#65707A", linewidth=1,
                args = list(mean = mean(histograma_riesgo$total), 
                            sd = sd(histograma_riesgo$total))) + 
  labs(
    title = 'Figura: Distribucion de vigencias en Riesgo Alto/Medio',
    x = "Vigencias",
    y = "Densidad") + 
  scale_x_continuous(breaks = seq(min(histograma_riesgo$total), max(histograma_riesgo$total), by = 1)) +
  geom_vline(xintercept = mean(histograma_riesgo$total), linetype = "dashed", color = "#3E3E3E", linewidth = 1) + 
  theme_bw() +
  theme(
    axis.text = element_text(size = 8),  
    axis.title.x = element_text(size = 8),  
    axis.title.y = element_text(size = 8) 
  )

figura1

# ESE que han estado siempre en Riesgo Alto/Medio

todo_riesgo <- histograma_riesgo %>%  filter(total == 8)
