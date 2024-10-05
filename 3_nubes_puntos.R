
# Figura Recaudo vs. Compromisos Alto/Medio Riesgo------------------------------

recaudo_compromisos <- read_delim("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/R/recaudo_compromisos.txt", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE)

recaudo_compromisos <- recaudo_compromisos %>% 
  filter(riesgo_original != "#N/D")

recaudo_compromisos <- recaudo_compromisos %>% 
  filter(riesgo_original == "Riesgo alto"|riesgo_original == "Riesgo medio")

recaudo_compromisos <- recaudo_compromisos %>% 
  select(-porc_reca_comp)

recaudo_compromisos$porcentaje <- recaudo_compromisos$compromiso/recaudo_compromisos$recaudo_total

recaudo_compromisos$ano <- as.factor(recaudo_compromisos$ano)

mean_values <- recaudo_compromisos %>% group_by(ano) %>%
               summarize(mean_porcent = mean(porcentaje))

figura2 <- ggplot(recaudo_compromisos, aes(x = ano, y = porcentaje, color = ano)) +
  geom_point(size = 3) + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", size = 1) +
  scale_color_brewer(palette = "Spectral") + 
  labs(title = "Compromisos vs. Recaudo - ESE en Riesgo Alto/Medio",
       x = "Vigencias",
       y = "Porcentaje",
       color = "ano") +
  theme_minimal() # Aplica un tema minimalista al grC!fico

figura2
