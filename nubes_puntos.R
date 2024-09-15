
library(pacman)
p_load(readr, dplyr, ggplot2)
options(scipen = 999)

#-------Figura Recaudo vs. Compromisos-----

recaudo_compromisos <- read_delim("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/R/recaudo.txt", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE)

recaudo_compromisos <- subset(recaudo_compromisos, riesgo_original != "#N/D") %>% 
                       subset(riesgo_original == "Riesgo alto")

table(recaudo_compromisos$riesgo_original) %>% as.data.frame()

mean_values <- recaudo_compromisos %>% group_by(ano) %>%
               summarize(mean_porcent = mean(porc_reca_comp))


figura1 <- ggplot(data = recaudo_compromisos, aes(x = ano, y = porc_reca_comp)) +
  geom_point(color = "#4273A1") +
  geom_smooth(method = "lm", aes(x = ano, y = mean_porcent), data = mean_values, color = "#D55D22", se = FALSE) +
  labs(title = "Porcentaje de recursos recaudados y comprometidos",
       x = "Vigencia",
       y = "Porcentaje Compromisos vs. Recaudo",
       color = "codigo_habilitacion") +
  theme_minimal()

figura1

