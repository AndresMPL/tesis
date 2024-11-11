
# Pruebas Modelos---------------------------------------------------------------

# Ordeno las columnas del archivo Panel Matrix

cols_var <- grep("^var", names(panel_final), value = TRUE)
cols_non_var <- setdiff(names(panel_final), cols_var)
panel_anova <- panel_final[, c(cols_non_var, cols_var)]

risk_mapping <- c("Sin riesgo" = 0, "Riesgo bajo" = 1, "Riesgo medio" = 2, "Riesgo alto" = 3)
panel_anova$Riesgo_Anterior_Num <- as.numeric(risk_mapping[panel_anova$Riesgo_Anterior])
panel_anova$Nuevo_Riesgo_Num <- as.numeric(risk_mapping[panel_anova$Nuevo_Riesgo])

# Lista para almacenar resultados
anova_results <- data.frame(Indicador = character(),
                            P_Anterior = numeric(),
                            P_Nuevo = numeric(),
                            stringsAsFactors = FALSE)

# Seleccionar los indicadores para el análisis
indicadores <- panel_anova %>% select(-(1:7), -Riesgo_Anterior, -Nuevo_Riesgo, -Riesgo_Anterior_Num, -Nuevo_Riesgo_Num)

# Iterar sobre cada indicador y realizar ANOVA para Riesgo Anterior y Nuevo Riesgo
for (indicador in colnames(indicadores)) {
  # Fórmulas para Riesgo Anterior y Nuevo Riesgo
  formula_anterior <- as.formula(paste(indicador, "~ Riesgo_Anterior_Num"))
  formula_nuevo <- as.formula(paste(indicador, "~ Nuevo_Riesgo_Num"))
  
  # ANOVA para Riesgo Anterior
  anova_anterior <- aov(formula_anterior, data = panel_anova)
  p_anterior <- summary(anova_anterior)[[1]]$`Pr(>F)`[1]
  
  # ANOVA para Nuevo Riesgo
  anova_nuevo <- aov(formula_nuevo, data = panel_anova)
  p_nuevo <- summary(anova_nuevo)[[1]]$`Pr(>F)`[1]
  
  # Agregar los resultados a la tabla
  anova_results <- rbind(anova_results, data.frame(Indicador = indicador,
                                                   P_Anterior = p_anterior,
                                                   P_Nuevo = p_nuevo))
}

# Mostrar los resultados de ANOVA
print(anova_results)