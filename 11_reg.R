
# Modelo OLS--------------------------------------------------------------------

library(pacman)
p_load(readxl, dplyr, tidyr, caret, broom, glmnet, lmtest, sandwich, stargazer,
       officer, flextable)

cols_var <- grep("^var", names(panel_final), value = TRUE)
cols_no_var <- setdiff(names(panel_final), cols_var)
panel_ols <- panel_final[, c(cols_no_var, cols_var)]

panel_ols <- panel_ols %>% 
  select(VIGENCIA, COD_CHIP, nombre_ese, nivel, caracter, Nuevo_Riesgo, Riesgo_Anterior, Cambio, all_of(tabla_reportes$id)) %>% 
  mutate(Riesgo = if_else(var60 <= 1, "Sin Riesgo", "Riesgo Alto")) 

# Convertir variables de control en factores
panel_ols$VIGENCIA <- as.factor(panel_ols$VIGENCIA)
panel_ols$caracter <- as.factor(panel_ols$caracter)
panel_ols$nivel <- as.factor(panel_ols$nivel)
panel_ols$Riesgo <- as.factor(panel_ols$Riesgo)

# Definir la fórmula de la regresión lineal inicial
vars_columns <- grep("^var", names(panel_ols), value = TRUE)
independent_vars <- paste(vars_columns[vars_columns != "var60"], collapse = " + ")
formula_initial <- as.formula(paste("var60 ~", "VIGENCIA + caracter + nivel + ", independent_vars))

# Ajustar el modelo de regresión lineal
model_initial <- lm(formula_initial, data = panel_ols, na.action = na.exclude)

# Calcular los errores estándar robustos usando sandwich y lmtest
robust_se <- coeftest(model_initial, vcov = vcovHC(model_initial, type = "HC1"))

# Obtener la tabla de coeficientes

summary_initial <- summary(model_initial)
coef_table1 <- tidy(model_initial)

coef_table <- tidy(model_initial, conf.int = TRUE) %>%
  filter(term %in% rownames(robust_se)) %>%
  mutate(p.value = robust_se[, 4]) %>%
  mutate(Significancia = ifelse(p.value <= 0.05, "Significativa", "No Significativa"))

# Modelo inicial en LateX
stargazer(model_initial, type = "latex", se = list(robust_se[, 2]), 
          title = "Regresión Lineal con Errores Robustos", 
          dep.var.labels = "var60", 
          covariate.labels = c(vars_columns[vars_columns != "var60"], "VIGENCIA", "caracter", "nivel"),
          out = "modelo_inicial.tex")

# Seleccionar las variables independientes significativas
significant_vars <- coef_table %>%
  filter(p.value <= 0.05, term != "(Intercept)") %>%
  pull(term)

# Nuevo modelo
significant_formula <- as.formula(paste("var60 ~", paste(significant_vars, collapse = " + ")))

# Ajustar el nuevo modelo de regresión lineal solo con variables significativas
model_significant <- lm(significant_formula, data = matrix_data, na.action = na.exclude)

# Calcular errores robustos para el modelo final
robust_se_final <- coeftest(model_significant, vcov = vcovHC(model_significant, type = "HC1"))

# Imprimir el resumen del modelo final

summary_significant <- summary(model_significant)
print(tidy(model_significant))

stargazer(model_significant, type = "latex", se = list(robust_se_final[, 2]), 
          title = "Regresión Lineal Final con Errores Robustos", 
          dep.var.labels = "var60", 
          covariate.labels = significant_vars,
          out = "modelo_final.tex")


# Modelo LR---------------------------------------------------------------------

# Definir la fórmula de la regresión logística con todas las variables "var" excepto "var60"
formula_logistic <- as.formula(paste("Riesgo ~", independent_vars, "+ VIGENCIA + caracter + nivel"))

# Ajustar el modelo de regresión logística
model_logistic <- glm(formula_logistic, data = panel_ols, family = binomial(), na.action = na.exclude)

# Calcular los errores estándar robustos usando sandwich y lmtest
robust_se_logistic <- coeftest(model_logistic, vcov = vcovHC(model_logistic, type = "HC1"))

# Obtener la tabla de coeficientes
coef_table <- tidy(model_logistic, conf.int = TRUE) %>%
  filter(term %in% rownames(robust_se_logistic)) %>%
  mutate(p.value = robust_se_logistic[, 4]) %>%
  mutate(Significancia = ifelse(p.value <= 0.05, "Significativa", "No Significativa"))

# Imprimir la tabla de coeficientes con la significancia estadística
print(coef_table)

# Generar la presentación del modelo en formato LaTeX con stargazer
stargazer(model_logistic, type = "latex", se = list(robust_se_logistic[, 2]), 
          title = "Regresión Logística con Errores Robustos", 
          dep.var.labels = "Riesgo", 
          covariate.labels = rownames(robust_se_logistic),
          out = "modelo_logistico.tex")

# Filtrar las variables significativas y ajustar un nuevo modelo
significant_vars <- coef_table %>%
  filter(p.value <= 0.05) %>%
  pull(term)

# Ajustar la fórmula para el modelo final sólo con variables significativas, si existen
if (length(significant_vars) > 0) {
  # Si hay variables significativas, procede con la fórmula y el modelo
  significant_formula_logistic <- as.formula(paste("Riesgo ~", paste(significant_vars, collapse = " + ")))
  
  # Ajustar el modelo de regresión logística con variables significativas
  model_significant_logistic <- glm(significant_formula_logistic, data = panel_ols, family = binomial(), na.action = na.exclude)
  
  # Calcular errores robustos para el modelo final
  robust_se_final_logistic <- coeftest(model_significant_logistic, vcov = vcovHC(model_significant_logistic, type = "HC1"))
  
  # Generar la presentación del modelo final en LaTeX
  stargazer(model_significant_logistic, type = "latex", se = list(robust_se_final_logistic[, 2]), 
            title = "Regresión Logística Final con Errores Robustos", 
            dep.var.labels = "Riesgo", 
            covariate.labels = significant_vars,
            out = "modelo_final_logistico.tex")
} else {
  # Si no hay variables significativas, mostrar un mensaje de advertencia
  warning("No hay variables significativas para incluir en el modelo final.")
}
