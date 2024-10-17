
# Modelo OLS--------------------------------------------------------------------

cols_var <- grep("^var", names(panel_final), value = TRUE)
cols_no_var <- setdiff(names(panel_final), cols_var)
panel_ols <- panel_final[, c(cols_no_var, cols_var)]

media_mas_desviacion <- mean(panel_ols$orden_var60, na.rm = TRUE) + sd(panel_ols$orden_var60, na.rm = TRUE)

panel_ols <- panel_ols %>% 
  select(VIGENCIA, COD_CHIP, nombre_ese, nivel, caracter, Nuevo_Riesgo, Riesgo_Anterior, Cambio, all_of(tabla_reportes$orden)) %>% 
  mutate(Riesgo60 = if_else(orden_var60 <= media_mas_desviacion, orden_var60, media_mas_desviacion)) # Criterio arbitrario considerando la media+1sd de la variable

# Convertir variables de control en factores
panel_ols$VIGENCIA <- as.factor(panel_ols$VIGENCIA)
panel_ols$caracter <- as.factor(panel_ols$caracter)
panel_ols$nivel <- as.factor(panel_ols$nivel)

# Definir la fórmula de la regresión lineal inicial
vars_columns <- grep("^(ascendente|descendente)", names(panel_ols), value = TRUE)
independent_vars <- paste(vars_columns[vars_columns != "orden_var60"], collapse = " + ")
formula_initial <- as.formula(paste("Riesgo60 ~", "VIGENCIA + ", independent_vars))

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
          dep.var.labels = "Riesgo60", 
          covariate.labels = c(vars_columns[vars_columns != "orden_var60"], "VIGENCIA"),
          out = "modelo_inicial.tex")

# Seleccionar las variables independientes significativas
significant_vars <- coef_table %>%
  filter(p.value <= 0.05, term != "(Intercept)") %>%
  pull(term)

# Nuevo modelo
significant_formula <- as.formula(paste("Riesgo60 ~", paste(significant_vars, collapse = " + ")))

# Ajustar el nuevo modelo de regresión lineal solo con variables significativas
model_significant <- lm(significant_formula, data = panel_ols, na.action = na.exclude)

# Calcular errores robustos para el modelo final
robust_se_final <- coeftest(model_significant, vcov = vcovHC(model_significant, type = "HC1"))

# Imprimir el resumen del modelo final

summary_significant <- summary(model_significant)
print(tidy(model_significant))

stargazer(model_significant, type = "latex", se = list(robust_se_final[, 2]), 
          title = "Regresión Lineal Final con Errores Robustos", 
          dep.var.labels = "Riesgo60", 
          covariate.labels = significant_vars,
          out = "modelo_final.tex")

# Pruebas ----------------------------------------------------------------------

# Prueba de Heterocedasticidad (Breusch-Pagan)

bp_test_initial <- bptest(model_initial)
bp_test_significant <- bptest(model_significant)

# Prueba de Normalidad de los Residuales (Shapiro-Wilk)

shapiro_initial <- shapiro.test(residuals(model_initial))
shapiro_significant <- shapiro.test(residuals(model_significant))

# Prueba de Autocorrelación (Durbin-Watson)

dw_test_initial <- dwtest(model_initial)
dw_test_significant <- dwtest(model_significant)

# Resultados de las pruebas

print("Prueba de Breusch-Pagan para el modelo inicial:")
print(bp_test_initial)
print("Prueba de Breusch-Pagan para el modelo significativo:")
print(bp_test_significant)

print("Prueba de Shapiro-Wilk para el modelo inicial:")
print(shapiro_initial)
print("Prueba de Shapiro-Wilk para el modelo significativo:")
print(shapiro_significant)

print("Prueba de Durbin-Watson para el modelo inicial:")
print(dw_test_initial)
print("Prueba de Durbin-Watson para el modelo significativo:")
print(dw_test_significant)



calc_condition_index <- function(model) {
  # Obtener la matriz de diseño (matriz X) del modelo
  X <- model.matrix(model)
  # Realizar la descomposición en valores singulares
  svd_decomp <- svd(X)
  # Calcular el índice de condición
  condition_index <- max(svd_decomp$d) / svd_decomp$d
  return(condition_index)
}

# Calcular el índice de condición para los modelos inicial y significativo
condition_index_initial <- calc_condition_index(model_initial)
condition_index_significant <- calc_condition_index(model_significant)

# Imprimir los resultados
print("Índice de Condición para el modelo inicial:")
print(condition_index_initial)

print("Índice de Condición para el modelo significativo:")
print(condition_index_significant)