
# Desviación estandar-----------------------------------------------------------

# NUEVO RIESGO
tabla_agrupada_nuevo <- panel_final %>%
  group_by(VIGENCIA, nivel, caracter, Nuevo_Riesgo) %>%
  summarise(across(starts_with(c("ascendente", "descendente")), sd, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(Riesgo = Nuevo_Riesgo) %>% 
  mutate(Método = "Nuevo riesgo")

# RIESGO ANTERIOR
tabla_agrupada_anterior <- panel_final %>%
  group_by(VIGENCIA, nivel, caracter, Riesgo_Anterior) %>%
  summarise(across(starts_with(c("ascendente", "descendente")), sd, na.rm = TRUE)) %>%
  ungroup() %>% 
  rename(Riesgo = Riesgo_Anterior) %>% 
  mutate(Método = "Riesgo Anterior")

# Unir ambas tablas
tabla_desviacion <- bind_rows(tabla_agrupada_nuevo, tabla_agrupada_anterior)

write.table(tabla_desviacion, file = "tabla_desviacion.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Coeficiente de Variación------------------------------------------------------

# NUEVO RIESGO
cv_intragrupos_nuevo <- panel_final %>%
  group_by(VIGENCIA, nivel, caracter, Nuevo_Riesgo) %>%
  summarise(across(starts_with(c("ascendente", "descendente")),
                   ~ sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100, .names = "cv_{.col}")) %>%
  ungroup() %>% 
  rename(Riesgo = Nuevo_Riesgo) %>% 
  mutate(Método = "Nuevo Riesgo")

# RIESGO ANTERIOR
cv_intragrupos_anterior <- panel_final %>%
  group_by(VIGENCIA, nivel, caracter, Riesgo_Anterior) %>%
  summarise(across(starts_with(c("ascendente", "descendente")),
                   ~ sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE) * 100, .names = "cv_{.col}")) %>%
  ungroup() %>% 
  rename(Riesgo = Riesgo_Anterior) %>% 
  mutate(Método = "Riesgo Anterior")

# Unir ambas tablas
tabla_cv <- bind_rows(cv_intragrupos_nuevo, cv_intragrupos_anterior)

write.table(tabla_cv, file = "tabla_cv.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)