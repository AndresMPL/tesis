
# Modelo RLM--------------------------------------------------------------------

# Convertimos las columnas a factores y eliminamos espacios

str(panel_final) # Utilizaremos esta tabla

convertir_a_factor <- function(x, columnas) {
  for (col in columnas) {
    x[[col]] <- gsub(" ", "_", x[[col]])
    x[[col]] <- as.factor(x[[col]])
  }
    return(x)
}

columnas_a_modificar <- c("Nuevo_Riesgo", "Riesgo_Anterior", "caracter")
panel_final <- convertir_a_factor(panel_final, columnas_a_modificar)

lista_variables <- tabla_reportes$id

# Riesgo Nuevo

fx_modelo_nuevo <- as.formula(paste("Nuevo_Riesgo ~ VIGENCIA + nivel + caracter +", paste(lista_variables, collapse = " + ")))

modelo_rlm_nuevo <- multinom(fx_modelo_nuevo, panel_final) # Riesgo Nuevo
summary(modelo_rlm_nuevo)

#Riesgo Anterior

fx_modelo_anterior <- as.formula(paste("Riesgo_Anterior ~ VIGENCIA + nivel + caracter +", paste(lista_variables, collapse = " + ")))

modelo_rlm_anterior <- multinom(fx_modelo_anterior, panel_final) #Riesgo Anterior
summary(modelo_rlm_anterior)