
#------------------------------------------------------------------------------
#                                                                             #
#                           TRABAJO DE GRADO                                  #
#         Categorización de riesgos de Empresas Sociales del Estado           #
#                                                                             #
#------------------------------------------------------------------------------


# Librerías que necesitamos

library(pacman)
p_load(dplyr, tidyverse, readr, openxlsx, writexl, readxl, readr,rlist, ggplot2)
options(scipen = 999)

# Limpiamos el entorno

rm(list=ls())

# Directorio archivos básicos

setwd("C:/Users/andre/OneDrive - Universidad de los andes/Tesis/data")

    #Listado ESE
    lista_ese <- read_excel("R/listado_ese.xlsx", na = "NA")

    #Archivo histórico de riesgos
    historico <- read_excel("R/historico_riesgos.xlsx", na = "NA")
    
    #Listado de diagnósticos
    diagnostico <- read_excel("R/cie10.xlsx", na = "NA")

# Scripts que vamos a ejecutar
    
    #Estadisticas generales
    
    
    #Estadisticas Vitales
    
    
    #Resolución 256
    
    
    #Información Presupuestal y Capacidad SIHO

    
    #Generar el Panel de datos
    
    
    #Ejecutar el MLR
    
    
    
# Función para ejecutar scripts
    
    ejecutar_script_r <- function(ruta_script) {
        if (file.exists(ruta_script)) {
            tryCatch({
                source(ruta_script)
                cat("Ejecución del script:", ruta_script, "completada.\n")
            }, error = function(e) {
                cat("Error al ejecutar el script:", ruta_script, "\n", e$message, "\n")
            })
        } else {
            cat("El archivo no existe:", ruta_script, "\n")
        }
    }
    
    ejecutar_script_r("mi_script.R")
