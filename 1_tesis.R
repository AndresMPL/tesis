
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

    #Listado ESE----------------------------------------------------------------
    lista_ese <- read_excel("R/listado_ese.xlsx", na = "NA")

    #Archivo histórico de riesgos-----------------------------------------------
    historico <- read_excel("R/historico_riesgos.xlsx", na = "NA")
    
    #Listado de diagnósticos----------------------------------------------------
    
    #XLS Descargado de: https://web.sispro.gov.co/WebPublico/Consultas/ConsultarDetalleReferenciaBasica.aspx?Code=CIE10
    #Los campos en blanco de "Extra_VI_Capitulo": se asignan los 3 primero dígitos de "codigo_cie10"

    #Explicación: https://ais.paho.org/classifications/chapters/pdf/volume1.pdf 
    #Capítulos - Nombres
    #Categorías - 3 dígitos
    #Subcategorías - 4 dígitos
    
    diagnostico <- read_excel("R/cie10.xlsx", na = "NA")
    dx_tabla <- diagnostico %>% 
        select(Extra_V, Extra_VI_Capitulo, descripcion, codigo_cie10, nombre) %>% 
        rename(Extra_V_Capitulo = Extra_V, Extra_VI_3=Extra_VI_Capitulo)
    
    dx_capitulos <- as.data.frame(table(dx_tabla$Extra_V_Capitulo))
    

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
