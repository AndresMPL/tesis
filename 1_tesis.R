
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

# Cargamos archivos básicos

setwd("C:/Users/andre/OneDrive - Universidad de los andes/Tesis/data/R")

 # Listado ESE
    lista_ese <- read_excel("listado_ese.xlsx", na = "NA")

 # Archivo histórico de riesgos
    historico <- read_excel("historico_riesgos.xlsx", na = "NA")
    
 # Listado de diagnósticos
    
    #XLS Descargado de: https://web.sispro.gov.co/WebPublico/Consultas/ConsultarDetalleReferenciaBasica.aspx?Code=CIE10
    #Los campos en blanco de "Extra_VI_Capitulo": se asignan los 3 primero dígitos de "codigo_cie10"

    #Explicación: https://ais.paho.org/classifications/chapters/pdf/volume1.pdf 
    #Capítulos - Nombres
    #Categorías - 3 dígitos
    #Subcategorías - 4 dígitos
    
    diagnostico <- read_excel("cie10.xlsx", na = "NA")
    dx_tabla <- diagnostico %>% 
        select(capitulo_ajustado, codigo_cie10, nombre) %>% 
        rename(nombre_dx = nombre)

 # Listado ESE
    deflactor <- read_excel("deflactor.xlsx", na = "NA")

    # Scripts que vamos a ejecutar
    
    #Estadisticas generales
    
    
    #Estadisticas Vitales
    
    
    #Resolución 256
    
    
    #Información Presupuestal y Capacidad SIHO

    
    #Generar el Panel de datos
    
    
    #Ejecutar el MLR
    
    
    
# Función para ejecutar scripts
    
   