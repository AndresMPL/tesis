
#------------------------------------------------------------------------------
#                                                                             #
#                           TRABAJO DE GRADO                                  #
#         Categorización de riesgos de Empresas Sociales del Estado           #
#                                                                             #
#------------------------------------------------------------------------------


# Librerías que necesitamos

library(pacman)
p_load(dplyr, tidyverse, readr, openxlsx, writexl, readxl, readr,rlist)
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

# Ejecución scripts
    
    #Estadisticas generales
    
    #Estadisticas Vitales
    
    #Resolución 256
    
    #Información Presupuestaly Capacidad SIHO

