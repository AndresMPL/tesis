
#------------------------------------------------------------------------------
#                                                                             #
#                           TRABAJO DE GRADO                                  #
#         Categorización de riesgos de Empresas Sociales del Estado           #
#                                                                             #
#------------------------------------------------------------------------------


# Librerías que necesitamos

library(pacman)
p_load(dplyr, tidyverse, readr, openxlsx, writexl, readxl, readr,rlist, ggplot2, cluster,factoextra)
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

    histograma      <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/2_histograma.R"
    nubes_puntos    <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/3_nubes_puntos.R"
    vitales         <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/4_vitales.R"
    res_256         <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/5_res_256.R"
    siho            <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/6_SIHO.R"
    panel           <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/7_panel.R"
    clusters        <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/8_clusters.R"
    pca             <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/9_PCA.R"
    cuartiles       <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/10_cuartiles.R"
    
    source(histograma)
    source(nubes_puntos)
    source(vitales)
    source(res_256)
    source(siho)
    source(panel)
    #source(clusters) #En el panel no se imputan los valores NA/NAN/INF entonces este falla
    source(pca)
    source(cuartiles)
   