
#------------------------------------------------------------------------------
#                                                                             #
#                           TRABAJO DE GRADO                                  #
#         Categorización de riesgos de Empresas Sociales del Estado           #
#                                                                             #
#------------------------------------------------------------------------------

# Limpieza del entorno

rm(list=ls())

# Directorio principal a usar

setwd("C:/Users/andre/OneDrive - Universidad de los andes/tesis/data/R")

# Librerías que necesitamos

library(pacman)
p_load(dplyr, tidyverse, readr, openxlsx, writexl, readxl, readr,rlist, ggplot2,
       cluster,factoextra, nnet, tidyr, caret, broom, glmnet, lmtest, sandwich, 
       stargazer,flextable, lmtestcar)

options(scipen = 999)

# Cargar archivos básicos de información

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
    
 # Vigencias que necesitamos
    vigencias <- data.frame(vigencia = c(2012:2019))

# Compilación de scrips a usar

    histograma      <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/2_histograma.R"
    nubes_puntos    <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/3_nubes_puntos.R"
    vitales         <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/4_vitales.R"
    res_256         <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/5_res_256.R"
    siho            <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/6_SIHO.R"
    panel           <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/7_panel.R"
    clusters        <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/8_clusters.R"
    pca             <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/9_PCA.R"
    cuartiles       <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/10_cuartiles.R"
    OLS             <- "C:/Users/andre/OneDrive - Universidad de los andes/Tesis/scripts/11_OLS.R"
    
    source(histograma)
    source(nubes_puntos)
    source(vitales)
    source(res_256)
    source(siho)
    source(panel)
    #source(clusters) #Para ejecutar se requiere gestionar los valores NA/NAN/INF
    source(pca)
    source(cuartiles)
    source(OLS)
   