
# Datos SIHO--------------------------------------------------------------------

setwd("C:/Users/andre/OneDrive - Universidad de los andes/Tesis/data/R")

# INGRESOS----------------------------------------------------------------------

# Cargamos Ingresos 2012 - 2019
ingresos <- read_excel("1_ingresos_12_19.xlsx", na = "NA")

# Tabla de conceptos de Ingresos
ingresos_conceptos <- as.data.frame(sort(table(ingresos$concepto), decreasing = TRUE))

# Tabla de Ingresos totales
ingresos_total <- ingresos %>% filter(concepto == "Total de ingresos")

ingresos_total <- ingresos_total %>% mutate(porc_recaudo = ingresos_recaudo_real/ingresos_definitivo_real)

ingresos_total$porc_recaudo[ingresos_total$porc_recaudo == Inf] <- 0

# GASTOS------------------------------------------------------------------------

# Cargamos Gastos 2012 - 2019
gastos <- read_excel("2_gastos_12_19.xlsx", na = "NA")

# Tabla de conceptos de Gastos
gastos_conceptos <- as.data.frame(sort(table(gastos$concepto), decreasing = TRUE))

# Tabla de Gastos totales
gastos_total <- gastos %>% filter(concepto == "TOTAL DE GASTOS" | concepto == "INVERSION" | concepto == "GASTOS DE FUNCIONAMIENTO")

gastos_total <- gastos_total %>%  mutate(porc_compromisos = gastos_compromiso_real/gastos_definitivo_real)

# CARTERA-----------------------------------------------------------------------

# Cargamos Cartera 2012 - 2019
cartera <- read_excel("3_cartera_12_19.xlsx", na = "NA")

# Tabla de conceptos de Cartera
cartera_conceptos <- as.data.frame(sort(table(cartera$concepto), decreasing = TRUE))

# Tabla de Cartera total
cartera_total <- cartera %>% filter(concepto == "TOTAL")
cartera_total <- cartera_total %>% mutate(porc_60 = hasta60 / `total_cartera radicada`,
                                          porc_61_90 = de61a90 / `total_cartera radicada`,
                                          porc_91_180 = de91a180 /`total_cartera radicada`,
                                          porc_181_360 = de181a360 /`total_cartera radicada`,
                                          porc_361 = mayor360 /`total_cartera radicada`)

# PASIVOS-----------------------------------------------------------------------

# Cargamos Pasivos 2012 - 2019
pasivos <- read_excel("4_pasivos_12_19.xlsx", na = "NA")

# Tabla de conceptos de Pasivos
pasivos_conceptos <- as.data.frame(sort(table(pasivos$concepto), decreasing = TRUE))

# Tabla de Pasivos totales
pasivos_total <- pasivos %>% filter(concepto == "TOTAL PASIVO")

pasivos_total <- pasivos_total %>% mutate(porc_anterior = vigencia_anterior / total,
                                          porc_actual = vigencia_actual / total)

# PRODUCCION--------------------------------------------------------------------

# Cargamos Producción 2012 - 2019
produccion <- read_excel("5_produccion_12_19.xlsx", na = "NA")

# Tabla de conceptos de Producción
produccion_conceptos <- as.data.frame(sort(table(produccion$concepto), decreasing = TRUE))
tabla_produccion <- c("Total de egresos", 
                      "Total de cirugías realizadas (Sin incluir partos y cesáreas)", 
                      "Consultas de medicina especializada electivas realizadas",
                      "Consultas de medicina especializada urgentes realizadas",
                      "Consultas de medicina general electivas realizadas",
                      "Consultas de medicina general urgentes realizadas",
                      "Controles de enfermería (Atención prenatal / crecimiento y desarrollo)",
                      "Partos por cesárea",
                      "Partos vaginales")

tabla_produccion <- as.data.frame(tabla_produccion)
tabla_produccion <- tabla_produccion %>% rename(produccion = tabla_produccion)

# Tabla de Producción total
producción_total <- produccion %>% filter(concepto %in% tabla_produccion$produccion)
