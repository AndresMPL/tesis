
#---------------Datos SIHO----

setwd("C:/Users/andre/OneDrive - Universidad de los andes/Tesis/data/R")

 #INGRESOS----------------------------------------------------------------------
# Cargamos Ingresos 2012 - 2019
ingresos <- read_excel("1_ingresos_12_19.xlsx", na = "NA")

# Tabla de conceptos de Ingresos
ingresos_conceptos <- as.data.frame(sort(table(ingresos$concepto), decreasing = TRUE))

# Tabla de Ingresos totales
ingresos_total <- ingresos %>% filter(concepto == "Total de ingresos")


# GASTOS------------------------------------------------------------------------
# Cargamos Gastos 2012 - 2019
gastos <- read_excel("2_gastos_12_19.xlsx", na = "NA")

# Tabla de conceptos de Gastos
gastos_conceptos <- as.data.frame(sort(table(gastos$concepto), decreasing = TRUE))

# Tabla de Gastos totales
gastos_total <- gastos %>% filter(concepto == "TOTAL DE GASTOS" | concepto == "INVERSION" | concepto == "GASTOS DE FUNCIONAMIENTO")


# CARTERA-----------------------------------------------------------------------
# Cargamos Cartera 2012 - 2019
cartera <- read_excel("3_cartera_12_19.xlsx", na = "NA")

# Tabla de conceptos de Cartera
cartera_conceptos <- as.data.frame(sort(table(cartera$concepto), decreasing = TRUE))

# Tabla de Cartera total
cartera_total <- cartera %>% filter(concepto == "TOTAL")


# PASIVOS-----------------------------------------------------------------------
# Cargamos Pasivos 2012 - 2019
pasivos <- read_excel("4_pasivos_12_19.xlsx", na = "NA")

# Tabla de conceptos de Pasivos
pasivos_conceptos <- as.data.frame(sort(table(pasivos$concepto), decreasing = TRUE))

# Tabla de Pasivos totales
pasivos_total <- pasivos %>% filter(concepto == "TOTAL PASIVO")


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

