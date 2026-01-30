# ==============================================================================
# ANÁLISIS DEl ESTADO DE SALUD BUCAL EN FUNCIÓN DE DERECHOHABIENCIA
# ==============================================================================
# Proyecto: Análisis del estado de salid bucal y derechohabiencia
# Repositorio: https://github.com/Cruzheba/ECOFA_analisis_derechohabiencia
# Autor: [CRUZ HERNANDEZ BARREDA]
# Fecha: 2026-01-09
# Descripción: Análisis de la asociación entre derechohabiencia y el estado de salud bucal de acuerdo con el indice cpo, ajustando por variables socioeconómicas.
# ==============================================================================

# 1. CARGAR LIBRERÍAS ----
library(tidyverse)

# 2. CARGAR DATOS Y CREAR COLUMNA IDENTIFICADORA ----

# 2.1 Interrogación ficha (variables sociodemográficas y derechohabiencia)
interrogación_ficha <- read_csv("ficha_de_interrogacion.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.2 Antecedentes patológicos
antecedentes_patologicos <- read_csv("antecedentes_personales_patologicos.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.3 Tratamiento general
tratamiento_gen <- read_csv("tratamiento_gen.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything()) 

# 2.4 Alimentación y vivienda (variables socioeconómicas)
alimentacion_vivienda <- read_csv("alimentacion_vivienda.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.5 Índice de higiene oral
indice_higiene <- read_csv("indice_higiene_oral.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything()) |>
  # Eliminar columnas de índices de higiene inicial y final (24 columnas), no son útiles para nuestra investigación.
  select(-starts_with("ir_i_"), -starts_with("ic_i_"),
         -starts_with("ir_f_"), -starts_with("ic_f_"))

# 2.6 Resumen de tablas cargadas
cat("\n========== RESUMEN DE TABLAS CARGADAS ==========\n\n")
cat("TABLAS DE DATOS DE PACIENTES (con clinica_no_expediente):\n")
cat("1.  Interrogación ficha:         ", nrow(interrogación_ficha), "registros,  ", ncol(interrogación_ficha), "columnas\n")
cat("2.  Antecedentes patológicos:    ", nrow(antecedentes_patologicos), "registros,  ", ncol(antecedentes_patologicos), "columnas\n")
cat("3. Tratamiento general:         ", nrow(tratamiento_gen), "registros,  ", ncol(tratamiento_gen), "columnas\n")
cat("4. Alimentación vivienda:       ", nrow(alimentacion_vivienda), "registros,  ", ncol(alimentacion_vivienda), "columnas\n")
cat("5. Índice higiene oral:         ", nrow(indice_higiene), "registros,  ", ncol(indice_higiene), "columnas\n")
cat("\n=======================================================\n")
cat("✅ TODAS LAS 5 TABLAS CARGADAS EXITOSAMENTE\n")
cat("✅ Todas las tablas tienen la columna 'clinica_no_expediente'\n")

# 3. INTEGRACIÓN DE TABLAS ----

# 3.1 Integrar las 5 tablas usando tratamiento_gen como base
tabla_integrada <- tratamiento_gen |>
  left_join(interrogación_ficha, by = "clinica_no_expediente") |>
  left_join(antecedentes_patologicos, by = "clinica_no_expediente") |>
  left_join(alimentacion_vivienda, by = "clinica_no_expediente") |>
  left_join(indice_higiene, by = "clinica_no_expediente")

# 3.2 Limpiar columnas duplicadas
tabla_integrada <- tabla_integrada |>
  select(
    -ends_with(".y"),
    -ends_with(".x.x"),
    -ends_with(".y.y")
  ) |>
  # Eliminar las columnas sin sufijo (vienen de indice_higiene)
  select(-any_of(c("id", "clinica", "no_expediente"))) |>
  # Ahora renombrar las .x quitando el sufijo
  rename(
    id = id.x,
    clinica = clinica.x,
    no_expediente = no_expediente.x
  )

cat("✅ Columnas duplicadas eliminadas\n")
cat("Dimensiones finales:", nrow(tabla_integrada), "registros ×", 
    ncol(tabla_integrada), "columnas\n\n")

# 3.3 Eliminar tablas originales para liberar memoria
rm(interrogación_ficha, antecedentes_patologicos, tratamiento_gen, 
   alimentacion_vivienda, indice_higiene)
cat("✅ Tablas originales eliminadas (solo queda tabla_integrada)\n\n")

# 4. EXPLORACIÓN DE VARIABLES CLAVE ----
# TODO: Explorar categorías de institucion_derechohabiencia
# TODO: Analizar odontogramas para identificar pérdida dental
# TODO: Revisar variables socioeconómicas disponibles

# 5. SELECCIÓN DE VARIABLES ----
# TODO: Seleccionar columnas relevantes de cada tabla

# 6. LIMPIEZA DE DATOS ----
# TODO: Manejar valores faltantes
# TODO: Estandarizar categorías
# TODO: Crear variables derivadas

# 7. ANÁLISIS EXPLORATORIO ----
# TODO: Análisis descriptivo por grupos
# TODO: Visualizaciones exploratorias

# 8. MODELADO ESTADÍSTICO ----
# TODO: Análisis de asociación entre derechohabiencia y pérdida dental
# TODO: Ajuste por variables de confusión

# 9. EXPORTAR DATOS PROCESADOS Y ANONIMIZADOS ----
# TODO: Guardar tabla final sin datos personales identificables

