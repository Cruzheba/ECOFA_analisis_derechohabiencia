# ==============================================================================
# ANÁLISIS DE PÉRDIDA DE ÓRGANOS DENTALES EN FUNCIÓN DE DERECHOHABIENCIA
# ==============================================================================
# Proyecto: Análisis de pérdida dental y derechohabiencia
# Repositorio: https://github.com/Cruzheba/ECOFA_poject_1
# Autor: [Tu nombre]
# Fecha: 2026-01-09
# Descripción: Análisis de la asociación entre derechohabiencia y pérdida de
#              órganos dentales, ajustando por variables socioeconómicas
# ==============================================================================

# 1. CARGAR LIBRERÍAS ----
library(tidyverse)

# 2. CARGAR DATOS Y CREAR COLUMNA IDENTIFICADORA ----

# 2.1 Interrogación ficha (variables sociodemográficas y derechohabiencia)
interrogación_ficha <- read_csv("ficha_de_interrogacion.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.2 Odontograma infantil (estado de dientes temporales)
odontograma_infantil <- read_csv("odontograma_infantil.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.3 Vestibular (estado de dientes permanentes)
vestibular <- read_csv("vestibular.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.4 Registro clínico (examen clínico oral)
registro_clinico <- read_csv("registro_clinico_odontologico.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.5 Antecedentes familiares
antecedentes_familiares <- read_csv("antecedentes_familiares_hereditarios.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.6 Antecedentes no patológicos
antecedentes_no_patologicos <- read_csv("antecedentes_personales_no_patologicos.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.7 Antecedentes patológicos
antecedentes_patologicos <- read_csv("antecedentes_personales_patologicos.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.8 Aparatos y sistemas
aparatos_sistemas <- read_csv("aparatos_y_sistemas.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.9 Diagnóstico
diagnostico <- read_csv("diagnostico.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.10 Notas médicas
notas_medicas <- read_csv("notas_medicas.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.11 Tratamiento general
tratamiento_gen <- read_csv("tratamiento_gen.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.12 Tratamiento desglose (tratamientos por órgano dental)
tratamiento_desglose <- read_csv("tratamiento_desglose.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.13 Alimentación y vivienda (variables socioeconómicas)
alimentacion_vivienda <- read_csv("alimentacion_vivienda.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.14 Mujeres (antecedentes gineco-obstétricos)
mujeres <- read_csv("mujeres.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.15 Índice de higiene oral
indice_higiene <- read_csv("indice_higiene_oral.csv") |>
  mutate(clinica_no_expediente = paste(clinica, no_expediente, sep = "-")) |>
  relocate(clinica_no_expediente, .before = everything())

# 2.16 Resumen de tablas cargadas
cat("\n========== RESUMEN DE TABLAS CARGADAS ==========\n\n")
cat("TABLAS DE DATOS DE PACIENTES (con clinica_no_expediente):\n")
cat("1.  Interrogación ficha:         ", nrow(interrogación_ficha), "registros,  ", ncol(interrogación_ficha), "columnas\n")
cat("2.  Odontograma infantil:        ", nrow(odontograma_infantil), "registros,  ", ncol(odontograma_infantil), "columnas\n")
cat("3.  Vestibular:                  ", nrow(vestibular), "registros,  ", ncol(vestibular), "columnas\n")
cat("4.  Registro clínico:            ", nrow(registro_clinico), "registros,  ", ncol(registro_clinico), "columnas\n")
cat("5.  Antecedentes familiares:     ", nrow(antecedentes_familiares), "registros,  ", ncol(antecedentes_familiares), "columnas\n")
cat("6.  Antecedentes no patológicos: ", nrow(antecedentes_no_patologicos), "registros,  ", ncol(antecedentes_no_patologicos), "columnas\n")
cat("7.  Antecedentes patológicos:    ", nrow(antecedentes_patologicos), "registros,  ", ncol(antecedentes_patologicos), "columnas\n")
cat("8.  Aparatos y sistemas:         ", nrow(aparatos_sistemas), "registros,  ", ncol(aparatos_sistemas), "columnas\n")
cat("9.  Diagnóstico:                 ", nrow(diagnostico), "registros,  ", ncol(diagnostico), "columnas\n")
cat("10. Notas médicas:               ", nrow(notas_medicas), "registros,  ", ncol(notas_medicas), "columnas\n")
cat("11. Tratamiento general:         ", nrow(tratamiento_gen), "registros,  ", ncol(tratamiento_gen), "columnas\n")
cat("12. Tratamiento desglose:        ", nrow(tratamiento_desglose), "registros,  ", ncol(tratamiento_desglose), "columnas\n")
cat("13. Alimentación vivienda:       ", nrow(alimentacion_vivienda), "registros,  ", ncol(alimentacion_vivienda), "columnas\n")
cat("14. Mujeres:                     ", nrow(mujeres), "registros,  ", ncol(mujeres), "columnas\n")
cat("15. Índice higiene oral:         ", nrow(indice_higiene), "registros,  ", ncol(indice_higiene), "columnas\n")
cat("\n=======================================================\n")
cat("✅ TODAS LAS 15 TABLAS CARGADAS EXITOSAMENTE\n")
cat("✅ Todas las tablas tienen la columna 'clinica_no_expediente'\n")

# 3. EXPLORACIÓN DE VARIABLES CLAVE ----
# TODO: Explorar categorías de institucion_derechohabiencia
# TODO: Analizar odontogramas para identificar pérdida dental
# TODO: Revisar variables socioeconómicas disponibles

# 4. SELECCIÓN DE VARIABLES ----
# TODO: Seleccionar columnas relevantes de cada tabla

# 5. UNIÓN DE TABLAS ----
# TODO: Unir tablas usando clinica_no_expediente

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

