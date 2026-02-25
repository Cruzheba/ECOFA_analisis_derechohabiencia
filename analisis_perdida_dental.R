# ==============================================================================
# ANÁLISIS DEl ESTADO DE SALUD BUCAL EN FUNCIÓN DE DERECHOHABIENCIA
# ==============================================================================
# Proyecto: Evaluación del estado de salud bucal en pacientes que acuden a los servicios odontológicos universitarios: Análisis de la asociación con la pobreza y la derechohabiencia a servicios de salud.
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

# 3.4 Reportar resultados

cantidad_registros_iniciales <- nrow(tabla_integrada)

cat("✅ Variable creada: cantidad_registros_iniciales =", cantidad_registros_iniciales, "\n\n")

# 4. FILTRAR REGISTROS AUTORIZADOS

# 4.1 Filtrar registros aceptados
tabla_integrada <- tabla_integrada |>
  filter(registro_aceptado == TRUE)

cat("✅ Filtro aplicado: solo registros aceptados\n")
cat("Registros restantes:", nrow(tabla_integrada), "\n\n")

# 4.2 Reportar resultados

cantidad_registros_autorizados <- nrow(tabla_integrada)

cat("✅ Variable creada: cantidad_registros_autorizados =", cantidad_registros_autorizados, "\n")
cat("Registros eliminados:", cantidad_registros_iniciales - cantidad_registros_autorizados, 
    "(", round((cantidad_registros_iniciales - cantidad_registros_autorizados) / cantidad_registros_iniciales * 100, 2), "%)\n\n")

# 5. LIMPIEZA, TRANSFORMACIÓN Y CREACIÓN DE VARIABLES ----

# 5.1 Grupos de edad

# 5.1.1 Calcular edad en años (enteros) a partir de fecha_nacimiento y fecha_inicio
tabla_integrada <- tabla_integrada |>
  mutate(
    edad = as.integer(as.numeric(difftime(fecha_inicio, fecha_nacimiento, units = "days")) / 365.25),
    .after = fecha_nacimiento
  )

cat("✅ Columna 'edad' creada exitosamente\n")
cat("Resumen de la variable edad:\n")
print(summary(tabla_integrada$edad))
cat("\n")

# 5.1.2 Filtrar edades válidas (adultos: 18-100 años)

# Contar casos antes del filtro

negativos <- sum(tabla_integrada$edad < 0, na.rm = TRUE)
menores <- sum(tabla_integrada$edad >= 0 & tabla_integrada$edad < 18, na.rm = TRUE)
mayores_100 <- sum(tabla_integrada$edad > 100, na.rm = TRUE)
na_edad <- sum(is.na(tabla_integrada$edad))
excluidos_por_edad <- negativos + menores + mayores_100 + na_edad

# Aplicar filtro
tabla_integrada <- tabla_integrada |>
  filter(!is.na(edad), edad >= 18, edad <= 100)

cantidad_registros_edad <- nrow(tabla_integrada)

# 5.1.3 Reportar resultados
cat("✅ Filtro de edad aplicado (18-100 años)\n\n")

cat("=== REGISTROS ELIMINADOS ===\n")

cat("Edades negativas:", negativos, "\n")
cat("Menores (0-17 años):", menores, "\n")
cat("Mayores a 100 años:", mayores_100, "\n")
cat("Valores NA:", na_edad, "\n")
cat("Total eliminado:", cantidad_registros_autorizados - nrow(tabla_integrada), "\n\n")
cat("=== REGISTROS RESTANTES ===\n")
cat("Total:", nrow(tabla_integrada), "registros\n\n")

# 5.2 Índice socioeconómico

# 5.2.1 Generación de indice de hacinamiento

# Eliminar registros que en las columnas "no_habitaciones" y "no_personas_en_vivienda" tengan valor "0"

# Contar registros antes del filtro
total_antes_hacinamiento <- nrow(tabla_integrada)

# Aplicar filtro
tabla_integrada <- tabla_integrada |>
  filter(no_habitaciones != 0, no_personas_en_vivienda != 0)

# Calcular registros eliminados y restantes
excluidos_hacinamiento <- total_antes_hacinamiento - nrow(tabla_integrada)
cantidad_registros_hacinamiento <- nrow(tabla_integrada)

# Reportar resultados
cat("✅ Filtro de hacinamiento aplicado\n\n")
cat("=== REGISTROS ELIMINADOS ===\n")
cat("Registros con no_habitaciones = 0 o no_personas_en_vivienda = 0:", excluidos_hacinamiento, "\n")
cat("Porcentaje eliminado:", round(excluidos_hacinamiento / total_antes_hacinamiento * 100, 2), "%\n\n")
cat("=== REGISTROS RESTANTES ===\n")
cat("cantidad_registros_hacinamiento =", cantidad_registros_hacinamiento, "registros\n\n")

# Crear índice
tabla_integrada <- tabla_integrada |>
  mutate(
    indice_hacinamiento = no_personas_en_vivienda / no_habitaciones,
    .after = no_personas_en_vivienda
  )

cat("✅ Índice de hacinamiento creado\n\n")
cat("=== RESUMEN DEL ÍNDICE DE HACINAMIENTO ===\n")
print(summary(tabla_integrada$indice_hacinamiento))
cat("\n")

# Crear indicador de carencia por calidad y espacios de la vivienda (CCEV)según CONEVAL
tabla_integrada <- tabla_integrada |>
  mutate(
    CCEV_hacinamiento = if_else(indice_hacinamiento > 2.5, 1, 0),
    .after = indice_hacinamiento
  )

cat("✅ Columna CCEV_hacinamiento creada (metodología CONEVAL)\n\n")
cat("=== DISTRIBUCIÓN DE CARENCIA POR HACINAMIENTO (CCEV_hacinamiento) ===\n")
cat("Sin carencia (CCEV_hacinamiento = 0, hacinamiento ≤ 2.5):", sum(tabla_integrada$CCEV_hacinamiento == 0), 
    "(", round(sum(tabla_integrada$CCEV_hacinamiento == 0) / nrow(tabla_integrada) * 100, 2), "%)\n")
cat("Con carencia (CCEV_hacinamiento = 1, hacinamiento > 2.5):", sum(tabla_integrada$CCEV_hacinamiento == 1), 
    "(", round(sum(tabla_integrada$CCEV_hacinamiento == 1) / nrow(tabla_integrada) * 100, 2), "%)\n\n")


# Crear indicadores de carencia por servicios básicos (CSBV) según CONEVAL
tabla_integrada <- tabla_integrada |>
  mutate(
    CSBV_agua = if_else(agua_intradomiciliaria == FALSE, 1, 0),
    .after = agua_intradomiciliaria
  ) |>
  mutate(
    CSBV_drenaje = if_else(drenaje == FALSE, 1, 0),
    .after = drenaje
  ) |>
  mutate(
    CSBV_luz = if_else(luz == FALSE, 1, 0),
    .after = luz
  )

cat("✅ Columnas CSBV creadas (Carencia por Servicios Básicos en la Vivienda)\n")
cat("CSBV_agua: Con carencia (1):", sum(tabla_integrada$CSBV_agua == 1, na.rm = TRUE), 
    "(", round(sum(tabla_integrada$CSBV_agua == 1, na.rm = TRUE) / nrow(tabla_integrada) * 100, 2), "%)\n")
cat("CSBV_drenaje: Con carencia (1):", sum(tabla_integrada$CSBV_drenaje == 1, na.rm = TRUE), 
    "(", round(sum(tabla_integrada$CSBV_drenaje == 1, na.rm = TRUE) / nrow(tabla_integrada) * 100, 2), "%)\n")
cat("CSBV_luz: Con carencia (1):", sum(tabla_integrada$CSBV_luz == 1, na.rm = TRUE), 
    "(", round(sum(tabla_integrada$CSBV_luz == 1, na.rm = TRUE) / nrow(tabla_integrada) * 100, 2), "%)\n\n")


# Clasificación según OMS (opcional):
# < 2.5 = Sin hacinamiento
# 2.5 - 3.4 = Hacinamiento medio
# 3.5 - 4.9 = Hacinamiento alto
# >= 5.0 = Hacinamiento crítico

# 5.2.2 Generación de indice de dependencia económica

# Eliminar registros que en las columnas "no_personas_en_familia" tengan valor 0



# "no_personas_trabajan", "no_personas_menores_15" tengan valor 0.



# 8. ANÁLISIS EXPLORATORIO ----
# TODO: Análisis descriptivo por grupos
# TODO: Visualizaciones exploratorias

# 9. MODELADO ESTADÍSTICO ----
# TODO: Análisis de asociación entre derechohabiencia y pérdida dental
# TODO: Ajuste por variables de confusión

# 10. EXPORTAR DATOS PROCESADOS Y ANONIMIZADOS ----
# TODO: Guardar tabla final sin datos personales identificables

