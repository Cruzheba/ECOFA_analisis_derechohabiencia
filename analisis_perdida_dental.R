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


# Crear grupos de edad
tabla_integrada <- tabla_integrada |>
  mutate(
    grupo_edad = case_when(
      edad >= 18 & edad <= 24 ~ "18-24",
      edad >= 25 & edad <= 29 ~ "25-29",
      edad >= 30 & edad <= 34 ~ "30-34",
      edad >= 35 & edad <= 39 ~ "35-39",
      edad >= 40 & edad <= 44 ~ "40-44",
      edad >= 45 & edad <= 49 ~ "45-49",
      edad >= 50 & edad <= 54 ~ "50-54",
      edad >= 55 & edad <= 59 ~ "55-59",
      edad >= 60 & edad <= 64 ~ "60-64",
      edad >= 65 & edad <= 69 ~ "65-69",
      edad >= 70 & edad <= 74 ~ "70-74",
      edad >= 75 & edad <= 79 ~ "75-79",
      edad >= 80 & edad <= 84 ~ "80-84",
      edad >= 85 & edad <= 89 ~ "85-89",
      edad >= 90 & edad <= 94 ~ "90-94",
      edad >= 95 & edad <= 100 ~ "95-100",
      TRUE ~ NA_character_
    ),
    .after = edad
  )

cat("✅ Columna 'grupo_edad' creada exitosamente\n\n")
cat("=== DISTRIBUCIÓN POR GRUPOS DE EDAD ===\n")
print(table(tabla_integrada$grupo_edad, useNA = "ifany"))
cat("\n")


# 5.2 Càlculo del índice CPO-D inicial

# 5.2.1 Eliminar registros que no contienen información sobre el índice CPO-D inicial (todas las columnas del índice = 0)
# Filtrar registros con CPO-D inicial válido
# Eliminar registros donde TODAS las columnas del índice CPO-D inicial sean 0
antes_filtro_cpod <- nrow(tabla_integrada)

tabla_integrada <- tabla_integrada |>
  filter(!(inicial_cariados == 0 & inicial_perdidos == 0 & 
           inicial_obturados == 0 & inicial_total_dientes == 0))

despues_filtro_cpod <- nrow(tabla_integrada)

# Reportar resultados
cat("✅ Filtro de CPO-D inicial aplicado\n\n")
cat("=== REGISTROS ELIMINADOS ===\n")
cat("Registros con CPO-D inicial = 0 (todas las columnas):", 
    antes_filtro_cpod - despues_filtro_cpod, "\n")
cat("Porcentaje eliminado:", 
    round((antes_filtro_cpod - despues_filtro_cpod) / antes_filtro_cpod * 100, 2), "%\n\n")
cat("=== REGISTROS RESTANTES ===\n")
cat("antes_filtro_cpod =", antes_filtro_cpod, "\n")
cat("despues_filtro_cpod =", despues_filtro_cpod, "\n\n")

# 5.2.2 Crear columna cpo_individual después de la columna "luz"
tabla_integrada <- tabla_integrada |>
  mutate(
    cpo_individual = inicial_cariados + inicial_perdidos + inicial_obturados,
    .after = luz
  )

# Verificar la nueva columna
tabla_integrada |>
  select(luz, cpo_individual, inicial_cariados, inicial_perdidos, inicial_obturados) |>
  head(10)

# Ver estadísticas descriptivas
summary(tabla_integrada$cpo_individual)



# 5.2.3 Categorización del estado de salud bucal en función del CPOD ajustado por grupo de edad en relación con clasificación de la OMS y propia ajustada en percentiles.

# Crear ambas clasificaciones usando los grupos de edad existentes
tabla_integrada <- tabla_integrada |>
  # CLASIFICACIÓN OMS (puntos de corte fijos por grupo)
  mutate(
    estado_salud_oms = case_when(
      # Grupos jóvenes (18-34 años): criterios más estrictos
      grupo_edad %in% c("18-24", "25-29", "30-34") & cpo_individual <= 3 ~ "MUY BAJO",
      grupo_edad %in% c("18-24", "25-29", "30-34") & cpo_individual <= 7 ~ "BAJO",
      grupo_edad %in% c("18-24", "25-29", "30-34") & cpo_individual <= 12 ~ "MODERADO",
      grupo_edad %in% c("18-24", "25-29", "30-34") & cpo_individual <= 18 ~ "ALTO",
      grupo_edad %in% c("18-24", "25-29", "30-34") & cpo_individual > 18 ~ "MUY ALTO",
      
      # Grupos intermedios (35-49 años)
      grupo_edad %in% c("35-39", "40-44", "45-49") & cpo_individual <= 5 ~ "MUY BAJO",
      grupo_edad %in% c("35-39", "40-44", "45-49") & cpo_individual <= 10 ~ "BAJO",
      grupo_edad %in% c("35-39", "40-44", "45-49") & cpo_individual <= 16 ~ "MODERADO",
      grupo_edad %in% c("35-39", "40-44", "45-49") & cpo_individual <= 22 ~ "ALTO",
      grupo_edad %in% c("35-39", "40-44", "45-49") & cpo_individual > 22 ~ "MUY ALTO",
      
      # Grupos maduros (50-64 años)
      grupo_edad %in% c("50-54", "55-59", "60-64") & cpo_individual <= 8 ~ "MUY BAJO",
      grupo_edad %in% c("50-54", "55-59", "60-64") & cpo_individual <= 14 ~ "BAJO",
      grupo_edad %in% c("50-54", "55-59", "60-64") & cpo_individual <= 20 ~ "MODERADO",
      grupo_edad %in% c("50-54", "55-59", "60-64") & cpo_individual <= 26 ~ "ALTO",
      grupo_edad %in% c("50-54", "55-59", "60-64") & cpo_individual > 26 ~ "MUY ALTO",
      
      # Grupos mayores (65+ años)
      grupo_edad %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-100") & cpo_individual <= 10 ~ "MUY BAJO",
      grupo_edad %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-100") & cpo_individual <= 16 ~ "BAJO",
      grupo_edad %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-100") & cpo_individual <= 22 ~ "MODERADO",
      grupo_edad %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-100") & cpo_individual <= 28 ~ "ALTO",
      grupo_edad %in% c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-100") & cpo_individual > 28 ~ "MUY ALTO",
      
      TRUE ~ NA_character_
    ),
    .after = cpo_individual
  ) |>
  # CLASIFICACIÓN POR PERCENTILES (ajustada a cada grupo de edad)
  group_by(grupo_edad) |>
  mutate(
    percentil_cpod = percent_rank(cpo_individual),
    estado_salud_percentiles = case_when(
      percentil_cpod <= 0.20 ~ "MUY BAJO",
      percentil_cpod <= 0.40 ~ "BAJO",
      percentil_cpod <= 0.60 ~ "MODERADO",
      percentil_cpod <= 0.80 ~ "ALTO",
      percentil_cpod > 0.80 ~ "MUY ALTO",
      TRUE ~ NA_character_
    ),
    .after = estado_salud_oms
  ) |>
  ungroup()

# Verificar resultados
cat("=== CLASIFICACIÓN OMS POR GRUPO DE EDAD ===\n")
tabla_integrada |>
  count(grupo_edad, estado_salud_oms)

cat("\n=== CLASIFICACIÓN POR PERCENTILES ===\n")
tabla_integrada |>
  count(grupo_edad, estado_salud_percentiles)

cat("\n=== COMPARACIÓN GENERAL ===\n")
tabla_integrada |>
  count(estado_salud_oms, estado_salud_percentiles) |>
  pivot_wider(names_from = estado_salud_percentiles, values_from = n, values_fill = 0)

#--------------------------------------------------------------------------------------------------------

# GRÁFICA DE PERCENTILES EXPLICATIVA

library(ggplot2)

# 1. GRÁFICA DE CAJAS (BOXPLOT) por grupo de edad
# Muestra la distribución del CPO-D en cada grupo etario
ggplot(tabla_integrada, aes(x = grupo_edad, y = cpo_individual, fill = grupo_edad)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = c(quantile(tabla_integrada$cpo_individual, c(0.20, 0.40, 0.60, 0.80))), 
             linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title = "Distribución del CPO-D por Grupo de Edad",
    subtitle = "Las líneas rojas muestran los percentiles 20, 40, 60 y 80 globales",
    x = "Grupo de Edad",
    y = "CPO-D Individual"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# 2. GRÁFICA DE PERCENTILES con líneas de corte (CORREGIDA)
tabla_integrada |>
  group_by(grupo_edad) |>
  summarise(
    n = n(),
    P20 = quantile(cpo_individual, 0.20),
    P40 = quantile(cpo_individual, 0.40),
    P60 = quantile(cpo_individual, 0.60),
    P80 = quantile(cpo_individual, 0.80)
  ) |>
  pivot_longer(cols = c(P20, P40, P60, P80), 
               names_to = "percentil", 
               values_to = "cpod_valor") |>
  ggplot(aes(x = grupo_edad, y = cpod_valor, color = percentil, group = percentil)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Puntos de Corte del CPO-D por Grupo de Edad (Percentiles)",
    subtitle = "Cada línea representa un percentil diferente",
    x = "Grupo de Edad",
    y = "Valor CPO-D",
    color = "Percentil"
  ) +
  scale_color_manual(
    values = c("P20" = "#27ae60", "P40" = "#3498db", "P60" = "#f39c12", "P80" = "#e74c3c"),
    labels = c("P20 (MUY BAJO/BAJO)", "P40 (BAJO/MODERADO)", 
               "P60 (MODERADO/ALTO)", "P80 (ALTO/MUY ALTO)")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# 3. TABLA RESUMEN con puntos de corte
cat("\n=== PUNTOS DE CORTE POR GRUPO DE EDAD ===\n")
tabla_integrada |>
  group_by(grupo_edad) |>
  summarise(
    n = n(),
    `Min` = min(cpo_individual),
    `P20 (MUY BAJO)` = quantile(cpo_individual, 0.20),
    `P40 (BAJO)` = quantile(cpo_individual, 0.40),
    `P60 (MODERADO)` = quantile(cpo_individual, 0.60),
    `P80 (ALTO)` = quantile(cpo_individual, 0.80),
    `Max` = max(cpo_individual),
    `Media` = round(mean(cpo_individual), 1)
  ) |>
  print(n = 20)

# 4. GRÁFICA DE DENSIDAD con áreas coloreadas por clasificación
# Ejemplo para un grupo específico (25-29 años)
tabla_integrada |>
  filter(grupo_edad == "25-29") |>
  ggplot(aes(x = cpo_individual, fill = estado_salud_percentiles)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = quantile(
    tabla_integrada$cpo_individual[tabla_integrada$grupo_edad == "25-29"], 
    c(0.20, 0.40, 0.60, 0.80)
  ), linetype = "dashed", color = "black") +
  labs(
    title = "Distribución del CPO-D para el Grupo 25-29 años",
    subtitle = "Las líneas verticales marcan los percentiles 20, 40, 60 y 80",
    x = "CPO-D Individual",
    y = "Densidad",
    fill = "Clasificación"
  ) +
  scale_fill_manual(
    values = c("MUY BAJO" = "#27ae60", "BAJO" = "#3498db", 
               "MODERADO" = "#f39c12", "ALTO" = "#e67e22", "MUY ALTO" = "#e74c3c")
  ) +
  theme_minimal()

# 5. HEATMAP: Comparación de clasificaciones
tabla_integrada |>
  count(estado_salud_oms, estado_salud_percentiles) |>
  ggplot(aes(x = estado_salud_oms, y = estado_salud_percentiles, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#3498db", high = "#e74c3c") +
  labs(
    title = "Comparación: Clasificación OMS vs Percentiles",
    subtitle = "Número de personas en cada combinación de clasificaciones",
    x = "Clasificación OMS",
    y = "Clasificación por Percentiles",
    fill = "Cantidad"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







# 5.3 Limpieza, estandarización de valores de derechohabiencia (normalización y recategorización)

# Función para estandarizar instituciones de derechohabiencia
tabla_integrada <- tabla_integrada |>
  mutate(
    institucion_derechohabiencia_std = case_when(
      # IMSS (incluyendo variantes)
      str_detect(str_to_upper(str_trim(institucion_derechohabiencia)), 
                 "^IMSS|^INSTITUTO MEXICANO DEL SEGURO|^SEGURO SOCIAL|^IMMS") ~ "IMSS",
      
      # ISSSTE
      str_detect(str_to_upper(str_trim(institucion_derechohabiencia)), 
                 "^ISS+TE|^INSTITUTO DE SEGURIDAD Y SERVICIOS SOCIALES") ~ "ISSSTE",
      
      # ISSEMYM
      str_detect(str_to_upper(str_trim(institucion_derechohabiencia)), 
                 "^ISSEMYM|^ISEMYM|^ISSEMYN|^ISSEMIN") ~ "ISSEMYM",
      
      # ISSFAM
      str_detect(str_to_upper(str_trim(institucion_derechohabiencia)), 
                 "^ISSFAM|^MILITAR") ~ "ISSFAM",
      
      # PEMEX
      str_detect(str_to_upper(str_trim(institucion_derechohabiencia)), 
                 "^PEMEX|^PETROLEOS") ~ "PEMEX",
      
      # Seguro Popular
      str_detect(str_to_upper(str_trim(institucion_derechohabiencia)), 
                 "SEGURO POPULAR") ~ "SEGURO POPULAR",
      
      # INSABI
      str_detect(str_to_upper(str_trim(institucion_derechohabiencia)), 
                 "INSABI") ~ "INSABI",
      
      # Sin derechohabiencia (incluye NA, SSA y múltiples variantes)
      is.na(institucion_derechohabiencia) | 
      str_detect(str_to_upper(str_trim(institucion_derechohabiencia)), 
                 "^NINGUN|^NO TIENE|^NO$|^N$|^NA$|^NO TENGO|^SIN |^NINGUNA|^SSA|^S/S|^N O TIENE|^NO PERTENECE|^0|^NIGUNO|^\\*+|^-|^NO CUENTA|^SECRETARIA DE SALUD") ~ "SIN DERECHOHABIENCIA",
      
      # Otros
      str_detect(str_to_upper(str_trim(institucion_derechohabiencia)), 
                 "^OTRO") ~ "OTROS",
      
      # Cualquier otro caso
      TRUE ~ "OTROS"
    )
  )

# Verificar resultado
tabla_integrada |>
  count(institucion_derechohabiencia_std, sort = TRUE)



# 5.4 Limpieza, estandarización de valores de escolaridad (norrmalización y recategorización)

# 5.4.1 Estandarizar escolaridad (versión mejorada)

tabla_integrada <- tabla_integrada |>
  mutate(
    escolaridad_std = case_when(
      # Sin escolaridad
      is.na(escolaridad) |
      str_detect(str_to_upper(str_trim(escolaridad)), 
                 "^SIN |^NINGUN|^NO$|^NINGUNA|^NO TIENE|^N$|^0$|^-+$|^\\.+$|^\\*|^NULA") ~ "SIN ESCOLARIDAD",
      
      # Preescolar / Jardín de niños
      str_detect(str_to_upper(str_trim(escolaridad)), 
                 "PREESCOLAR|JARDIN|KINDER") ~ "PREESCOLAR",
      
      # Primaria (completa e incompleta)
      str_detect(str_to_upper(str_trim(escolaridad)), 
                 "PRIMARIA|^1ER|^2DO|^3ER|^4TO|^5TO|^6TO") ~ "PRIMARIA",
      
      # Secundaria (incluye errores tipográficos)
      str_detect(str_to_upper(str_trim(escolaridad)), 
                 "SECUNDARIA|SECUENDARIA|SECUANDARIA|SCUNDARIA|SECUNDARÍA|TELESECUNDARIA") ~ "SECUNDARIA",
      
      # Preparatoria / Bachillerato / Media superior / Vocacional
      str_detect(str_to_upper(str_trim(escolaridad)), 
                 "PREPARATORIA|BACHILLERATO|BACHILLER|BACHILERATO|PREPA|MEDIA SUPERIOR|MEDIO SUPERIOR|CCH|CONALEP|CECYT|CETIS|CBTIS|VOCACIONAL") ~ "BACHILLERATO",
      
      # Carrera Técnica / Carrera Comercial / Técnico / Secretariado / Comercio
      str_detect(str_to_upper(str_trim(escolaridad)), 
                 "TECNIC|CARRERA TECNICA|CARRERA TÉCNICA|CARRERA COMERCIAL|TÉCNICO|TÉCNICA|COMERCIO|SECRETARIADO|SECRETARIA") ~ "CARRERA TÉCNICA",
      
      # Licenciatura / Universidad / Superior (incluye errores tipográficos)
      str_detect(str_to_upper(str_trim(escolaridad)), 
                 "LICENCIATURA|LINCENCIATURA|LICENCUATURA|LICENCITURA|LICENSIATURA|UNIVERSIDAD|UNIVERSITARI|PROFESIONAL|INGENIERIA|^LIC\\.|CARRERA PROFESIONAL|SUPERIOR|NIVEL SUPERIOR") ~ "LICENCIATURA",
      
      # Carrera trunca (incompleta)
      str_detect(str_to_upper(str_trim(escolaridad)), 
                 "TRUNCA") ~ "LICENCIATURA TRUNCA",
      
      # Posgrado (Maestría, Doctorado, Especialidad)
      str_detect(str_to_upper(str_trim(escolaridad)), 
                 "MAESTRIA|MAESTRA|MAESTRÍA|DOCTORADO|POSGRADO|ESPECIALIDAD") ~ "POSGRADO",
      
      # Otros
      TRUE ~ "OTROS"
    )
  )

# Verificar resultado
tabla_integrada |>
  count(escolaridad_std, sort = TRUE)

# 5.4.2 Coincidencia difusa para 2,021 caos categorizados en "OTROS".

# install.packages("stringdist")
install.packages("stringdist")
library(stringdist)

# Definir palabras clave por categoría para matching difuso
palabras_clave <- list(
  PRIMARIA = c("primaria", "primer", "segundo", "tercero", "cuarto", "quinto", "sexto"),
  SECUNDARIA = c("secundaria", "telesecundaria"),
  BACHILLERATO = c("preparatoria", "bachillerato", "prepa", "bachiller", "vocacional", "media", "superior"),
  `CARRERA TÉCNICA` = c("tecnica", "tecnico", "comercial", "secretariado", "comercio"),
  LICENCIATURA = c("licenciatura", "universidad", "universitaria", "profesional", "superior", "ingenieria"),
  POSGRADO = c("maestria", "doctorado", "posgrado", "especialidad"),
  PREESCOLAR = c("preescolar", "jardin", "kinder"),
  `SIN ESCOLARIDAD` = c("sin", "ninguna", "ninguno", "nula")
)

# Función de clasificación con fuzzy matching
clasificar_fuzzy_escolaridad <- function(texto) {
  if (is.na(texto) || texto == "") return("SIN ESCOLARIDAD")
  
  texto_limpio <- str_to_upper(str_trim(texto))
  texto_lower <- str_to_lower(texto_limpio)
  
  # Buscar mejor coincidencia en palabras clave
  mejor_categoria <- NULL
  mejor_distancia <- Inf
  
  for (categoria in names(palabras_clave)) {
    for (palabra in palabras_clave[[categoria]]) {
      # Calcular distancia Jaro-Winkler
      dist <- stringdist(texto_lower, palabra, method = "jw")
      
      if (dist < mejor_distancia) {
        mejor_distancia <- dist
        mejor_categoria <- categoria
      }
    }
  }
  
  # Si la mejor coincidencia es muy mala (> 0.35), marcar para revisión
  if (mejor_distancia > 0.35) {
    return("REVISAR MANUAL")
  }
  
  return(mejor_categoria)
}

# Aplicar fuzzy matching solo a "OTROS"
tabla_integrada <- tabla_integrada |>
  mutate(
    escolaridad_final = if_else(
      escolaridad_std == "OTROS",
      map_chr(escolaridad, clasificar_fuzzy_escolaridad),  # map_chr aplica la función a cada elemento
      escolaridad_std
    )
  )

# Ver resultados
tabla_integrada |>
  count(escolaridad_final, sort = TRUE)

# Ver cuántos quedaron para revisión manual
tabla_integrada |>
  filter(escolaridad_final == "REVISAR MANUAL") |>
  count(escolaridad, sort = TRUE) |>
  head(20)

# Contar registros antes de eliminar
antes_supr_rev_man <- nrow(tabla_integrada)

# Eliminar registros con "REVISAR MANUAL"
tabla_integrada <- tabla_integrada |>
  filter(escolaridad_final != "REVISAR MANUAL")

# Contar registros después de eliminar
despues_supr_rev_man <- nrow(tabla_integrada)

# Ver el resultado
cat("Registros antes:", antes_supr_rev_man, "\n")
cat("Registros después:", despues_supr_rev_man, "\n")
cat("Registros eliminados:", antes_supr_rev_man - despues_supr_rev_man, "\n")

# Verificar que ya no hay "REVISAR MANUAL"
tabla_integrada |>
  count(escolaridad_final, sort = TRUE)






# 8. ANÁLISIS EXPLORATORIO ----
# TODO: Análisis descriptivo por grupos
# TODO: Visualizaciones exploratorias

# 9. MODELADO ESTADÍSTICO ----
# TODO: Análisis de asociación entre derechohabiencia y pérdida dental
# TODO: Ajuste por variables de confusión

# 10. EXPORTAR DATOS PROCESADOS Y ANONIMIZADOS ----
# TODO: Guardar tabla final sin datos personales identificables

