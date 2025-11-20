### Exceso de Mortalidad por ENT en Argentina durante la pandemia de COVID-19
### Limpieza códigos ICD-10 para ENT y garbage code según categorías GBD (2023)
### Autora: Tamara Ricardo
# Última modificación: 20-11-2025 08:58

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
icd10_raw <- import(
  "raw/IHME_GBD_2021_COD_CAUSE_ICD_CODE_MAP_Y2024M05D16_0.XLSX",
  skip = 1
)


# Limpiar dataset --------------------------------------------------------
icd10 <- icd10_raw |>
  # Estandarizar nombres columnas
  clean_names() |>

  # Descartar códigos ICD9
  select(-icd9) |>

  # Extraer grandes grupos ENT y GC nivel 3 y 4
  filter(
    cause == "Neoplasms" | # Cáncer
      cause == "Cardiovascular diseases" | # Enfermedad cardiovascular
      cause == "Chronic respiratory diseases" | # Enfermedad respiratoria crónica
      cause == "Diabetes mellitus" | # Diabetes
      cause == "Parkinson's disease" | # Parkinson
      cause == "Alzheimer's disease and other dementias" | # Alzheimer y demencias
      cause == "Alcohol use disorders" | # Alcoholismo
      cause == "Drug use disorders" | # Uso de drogas
      cause == "Self-harm" | # Suicidio
      cause == "COVID-19" | # COVID-19
      str_detect(cause, "Garbage") # Códigos basura
  ) |>

  # Separar en filas
  separate_rows(icd10, sep = ", ") |>

  # Corregir código mal cargado
  mutate(icd10 = if_else(icd10 == "I99-ID5.9", "I99.0", icd10)) |>

  # Separar inicio y fin secuencia
  separate_wider_delim(
    icd10,
    delim = "-",
    names = c("start", "end"),
    too_few = "align_start",
    cols_remove = FALSE
  ) |>

  # Definir NAs
  mutate(across(
    .cols = c(start, end),
    .fns = ~ na_if(.x, "unsp.")
  )) |>

  # Si start tiene 3 dígitos añadir .0
  mutate(
    start = if_else(
      nchar(start) == 3,
      paste0(start, ".0"),
      start
    )
  ) |>

  # Si end tiene menos de 5 dígitos añadir .9
  mutate(
    end = case_when(
      nchar(end) == 3 ~ paste0(end, ".9"),
      nchar(end) == 4 ~ paste0(end, "9"),
      is.na(end) ~ coalesce(end, start),
      .default = end
    )
  ) |>

  # Base long
  pivot_longer(c(start, end)) |>

  # Separar código en número y letra
  separate_wider_position(value, widths = c(letra = 1, numero = 4)) |>

  # Número a formato numérico
  mutate(numero = parse_number(numero)) |>

  # Añadir filas para cuando cambia la letra (G99-H05)
  add_row(
    cause = rep("Garbage Code (GBD Level 1)", 2),
    icd10 = rep("G99-H05", 2),
    name = c("end", "start"),
    letra = c("G", "H"),
    numero = c(99.9, 0.0)
  ) |>

  # Añadir filas para cuando cambia la letra (S00-T98.3)
  add_row(
    cause = rep("Garbage Code (GBD Level 2)", 2),
    icd10 = rep("S00-T98.3", 2),
    name = c("end", "start"),
    letra = c("S", "T"),
    numero = c(99.9, 0.0)
  ) |>

  # Añadir filas para cuando cambia la letra (C97-D00.0)
  add_row(
    cause = rep("Garbage Code (GBD Level 3)", 2),
    icd10 = rep("C97-D00.0", 2),
    name = c("end", "start"),
    letra = c("C", "D"),
    numero = c(99.9, 0.0)
  ) |>

  # Añadir filas para cuando cambia la letra (Q99.9-R01.2)
  add_row(
    cause = rep("Garbage Code (GBD Level 3)", 2),
    icd10 = rep("Q99.9-R01.2", 2),
    name = c("end", "start"),
    letra = c("Q", "R"),
    numero = c(99.9, 0.0)
  ) |>

  # Generar secuencia códigos
  group_by(cause, icd10, letra) |>
  summarise(
    numero = list(seq(
      from = numero[name == "start"],
      to = numero[name == "end"],
      by = 0.1
    )),
    .groups = "drop"
  ) |>

  # Expandir lista
  unnest(numero) |>

  # Regenerar códigos
  mutate(cie10_cod = paste0(letra, sprintf("%04.1f", numero)))


# Exportar datos limpios -------------------------------------------------
export(icd10, file = "clean/ihme_gbd_2021_codes_clean.csv")


## Limpiar environment
rm(list = ls())

pacman::p_unload("all")
