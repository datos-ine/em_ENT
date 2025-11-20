### Exceso de Mortalidad por ENT en Argentina durante la pandemia de COVID-19
### Limpieza códigos ICD-10 para ENT y garbage code según categorías GBD (2023)
### Autora: Tamara Ricardo
# Última modificación: 20-11-2025 12:34

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## Códigos CIE10 - GBD 2023
icd10_raw <- import(
  "raw/IHME_GBD_2021_COD_CAUSE_ICD_CODE_MAP_Y2024M05D16_0.XLSX",
  skip = 1
)

## Códigos CIE10 - DEIS
deis <- import("clean/deis_defun_mensuales_2010_2022.csv") |>
  select(starts_with("cie"))


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

  # Filtrar códigos repetidos
  filter(
    !icd10 %in%
      c(
        "C83",
        "C68",
        "C69",
        "D01",
        "D07",
        "D13",
        "D36.0",
        "D44",
        "D48",
        "I27",
        "I50",
        "I51",
        "I67",
        "J81",
        "K92",
        "N84",
        "N95",
        "R07",
        "R58"
      )
  ) |>

  # Corregir códigos mal cargados
  mutate(
    icd10 = case_when(
      icd10 == "A47-A48" ~ "A47.0-A47.9",
      icd10 == "A48.8-A49" ~ "A48.8-A48.9",
      icd10 == "B92-B94" ~ "B92.0-B93.9",
      icd10 == "D01.4-D02" ~ "D01.4-D01.9",
      icd10 == "D07.6-D09" ~ "D07.6-D08.9",
      icd10 == "D09.9-D10" ~ "D09.9",
      icd10 == "D13.9-D14" ~ "D13.9",
      icd10 == "D28.9-D29" ~ "D28.9",
      icd10 == "D29.9-D30" ~ "D29.9",
      icd10 == "D31-D36" ~ "D31.0-D35.9",
      icd10 == "D37.6-D38" ~ "D37.6-D37.9",
      icd10 == "D39.9-D40" ~ "D39.9",
      icd10 == "D40.9-D41" ~ "D40.9",
      icd10 == "D63.8-D64" ~ "D63.8-D64.0",
      icd10 == "G91.4-G93" ~ "G91.4-G92.9",
      icd10 == "G99-H05" ~ "G99.0-H05.1",
      icd10 == "I68.8-I69" ~ "I68.8",
      icd10 == "I99-ID5.9" ~ "I99.0",
      icd10 == "M65.1-M71" ~ "M65.1-M71.1",
      icd10 == "R03.1-R04" ~ "R03.1-R03.9",
      icd10 == "R19.8-R23" ~ "R19.8-R22.9",
      icd10 == "R74-R78" ~ "R74.0-R77.9",
      icd10 == "Y86-Y87" ~ "Y86.0-Y86.9",
      .default = icd10
    )
  ) |>

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
    icd10 = rep("G99.0-H05.1", 2),
    name = c("end", "start"),
    letra = c("G", "H"),
    numero = c(99.9, 0.1)
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


# Probar merge con dataset defunciones -----------------------------------
left_join(deis, icd10) |> warnings()


## Códigos repetidos/mal cargados
deis[40, "cie10_cod"] # J81.0: edema pulmonar
deis[83, "cie10_cod"] # G93.2: hipertensión intracraneal benigna
deis[129, "cie10_cod"] # I50.0: falla cardíaca congestiva
deis[404, "cie10_cod"] # K92.0: Haematemesis
deis[676, "cie10_cod"] # I69.0: Secuelas de hemorragia subaracnoidea
deis[726, "cie10_cod"] # D64.9: Anemia no especificada
deis[1008, "cie10_cod"] # R04.2: Haemoptysis
deis[1052, "cie10_cod"] # D38.3: Mediastinum
deis[1406, "cie10_cod"] # I27.0: Hipertensión pulmonar primaria
deis[1944, "cie10_cod"] # D41.0: Neoplasia de riñón
deis[4082, "cie10_cod"] # R58.0: Hemorragia no clasificada
deis[4262, "cie10_cod"] # A48.0: Gas gangrene
deis[5103, "cie10_cod"] # Y87.2: Secuelas de daño autoinfligido
deis[7733, "cie10_cod"] # D40.1: Neoplasia de testículo
deis[8007, "cie10_cod"] # A49.8: Otras infecciones bacterianas
deis[9079, "cie10_cod"] # I51.0: Cardiac septal defect, acquired
deis[9326, "cie10_cod"] # R07.0: Dolor de garganta
deis[9745, "cie10_cod"] # D48.0: Neoplasia de hueso y cartílago
deis[9894, "cie10_cod"] # D29.1: Neoplasia de próstata
deis[18798, "cie10_cod"] # C83.0: Small cell B-cell lymphoma
deis[19592, "cie10_cod"] # I67.0: Dissection of cerebral arteries, nonruptured
deis[20320, "cie10_cod"] # D44.0: Neoplasia de tiroides
deis[20320, "cie10_cod"] # D44.0: Neoplasia de tiroides
deis[32407, "cie10_cod"] # D36.7: Neoplasia de otros sitios específicos
deis[34984, "cie10_cod"] # C68.0: Cáncer de uretra
deis[65296, "cie10_cod"] # C69.0: Cáncer de conjuntiva
deis[67900, "cie10_cod"] # D30.0: Neoplasia de riñón
deis[113117, "cie10_cod"] # D09.9: Carcinoma in situ no especificado
deis[267390, "cie10_cod"] # D02.2: Carcinoma de bronquio y pulmón
deis[354010, "cie10_cod"] # B94.8: Secuelas de enf. infecciosas o parasitarias
deis[362016, "cie10_cod"] # N84.0: Polyp of corpus uteri
deis[373712, "cie10_cod"] # M71.2: Synovial cyst of popliteal space
deis[440539, "cie10_cod"] # R23.0: Cyanosis
deis[482906, "cie10_cod"] # N95.0: Sangrado postmenopausia
deis[502762, "cie10_cod"] # D01.0: Carcinoma de colon
deis[878972, "cie10_cod"] # D13.0: Neoplasia de esófago
deis[1114959, "cie10_cod"] # D07.0: Carcinoma de endometrio
deis[1176066, "cie10_cod"] # D10.3: Neoplasma de boca no especificado
deis[1176576, "cie10_cod"] # H05.5: Retrobulbar foreign body
deis[1244896, "cie10_cod"] # R78.0: Alcohol en sangre


# Exportar datos limpios -------------------------------------------------
export(icd10, file = "clean/gbd_2023_codes.csv")


## Limpiar environment
rm(list = ls())

pacman::p_unload("all")
