### Exceso de mortalidad por ENTs en Argentina durante la pandemia de COVID-19
### Categorización de causas de muerte y reasignación de códigos basura cie-10
### Autores: Tamara Ricardo, Juan Irassar
### Fecha modificación:
# 2025-10-14 10:51:00

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
datos <- import("clean/defun_mensuales_arg_2010_2022.csv")


# Recategorizar causas de muerte -----------------------------------------
datos_causa <- datos |>
  # Reasignar causas de muerte
  mutate(
    causa_cat = case_when(
      ### COVID-19 ###
      between(cie10_cod, "U07.1", "U07.2") ~ "COVID-19",

      ### Enfermedad de Parkinson ###
      cie10_cod %in% c("G20.x", "G20.X") ~ "Enfermedad de Parkinson",

      ### Suicidio ###
      between(cie10_cod, "X60.0", "X84.9") ~ "Suicidio",

      ### Alzheimer y otras demencias ###
      between(cie10_cod, "F00.0", "F02.0") |
        between(cie10_cod, "F02.8", "F03.9") |
        between(cie10_cod, "G30.0", "G31.1") |
        between(cie10_cod, "G31.8", "G31.9") ~
        "Alzheimer y otras demencias",

      ### Trastornos por uso de alcohol ###
      between(cie10_cod, "F10.0", "F10.9") |
        between(cie10_cod, "K70.0", "K70.9") |
        between(cie10_cod, "K74.0", "K74.9") |
        between(cie10_cod, "K76.0", "K76.2") |
        between(cie10_cod, "K76.4", "K76.9") |
        between(cie10_cod, "X45.0", "X45.9") |
        between(cie10_cod, "Y15.0", "Y15.9") |
        cie10_cod == "G31.2" ~
        "Trastornos por uso de alcohol",

      ### Trastornos por uso de drogas ###
      between(cie10_cod, "F11.0", "F16.9") |
        between(cie10_cod, "F18.0", "F19.9") |
        between(cie10_cod, "X40.0", "X44.9") |
        between(cie10_cod, "Y10.0", "Y14.9") ~
        "Trastornos por uso de drogas",

      ### Diabetes Mellitus ###
      between(cie10_cod, "E10.0", "E11.9") &
        !cie10_cod %in% c("E10.2", "E11.2") ~ # Excluir complicaciones renales
        "Diabetes Mellitus",

      ### Enfermedades respiratorias crónicas ###
      between(cie10_cod, "J30.0", "J35.9") |
        between(cie10_cod, "J37.0", "J37.9") |
        between(cie10_cod, "J40.0", "J46.9") |
        between(cie10_cod, "J60.0", "J63.8") |
        between(cie10_cod, "J65.0", "J68.9") |
        between(cie10_cod, "J70.8", "J70.9") |
        between(cie10_cod, "J84.0", "J84.9") |
        between(cie10_cod, "J91.0", "J92.9") |
        between(cie10_cod, "D86.0", "D86.2") |
        cie10_cod %in% c("D86.9", "G47.3", "J70.0", "J82.0") ~
        "Enfermedades respiratorias crónicas",

      ### Enfermedades cardiovasculares ###
      between(cie10_cod, "I01.0", "I02.0") |
        between(cie10_cod, "I05.0", "I09.9") | # Enfermedades reumáticas del corazón
        between(cie10_cod, "I11.0", "I11.9") | # Enfermedad hipertensiva del corazón
        between(cie10_cod, "I20.0", "I25.9") | # Enfermedad isquémica del corazón
        between(cie10_cod, "I28.0", "I28.8") |
        between(cie10_cod, "I30.0", "I31.1") | # Pericarditis
        between(cie10_cod, "I31.8", "I37.8") |
        between(cie10_cod, "I38.0", "I41.9") |
        between(cie10_cod, "I42.1", "I42.8") |
        between(cie10_cod, "I43.0", "I43.9") |
        between(cie10_cod, "I47.0", "I48.9") |
        between(cie10_cod, "I51.0", "I51.4") |
        between(cie10_cod, "I60.0", "I63.9") |
        between(cie10_cod, "I65.0", "I67.3") |
        between(cie10_cod, "I67.5", "I67.6") |
        between(cie10_cod, "I68.0", "I68.2") |
        between(cie10_cod, "I69.0", "I69.3") |
        between(cie10_cod, "I70.2", "I70.8") |
        between(cie10_cod, "I71.0", "I73.9") |
        between(cie10_cod, "I77.0", "I83.9") |
        between(cie10_cod, "I86.0", "I89.0") |
        between(cie10_cod, "G45.0", "G46.8") |
        cie10_cod %in% c("B33.2", "I89.9", "I98.0", "K75.1") ~
        "Enfermedades cardiovasculares",

      ### Neoplasias ###
      between(cie10_cod, "C00.0", "C13.9") |
        between(cie10_cod, "C15.0", "C25.9") |
        between(cie10_cod, "C30.0", "C34.9") |
        between(cie10_cod, "C37.0", "C38.8") |
        between(cie10_cod, "C40.0", "C41.9") |
        between(cie10_cod, "C43.0", "C45.9") |
        between(cie10_cod, "C47.0", "C54.9") |
        between(cie10_cod, "C56.0", "C57.8") |
        cie10_cod == "C58" |
        between(cie10_cod, "C60.0", "C63.8") |
        between(cie10_cod, "C64.0", "C67.9") |
        between(cie10_cod, "C68.0", "C68.8") |
        between(cie10_cod, "C69.0", "C75.8") |
        between(cie10_cod, "C81.0", "C86.6") |
        between(cie10_cod, "C88.0", "C96.9") |
        between(cie10_cod, "D00.1", "D00.2") |
        between(cie10_cod, "D01.0", "D01.3") |
        between(cie10_cod, "D02.0", "D02.3") |
        between(cie10_cod, "D03.0", "D06.9") |
        between(cie10_cod, "D07.0", "D07.2") |
        between(cie10_cod, "D07.4", "D07.5") |
        cie10_cod == "D09.0" |
        between(cie10_cod, "D09.2", "D09.3") |
        cie10_cod == "D09.8" |
        between(cie10_cod, "D10.0", "D10.7") |
        between(cie10_cod, "D11.0", "D12.9") |
        between(cie10_cod, "D13.0", "D13.7") |
        between(cie10_cod, "D14.0", "D14.3") |
        between(cie10_cod, "D15.0", "D16.9") |
        between(cie10_cod, "D22.0", "D24.9") |
        between(cie10_cod, "D26.0", "D27.9") |
        between(cie10_cod, "D28.0", "D28.1") |
        cie10_cod == "D28.7" |
        between(cie10_cod, "D29.0", "D29.8") |
        between(cie10_cod, "D30.0", "D30.8") |
        between(cie10_cod, "D31.0", "D36.0") |
        between(cie10_cod, "D36.1", "D36.7") |
        between(cie10_cod, "D37.1", "D37.5") |
        between(cie10_cod, "D38.0", "D38.5") |
        between(cie10_cod, "D39.1", "D39.2") |
        cie10_cod == "D39.8" |
        between(cie10_cod, "D40.0", "D40.8") |
        between(cie10_cod, "D41.0", "D41.8") |
        between(cie10_cod, "D42.0", "D43.9") |
        between(cie10_cod, "D44.0", "D44.8") |
        between(cie10_cod, "D45.0", "D47.9") |
        between(cie10_cod, "D48.0", "D48.6") |
        between(cie10_cod, "D49.2", "D49.4") |
        cie10_cod == "D49.6" |
        between(cie10_cod, "K62.0", "K62.1") |
        cie10_cod == "K63.5" |
        between(cie10_cod, "N60.0", "N60.9") |
        between(cie10_cod, "N84.0", "N84.1") |
        between(cie10_cod, "N87.0", "N87.9") ~
        "Neoplasias",
    )
  )
