### Exceso de mortalidad por ENTs en Argentina durante la pandemia de COVID-19
### Categorización de causas de muerte y reasignación de códigos basura CIE-10
### según tabla GBD 2023 y metodología propuesta por Teixeira et al. (2023)
### Autora: Tamara Ricardo
# Última modificación: 20-11-2025 12:43

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## Códigos CIE-10 2021 (GBD)
codigos_cie10 <- import("clean/gbd_2023_codes.csv")

## Proyecciones poblacionales mensuales (INDEC)
proy_mensual <- import("clean/indec_pob_mensual_2010_2022.csv")

## Mortalidad mensual en Argentina
defun <- import("clean/deis_defun_mensuales_2010_2022.csv")


# Explorar mortalidad por grupo de causas --------------------------------
defun |>
  tabyl(cie10_grupo) |>
  adorn_pct_formatting()


# Recategorizar causas de muerte -----------------------------------------
datos <- defun |>
  # Añadir códigos CIE10 - GBD 2023
  left_join(
    codigos_cie10 |>
      select(-letra, -numero)
  ) |>

  # Cambiar etiquetas categorías
  mutate(
    cie10_gbd_cat = case_when(
      str_detect(cause, "Alcohol") ~ "Alcoholismo",
      str_detect(cause, "Alzheimer") ~ "Alzheimer/demencias",
      str_detect(cause, "Cardiovascular") ~ "Cardiovascular",
      str_detect(cause, "Chronic") ~ "Respiratoria crónica",
      str_detect(cause, "Drug") ~ "Uso de drogas",
      str_detect(cause, "Neoplasms") ~ "Neoplasias",
      str_detect(cause, "Parkinson") ~ "Parkinson",
      str_detect(cause, "Self-harm") ~ "Suicidio",
      is.na(cause) ~ "Otras causas",
      .default = cause
    )
  )
