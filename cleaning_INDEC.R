### Exceso de mortalidad por ENT en Argentina durante la pandemia de COVID-19
### Limpieza de los datasets de proyecciones poblacionales 2010-2040 y población
### según censos nacionales 2010 y 2022
### Autora: Tamara Ricardo
# Última modificación: 18-11-2025 12:36

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  zoo,
  tidyverse,
  readxl
)


# Cargar datos ------------------------------------------------------------
## Proyecciones 2010-2021
proy_10_23_raw <- "raw/c2_proyecciones_prov_2010_2040.xls"

## Población Estándar Censo 2022 (INDEC)
pob_22_raw <- import("raw/_tmp_7830741.xlsX", range = "B10:D680")


# Limpiar datos ----------------------------------------------------------
## Proyecciones 2010-2023 ----
proy_10_23 <- # Proyecciones 2010-2015
  excel_sheets(proy_10_23_raw)[-c(1:2)] |>

  # Crear columna para provincia
  set_names() |>

  # Unir por provincia
  map(~ read_excel(proy_10_23_raw, sheet = .x, range = "A3:X28")) |>
  list_rbind(names_to = "prov") |>

  # Añadir proyecciones 2016-2021
  bind_cols(
    excel_sheets(proy_10_23_raw)[-c(1:2)] |>

      # Crear columna para provincia
      set_names() |>

      # Unir por provincia
      map(~ read_excel(proy_10_23_raw, sheet = .x, range = "A31:X56")) |>
      list_rbind(names_to = "prov")
  ) |>

  # Añadir proyecciones 2022-2023
  bind_cols(
    excel_sheets(proy_10_23_raw)[-c(1:2)] |>

      # Crear columna para provincia
      set_names() |>

      # Unir por provincia
      map(~ read_excel(proy_10_23_raw, sheet = .x, range = "A59:H84")) |>
      list_rbind(names_to = "prov")
  ) |>

  # Estandarizar nombres de columnas
  clean_names() |>

  # Seleccionar columnas relevantes
  select(
    prov_id = prov_1,
    grupo_edad_5 = edad_2,
    Masculino_2010 = x4,
    Femenino_2010 = x5,
    Masculino_2011 = x8,
    Femenino_2011 = x9,
    Masculino_2012 = x12,
    Femenino_2012 = x13,
    Masculino_2013 = x16,
    Femenino_2013 = x17,
    Masculino_2014 = x20,
    Femenino_2014 = x21,
    Masculino_2015 = x24,
    Femenino_2015 = x25,
    Masculino_2016 = x29,
    Femenino_2016 = x30,
    Masculino_2017 = x33,
    Femenino_2017 = x34,
    Masculino_2018 = x37,
    Femenino_2018 = x38,
    Masculino_2019 = x41,
    Femenino_2019 = x42,
    Masculino_2020 = x45,
    Femenino_2020 = x46,
    Masculino_2021 = x49,
    Femenino_2021 = x50,
    Masculino_2022 = x54,
    Femenino_2022 = x55,
    Masculino_2023 = x58,
    Femenino_2023 = x59,
  ) |>

  # Crear id numérico de provincia
  mutate(prov_id = parse_number(prov_id)) |>

  # Filtrar filas con NAs
  drop_na() |>

  # Filtrar menores de 20 años y totales
  filter(!grupo_edad_5 %in% c("0-4", "5-9", "10-14", "15-19", "Total")) |>

  # Pasar a formato long
  pivot_longer(
    cols = c(Masculino_2010:Femenino_2023),
    values_to = "pob"
  ) |>

  # Separar año y sexo
  separate_wider_delim(name, delim = "_", names = c("sexo", "anio"))


## Población Censo 2022 ----
pob_22 <- pob_22_raw |>
  # Estandarizar nombre de columnas
  clean_names() |>
  rename(
    grupo_edad_5 = x1,
    Femenino = x2,
    Masculino = x3
  ) |>

  # Crear id numérico de provincia
  mutate(
    prov_id = if_else(
      str_detect(grupo_edad_5, "AREA"),
      parse_number(grupo_edad_5),
      NA
    )
  ) |>

  # Completar filas
  fill(prov_id, .direction = "down") |>

  # Quitar NAs
  drop_na() |>

  # Filtrar menores de 20 años y totales
  filter(
    between(grupo_edad_5, "20 a 24", "95 a 99") |
      between(grupo_edad_5, "100 a 104", "105 y más")
  ) |>

  # Pasar a formato long
  pivot_longer(
    cols = c(Femenino, Masculino),
    names_to = "sexo",
    values_to = "pob_est_2022"
  )


# Unir los datasets ------------------------------------------------------
pob_2010_2023 <- proy_10_23 |>
  # Añadir población estándar 2022
  left_join(pob_22) |>

  # Crear grupo etario ampliado
  mutate(
    grupo_edad = case_when(
      between(grupo_edad_5, "20-24", "35-39") ~ "20 a 39 años",
      between(grupo_edad_5, "40-44", "45-49") ~ "40 a 49 años",
      between(grupo_edad_5, "50-54", "55-59") ~ "50 a 59 años",
      between(grupo_edad_5, "60-64", "65-69") ~ "60 a 69 años",
      between(grupo_edad_5, "70-74", "75-79") ~ "70 a 79 años",
      .default = "80+ años"
    )
  ) |>

  # Población y año a numérico
  mutate(across(
    .cols = c(anio, pob, pob_est_2022),
    .fns = ~ parse_number(.x, na = "-")
  )) |>

  # Reagrupar datos
  group_by(prov_id, anio, grupo_edad, sexo) |>
  summarise(
    poblacion = sum(pob, na.rm = TRUE),
    pob_est_2022 = sum(pob_est_2022, na.rm = TRUE),
    .groups = "drop"
  )


# Estimar población mensual ----------------------------------------------
pob_mensual <- pob_2010_2023 |>
  # Expandir datset
  expand(
    prov_id,
    grupo_edad,
    sexo,
    fecha = seq.Date(
      from = ymd("2010-01-01"),
      to = ymd("2023-12-01"),
      by = "month"
    )
  ) |>

  # Crear columna para el año
  mutate(
    anio = year(fecha),
    mes = month(fecha)
  ) |>

  # Unir con base población anual
  left_join(
    pob_2010_2023 |>
      mutate(fecha = make_date(anio))
  ) |>

  # Agrupar datos
  group_by(prov_id, grupo_edad, sexo) |>

  # Ordenar por fecha
  arrange(fecha) |>

  # Interpolación lineal
  mutate(
    pob_mensual = na.approx(poblacion, x = fecha, na.rm = FALSE)
  ) |>
  ungroup() |>

  # Quitar columnas no relevantes
  select(anio, mes, prov_id, grupo_edad, sexo, pob_mensual) |>

  # Quitar filas de 2023
  filter(anio < 2023)


# Guardar datos limpios --------------------------------------------------
## Proyecciones anuales y población estándar 2022
pob_2010_2023 |>
  filter(anio < 2023) |>
  export(file = "clean/indec_pob_2010_2022.csv")


## Proyecciones mensuales
pob_mensual |>
  export(file = "clean/indec_pob_mensual_2010_2022.csv")


## Limpiar environment
rm(list = ls())

pacman::p_unload("all")
