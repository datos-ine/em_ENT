### Exceso de mortalidad por ENT en Argentina durante la pandemia de COVID-19
### Limpieza de los datasets de proyecciones poblacionales 2010-2040 y población
### según Censo Nacional 2022
### Autora: Tamara Ricardo
# Última modificación: 20-11-2025 09:45

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
proy_2010_2023_raw <- "raw/c2_proyecciones_prov_2010_2040.xls"

## Población Estándar Censo 2022 (INDEC)
pob_2022_raw <- import("raw/_tmp_7830741.xlsX", range = "B10:D680")


# Limpiar datos ----------------------------------------------------------
## Proyecciones 2010-2023 ----
proy_2010_2023 <- map(
  c("A3:X28", "A31:X56", "A59:H84"),
  ~ excel_sheets(proy_2010_2023_raw)[-c(1:2)] |>
    set_names() |>
    map(\(x) read_excel(proy_2010_2023_raw, sheet = x, range = .x)) |>
    list_rbind(names_to = "prov")
) |>
  list_cbind() |>

  # Estandarizar nombres de columnas
  clean_names() |>

  # Filtrar columnas con totales y repetidas
  select(
    -starts_with("x201"),
    -starts_with("x202"),
    -(starts_with("prov_") & !matches("prov_1$")),
    -(starts_with("edad_") & !matches("edad_2$"))
  ) |>

  # Filtrar columnas vacías
  remove_empty(which = "cols") |>

  # Renombrar columnas
  rename(
    prov_id = prov_1,
    grupo_edad_5 = edad_2
  ) |>

  rename_with(
    .cols = c(x4:x59),
    .fn = ~ paste0(
      rep(c("Masculino", "Femenino"), length(.x) / 2),
      "_",
      rep(2010:2023, each = 2)
    )
  ) |>

  # Crear id numérico de provincia
  mutate(prov_id = parse_number(prov_id)) |>

  # Filtrar filas con NAs
  drop_na() |>

  # Filtrar menores de 20 años y totales
  filter(!grupo_edad_5 %in% c("0-4", "5-9", "10-14", "15-19", "Total")) |>

  # Pasar a formato long
  pivot_longer(cols = c(Masculino_2010:Femenino_2023)) |>

  # Separar año y sexo
  separate_wider_delim(name, delim = "_", names = c("sexo", "anio")) |>

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
  mutate(across(.cols = c(anio, value), .fns = ~ parse_number(.x))) |>

  # Reagrupar datos
  count(anio, prov_id, grupo_edad, sexo, wt = value, name = "proy_pob")


## Población Censo 2022 ----
pob_2022 <- pob_2022_raw |>
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

  # Crear id categórico de provincia
  mutate(
    prov_nombre = if_else(
      str_detect(grupo_edad_5, "AREA"),
      Femenino,
      NA
    ) |>
      str_replace("Caba", "CABA")
  ) |>

  # Completar filas
  fill(c(prov_id, prov_nombre), .direction = "down") |>

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
    names_to = "sexo"
  ) |>

  # Crear grupo etario ampliado
  mutate(
    grupo_edad = case_when(
      between(grupo_edad_5, "20 a 24", "35 a 39") ~ "20 a 39 años",
      between(grupo_edad_5, "40 a 44", "45 a 49") ~ "40 a 49 años",
      between(grupo_edad_5, "50 a 54", "55 a 59") ~ "50 a 59 años",
      between(grupo_edad_5, "60 a 64", "65 a 69") ~ "60 a 69 años",
      between(grupo_edad_5, "70 a 74", "75 a 79") ~ "70 a 79 años",
      .default = "80+ años"
    )
  ) |>

  # Población a numérico
  mutate(value = parse_number(value, na = "-")) |>

  # Reagrupar datos
  count(
    prov_id,
    prov_nombre,
    grupo_edad,
    sexo,
    wt = value,
    name = "pob_est_2022"
  )


# Unir los datasets ------------------------------------------------------
pob_2010_2023 <- proy_2010_2023 |>
  # Añadir población estándar 2022
  left_join(pob_2022) |>

  # Filtrar filas de 2023
  filter(anio < 2023) |>

  # Ordenar columnas
  select(anio, starts_with("prov_"), everything())


# Estimar población mensual ----------------------------------------------
pob_mensual <- proy_2010_2023 |>
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
    proy_2010_2023 |>
      mutate(fecha = make_date(anio))
  ) |>

  # Agrupar datos
  group_by(prov_id, grupo_edad, sexo) |>

  # Ordenar por fecha
  arrange(fecha) |>

  # Interpolación lineal
  mutate(
    proy_pob_mensual = na.approx(proy_pob, x = fecha, na.rm = FALSE)
  ) |>
  ungroup() |>

  # Añadir población estándar 2022
  left_join(pob_2022) |>

  # Quitar filas de 2023
  filter(anio < 2023) |>

  # Quitar columnas no relevantes
  select(
    anio,
    mes,
    prov_id,
    grupo_edad,
    sexo,
    proy_pob_mensual,
    pob_est_2022
  )


# Guardar datos limpios --------------------------------------------------
## Proyecciones anuales y población estándar 2022
export(pob_2010_2023, file = "clean/indec_pob_anual_2010_2022.csv")


## Proyecciones mensuales
export(pob_mensual, file = "clean/indec_pob_mensual_2010_2022.csv")


## Limpiar environment
rm(list = ls())

pacman::p_unload("all")
