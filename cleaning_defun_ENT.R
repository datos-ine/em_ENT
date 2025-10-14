### Exceso de mortalidad por ENTs en Argentina durante la pandemia de COVID-19
### Limpieza del dataset: Defunciones Generales Mensuales ocurridas y
### registradas en la República Argentina (2010-2022)
### Autora: Tamara Ricardo
### Fecha modificación:
# 2025-10-14 10:25:43

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  geoAr,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## Códigos provincias ----
cod_prov <- show_arg_codes() |>
  # Filtrar totales
  filter(between(codprov, "01", "24")) |>

  # Códigos a numérico
  mutate(across(
    .cols = c(codprov, codprov_censo),
    .fns = ~ parse_number(.x)
  )) |>

  # Cambiar etiqueta CABA
  mutate(prov_nombre = if_else(codprov_censo == 2, "CABA", name_iso))


## Defunciones mensuales (DEIS-MSAL) ----
datos_raw <- bind_rows(
  import("raw/base_def_10_15_men_4dig.csv"), # 2010-2015
  import("raw/base_def_16_20_men_4dig.csv"), # 2016-2021
  import("raw/base_def_21_23_men_4dig.csv") # 2022-2023
)


## Explorar datos crudos
glimpse(datos_raw)

tabyl(datos_raw$region)

tabyl(datos_raw$jurisdiccion)

tabyl(datos_raw$sexo_id)

tabyl(datos_raw$grupo_etario)

tabyl(datos_raw$grupo_causa_defuncion_CIE10)


# Limpiar dataset defunciones --------------------------------------------
datos <- datos_raw |>
  # Estandarizar nombres de columnas
  rename(
    sexo = sexo_id,
    cie10_grupo = grupo_causa_defuncion_CIE10,
    cie10_cod = cod_causa_muerte_CIE10
  ) |>

  # Filtrar defunciones 2009 y 2023
  filter(between(anio_def, 2010, 2022)) |>

  # Filtrar datos ausentes región geográfica
  filter(region != "10.sin especificar.") |>

  # Filtrar datos ausentes sexo
  filter(between(sexo, 1, 2)) |>

  # Filtrar menores de 20 años y datos ausentes grupo etario
  filter(between(
    grupo_etario,
    "02.de 20 a 39 años",
    "07.de 80 años y más"
  )) |>

  # Modificar código CIE10
  mutate(
    cie10_cod = paste0(str_sub(cie10_cod, 1, 3), ".", str_sub(cie10_cod, 4))
  ) |>

  # Modificar etiquetas sexo
  mutate(sexo = if_else(sexo == 1, "Masculino", "Femenino")) |>

  # Modificar etiquetas grupo etario
  mutate(
    grupo_etario = str_sub(grupo_etario, 7) |>
      fct_recode("80+ años" = "80 años y más")
  ) |>

  # Modificar etiquetas región
  mutate(
    region = fct_relabel(
      region,
      ~ c(
        "Centro",
        "NEA",
        "NOA1",
        "NOA",
        "NOA2",
        "Cuyo1",
        "Cuyo2",
        "Patagonia Norte",
        "Patagonia Sur"
      )
    )
  ) |>

  # Jurisdicción a numérico
  mutate(
    prov_id = str_remove(jurisdiccion, "\\..*") |>
      parse_number()
  ) |>

  # Crear variable para nombre de provincia
  left_join(
    cod_prov |>
      select(prov_id = codprov_censo, prov_nombre)
  ) |>

  # Crear columna para jurisdicción categorizada
  mutate(
    prov_cat = if_else(
      !is.na(prov_nombre),
      prov_nombre,
      region
    )
  ) |>

  # Agrupar datos
  count(
    anio_def,
    mes_def,
    prov_cat,
    region,
    sexo,
    grupo_etario,
    cie10_grupo,
    cie10_cod,
    wt = cantidad
  )


# Exportar datos ---------------------------------------------------------
export(
  datos,
  "clean/defun_mensuales_arg_2010_2022.csv"
)
