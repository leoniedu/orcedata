## code to prepare `geobrcache` dataset goes here
library(dplyr)
library(orce)
source("R/rename_ibge.R") ## needed rename_ibge
ufs <- geobr::read_state(year = 2020) %>%
  sf::st_centroid() %>%
  orce::add_coordinates(lat = "uf_lat", lon = "uf_lon") %>%
  rename_ibge()

usethis::use_data(ufs, overwrite = TRUE)


setores2022_map <- geobr::read_census_tract(year = 2022, code_tract = "all")
readr::write_rds(setores2022_map, "data-raw/setores2022_map.rds")


setores2010_map <- geobr::read_census_tract(year = 2010, code_tract = "all")
readr::write_rds(setores2010_map, "data-raw/setores2010_map.rds")


municipios_map <- geobr::read_municipality(year = 2024)
readr::write_rds(municipios_map, "data-raw/municipios_map.rds")

municipios_geo <- geobr::read_municipality(year = 2024) %>%
  sf::st_centroid() %>%
  rename_ibge()
# Estimativas de população dos municípios (SIDRA tabela 6579)
pop <- sidrar::get_sidra(
  6579,
  variable = 9324,
  period = "last",
  geo = "City"
) %>%
  transmute(
    municipio_codigo = `Município (Código)`,
    municipio_populacao = Valor
  )

load(here::here("data/pontos_municipios.rda"))
pontos_municipios_sede_0 <- geobr::read_municipal_seat(year = "2010")
pontos_municipios_sede_1 <- pontos_municipios_sede_0 %>%
  transmute(municipio_codigo = as.character(code_muni)) %>%
  dplyr::rename(geometry = geom)
## faltando: usar cnefe, depois centroide do geobr como fallback
existing_codes <- pontos_municipios_sede_1 %>% sf::st_drop_geometry() %>% pull(municipio_codigo)

pontos_municipios_sede <- pontos_municipios_sede_1 %>%
  # fallback 1: pontos CNEFE para municípios sem sede
  bind_rows(
    pontos_municipios %>%
      select(municipio_codigo) %>%
      filter(!municipio_codigo %in% existing_codes)
  )

existing_codes <- pontos_municipios_sede %>% sf::st_drop_geometry() %>% pull(municipio_codigo)

pontos_municipios_sede <- pontos_municipios_sede %>%
  # fallback 2: centroide geobr para municípios sem sede nem CNEFE
  bind_rows(
    municipios_geo %>%
      select(municipio_codigo) %>%
      filter(!municipio_codigo %in% existing_codes)
  ) %>%
  orce::add_coordinates(lat = "municipio_sede_lat", lon = "municipio_sede_lon")

municipios <- pontos_municipios_sede %>%
  left_join(
    municipios_geo %>%
      sf::st_drop_geometry()
  ) %>%
  left_join(pop, by = "municipio_codigo")


usethis::use_data(municipios, overwrite = TRUE)
