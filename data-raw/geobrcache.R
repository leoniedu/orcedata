## code to prepare `geobrcache` dataset goes here
library(dplyr)
source("R/rename_ibge.R") ## needed rename_ibge
source("R/add_coordinates.R") ## needed rename_ibge
ufs <- geobr::read_state(year = 2020)%>%
  sf::st_centroid()%>%
  add_coordinates(lat = "uf_lat", lon = "uf_lon")%>%
  rename_ibge()

usethis::use_data(ufs, overwrite = TRUE)


setores2022_map <- geobr::read_census_tract(year=2022, code_tract = "all")
readr::write_rds(setores2022_map, "data-raw/setores2022_map.rds")


setores2010_map <- geobr::read_census_tract(year=2010, code_tract = "all")
readr::write_rds(setores2010_map, "data-raw/setores2010_map.rds")


municipios2022_map <- geobr::read_municipality(year=2022)
readr::write_rds(municipios2022_map, "data-raw/municipios2022_map.rds")

municipios2022 <- geobr::read_municipality(year=2022)%>%
  sf::st_centroid()%>%
  rename_ibge()
pop2022 <- censobr::read_tracts(year=2022, dataset = "Preliminares")%>%
  arrow::as_arrow_table()%>%
  rename_ibge()%>%
  group_by(municipio_codigo)%>%
  summarise(municipio_populacao=sum(v0001))%>%
  collect()

load(here::here("data/pontos_municipios.rda"))
pontos_municipios_sede_0 <- geobr::read_municipal_seat(year = "2010")
pontos_municipios_sede_1 <- pontos_municipios_sede_0%>%
  transmute(municipio_codigo=as.character(code_muni))%>%
  dplyr::rename(geometry=geom)
## tem alguns faltando. usa base do cnefe
pontos_municipios_sede <- pontos_municipios_sede_1%>%
  bind_rows(
    pontos_municipios%>%
      select(municipio_codigo)%>%
      anti_join(pontos_municipios_sede_1%>%sf::st_drop_geometry(), by="municipio_codigo")
  )%>%
  add_coordinates(lat = "municipio_sede_lat", lon = "municipio_sede_lon")

municipios_22 <- pontos_municipios_sede%>%
  left_join(municipios2022%>%
  sf::st_drop_geometry())%>%
  full_join(pop2022, by="municipio_codigo")


usethis::use_data(municipios_22, overwrite = TRUE)
