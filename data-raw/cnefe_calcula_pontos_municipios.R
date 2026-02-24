library(dplyr)
library(arrow)
library(orce)
out_dir <- here::here(file.path("data-raw","cnefe", "2022"))
load(here::here("data/pontos_setores.rda"))
cnefe_setores <- open_dataset(file.path(out_dir,"arrow"))%>%
  count(uf_codigo, municipio_codigo=substr(setor,1,7),
    setor)



get1 <- function(ufnow) {
  # ufnow <- as.numeric(substr(municipio_codigo_now,1,2))
  # municipio_codigo_now <- as.character(municipio_codigo_now)
  cnefe <- cnefe_setores%>%
    filter(uf_codigo==as.numeric(ufnow))%>%collect()
  #%>%filter(municipio_codigo==municipio_codigo_now)
  pontos_municipios_setores <- pontos_setores%>%
    inner_join(cnefe, by="setor")
  ponto_densidade(pontos_municipios_setores, municipio_codigo)
}

uf_toget <- unique(ufs$uf_codigo)%>%intersect(unique(substr(pontos_setores$setor,1,2)))


pontos_municipios <- purrr::map(uf_toget, get1)%>%bind_rows()%>%
  sf::st_as_sf(crs=sf::st_crs("EPSG:4674"), coords=c("lon", "lat"), remove=FALSE)%>%
  dplyr::rename(municipio_cnefe_lon=lon, municipio_cnefe_lat=lat)


# municipios2022_map <- readr::read_rds("data-raw/municipios2022_map.rds")
# municipios2022_map%>%mutate(municipio_codigo=as.character(code_muni))%>%anti_join(pontos_municipios%>%sf::st_drop_geometry(), by=c("municipio_codigo"))
## 4300001 Lagoa Mirim e 4300002 Lagoa dos Patos


usethis::use_data(pontos_municipios, overwrite = TRUE)
