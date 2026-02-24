## maior densidade de domicilios particulares, quando existem.
## se não, maior densidade dos endereços

library(furrr)
library(tictoc)
plan("future::multisession", workers=5)

library(dplyr)
library(arrow)
library(orce)
fname <- here::here("data/pontos_setores.rda")
if (file.exists(fname)) {
  load(fname)
} else {
  pontos_setores <- tibble(setor=as.character())
}
out_dir <- here::here(file.path("data-raw","cnefe", "2022"))

get_ponto_setor <- function(setor_now) {
  ufnow <- as.numeric(substr(setor_now,1,2))
  setor_now <- as.character(setor_now)
  cnefe <- open_dataset(file.path(out_dir,"arrow"))%>%
    dplyr::filter(uf_codigo==ufnow)%>%
    dplyr::filter(setor=={{setor_now}})%>%
    ## 1=Domicílio particular
    dplyr::count(setor,dp=especie_codigo==1,latitude,longitude)%>%
    collect()
  cnefe_setor_0 <- cnefe%>%
    dplyr::filter(dp)
  if (nrow(cnefe_setor_0)==0) {
    ## nao tem domicilio particular no setor
    cnefe_setor_0 <- cnefe%>%
      dplyr::count(setor,latitude,longitude)
  }
  cnefe_setor <- cnefe_setor_0%>%
    sf::st_as_sf(crs=sf::st_crs("EPSG:4674"), coords=c("longitude", "latitude"))
  ponto_setor <- ponto_densidade(cnefe_setor, setor)
  ponto_setor
}



setores <- open_dataset(file.path(out_dir,"arrow"))%>%
  ungroup%>%
  distinct(setor)%>%
  collect()%>%
  arrange(setor)%>%
  anti_join(pontos_setores%>%sf::st_drop_geometry(), by="setor")%>%
  #head(10e3)%>%
  pull(setor)
print("done")
print(nrow(pontos_setores))
print("doing")
print(length(setores))
tic()
tmp <- furrr::future_map(setores, get_ponto_setor, .progress = TRUE)
toc()
#tmp <- purrr::map(setores, get_ponto_setor, .progress = TRUE)
pontos_setores_new <- bind_rows(tmp)%>%
  sf::st_as_sf(coords=c("lon", "lat"), remove=FALSE, crs=sf::st_crs("EPSG:4674"))%>%
  dplyr::rename("setor_cnefe_lon"=lon, 'setor_cnefe_lat'=lat)

if (nrow(pontos_setores)>0) {
  pontos_setores <- pontos_setores%>%
    anti_join(pontos_setores_new%>%sf::st_drop_geometry(), by="setor")%>%
    bind_rows(pontos_setores_new)
} else {
  pontos_setores <- pontos_setores_new
}


setores2022_map <- readr::read_rds("data-raw/setores2022_map.rds")
setores2022_cent <- setores2022_map%>%mutate(setor=as.character(code_tract))%>%
  sf::st_centroid()%>%
  select(setor)%>%
  add_coordinates(lon="setor_centroide_lon",
                  lat="setor_centroide_lat")
pontos_setores <- pontos_setores%>%
  left_join(setores2022_cent%>%sf::st_drop_geometry(), by="setor")

pontos_setores <- bind_rows(pontos_setores,
                            setores2022_cent%>%
                              rename(geometry=geom)%>%
  anti_join(pontos_setores%>%sf::st_drop_geometry(), by=c("setor")))

pontos_setores <- pontos_setores%>%
  mutate(setor_lon=coalesce(setor_cnefe_lon, setor_centroide_lon),
         setor_lat=coalesce(setor_cnefe_lat, setor_centroide_lat)
         )%>%
  select(setor, setor_lon, setor_lat, everything())


usethis::use_data(pontos_setores, overwrite = TRUE)
