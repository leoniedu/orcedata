## maior densidade de domicilios particulares, quando existem.
## se não, maior densidade dos endereços
library(dplyr)
library(arrow)
library(sf)
source("R/ponto_densidade.R")
load("data/pontos_setores.rda")
load("data/ufs.rda")
load("data/pontos_municipios.rda")

cnefe_calcula_pontos_upas_uf <- function(ufcodigonow) {
  fname <- paste0("data-raw/pontos_upas_", ufcodigonow,".rds")
  if (file.exists(fname)) return(NULL)
  get_ponto_upa_0 <- function(upa, setores_now) {
    ufnow <- unique(as.numeric(substr(setores_now,1,2)))
    stopifnot(length(ufnow)==1)
    setores_now <- as.character(setores_now)
    cnefe <- #open_dataset(file.path(out_dir,"arrow"))%>%
      #dplyr::filter(uf_codigo==ufnow)%>%
      cnefe_uf%>%
      sf::st_drop_geometry()%>%
      dplyr::filter(setor%in%{{setores_now}})%>%
      ## 1=Domicílio particular
      dplyr::group_by(setor, dp=especie_codigo==1,lat_cnefe,lon_cnefe)%>%
      summarise(n=sum(n))%>%
      collect()
    cnefe_upa_0 <- cnefe%>%
      dplyr::filter(dp)
    if (nrow(cnefe_upa_0)==0) {
      ## nao tem domicilio particular no setor
      cnefe_upa_0 <- cnefe%>%
        group_by(dp, lat_cnefe,lon_cnefe)%>%
        summarise(n=sum(n))
    }
    cnefe_upa <- cnefe_upa_0%>%
      sf::st_as_sf(crs=sf::st_crs("EPSG:4674"), coords=c("lon_cnefe", "lat_cnefe"))
    ponto_upa <- ponto_densidade(cnefe_upa%>%mutate(upa={{upa}}), upa)
    ponto_upa
  }
  get_ponto_upa <- memoise::memoise(get_ponto_upa_0)
  out_dir <- here::here(file.path("data-raw","cnefe", "2022"))
  amostra_mestra <- readRDS(here::here("data-raw/amostra_br_2024_01_2025_06.rds"))%>%
    mutate(trimestre=ceiling(as.numeric(mes_codigo)/3))%>%
    filter(uf_codigo==ufcodigonow)

  cnefe_uf_0 <- open_dataset(file.path(out_dir,"arrow"))%>%
    dplyr::filter(uf_codigo==ufcodigonow)%>%
    rename(lon_cnefe=longitude,lat_cnefe=latitude)%>%
    collect()%>%
    sf::st_as_sf(coords=c("lon_cnefe", "lat_cnefe"), crs=sf::st_crs("EPSG:4674"), remove=FALSE)

  cnefe_uf_a <- cnefe_uf_0%>%
    semi_join(amostra_mestra, by="setor")

  cnefe_uf_m <- amostra_mestra%>%
    anti_join(cnefe_uf_a, by="setor")%>%
    ungroup()%>%
    distinct(setor)

  if (ufcodigonow==29) {
    setores2024_map <- ibgemaps::mapa_setores_ba_27_08_24%>%
      filter(uf_codigo==ufcodigonow)%>%
      select(setor)%>%
      ## setores sem cnefe
      filter(setor%in%cnefe_uf_m$setor)
    cnefe_uf_p <- cnefe_uf_0%>%
      filter(substr(setor,1,7)%in%substr(unique(setores2024_map$setor),1,7))%>%
      st_join(setores2024_map, left=FALSE, suffix = c("_cnefe", "_poligono"))
    cnefe_uf <- cnefe_uf_a%>%
      bind_rows(cnefe_uf_p%>%rename(setor=setor_poligono)%>%select(-setor_cnefe))
  } else {
    cnefe_uf <- cnefe_uf_a
  }

  upas <- amostra_mestra%>%
    group_by(upa, ano, trimestre)%>%
    mutate(n_setores=n())%>%
    group_by(upa, ano, trimestre, n_setores)%>%
    ## a partir do 2o trimestre de 2023 é utilizado o CNEFE do Censo
    ## obs, com a atualização dos setores o cálculo deverá ser refeito
    ## mas não se espera grandes alterações
    filter(ano_mes>="2023-04-01")%>%
    summarise(setores=list(setor))




  pontos_upas_uf_0 <- purrr::pmap(upas%>%filter(n_setores>1), function(upa, setores, ano, trimestre, ...) get_ponto_upa(upa, setores_now = setores)%>%mutate(ano={{ano}}, trimestre={{trimestre}}), .progress = TRUE)%>%
    bind_rows()%>%
    sf::st_as_sf(coords=c("lon", "lat"), remove=FALSE, crs=sf::st_crs("EPSG:4674"))%>%
    dplyr::rename("upa_cnefe_lon"=lon, 'upa_cnefe_lat'=lat)

  pontos_upas_uf_1 <- pontos_setores%>%
    filter(substr(setor,1,2)==ufcodigonow)%>%
    transmute(upa=setor, upa_cnefe_lat=setor_lat, upa_cnefe_lon=setor_lon)%>%
    group_by(upa)%>%
    slice(1)%>%
    inner_join(upas%>%
                 anti_join(pontos_upas_uf_0)%>%
                 filter(n_setores==1)%>%
                 distinct(upa, ano, trimestre), by=c("upa"))

  pontos_upas_uf_2 <- bind_rows(pontos_upas_uf_0, pontos_upas_uf_1)

  pontos_upas_uf_3_0 <- amostra_mestra%>%
    anti_join(pontos_upas_uf_2, by="upa")%>%
    group_by(upa, ano, trimestre)%>%
    mutate(setores=list(setor), n_setores=n())
  if (nrow(pontos_upas_uf_3_0)>0) {
    pontos_upas_uf_3 <- pontos_upas_uf_3_0%>%
      purrr::pmap(function(upa, setores, ano, trimestre, ...) get_ponto_upa(upa, setores_now = setores)%>%mutate(ano={{ano}}, trimestre={{trimestre}}), .progress = TRUE)%>%
      bind_rows()%>%
      sf::st_as_sf(coords=c("lon", "lat"), remove=FALSE, crs=sf::st_crs("EPSG:4674"))%>%
      dplyr::rename("upa_cnefe_lon"=lon, 'upa_cnefe_lat'=lat)
    pontos_upas_uf_4_0 <- pontos_upas_uf_3_0%>%
      anti_join(pontos_upas_uf_3)
    pontos_upas_uf_4 <- pontos_municipios%>%
      inner_join(pontos_upas_uf_4_0%>%mutate(municipio_codigo=substr(setor,1,7)))%>%
      transmute(upa, upa_cnefe_lon=municipio_cnefe_lon, upa_cnefe_lat=municipio_cnefe_lat, ano, trimestre, ponto_origem="municipio")
  } else {
    pontos_upas_uf_3 <- NULL
    pontos_upas_uf_4 <- NULL
    pontos_upas_uf_2 <- pontos_upas_uf_2%>%mutate(ponto_origem="upa")
  }
  pontos_upas_uf <- bind_rows(pontos_upas_uf_0, pontos_upas_uf_1, pontos_upas_uf_2, pontos_upas_uf_3, pontos_upas_uf_4)%>%
    distinct(upa, upa_cnefe_lon, upa_cnefe_lat, ano, trimestre, geometry, ponto_origem=coalesce(ponto_origem, "upa"))
  readr::write_rds(pontos_upas_uf, fname)
}


purrr::walk(as.numeric(ufs$uf_codigo), cnefe_calcula_pontos_upas_uf)
