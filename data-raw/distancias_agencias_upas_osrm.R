library(dplyr)
library(sf)
source(here::here("R/rename_ibge.R"))
source(here::here("R/calcula_distancias.R"))
#source(here::here("R/add_coordinates.R"))
load(here::here("data/agencias_bdo.rda"))
load(here::here("data/pontos_setores.rda"))
load(here::here("data/municipios_22.rda"))
load("data/ufs.rda")

calcula_distancias_setores_agencias <- function(uf_codigo_now) {
  fname <- paste0("data-raw/distancias_agencias_upas_osrm_", uf_codigo_now, ".rds")
  if (file.exists(fname)) return(NULL)
  #if (uf_codigo_now==29) {
  pontos_upas <- readr::read_rds(here::here(paste0("data-raw/pontos_upas_", uf_codigo_now, ".rds")))
  #} else {
  #  pontos_upas <- readr::read_rds(here::here("data-raw/pontos_upas.rds"))
  #}

  amostra_mestra <- readRDS(here::here("data-raw/amostra_br_2024_01_2025_06.rds"))
  amostra_pof <- readxl::read_excel("~/gitlab/pof2024ba/data-raw/Alocação_trimestre_POF2425_1907.xls")%>%
    rename_ibge()
  amostra_pof_uf <- amostra_pof%>%
    filter(substr(upa,1,2)==uf_codigo_now)%>%
    distinct(setor, upa, agencia_codigo)
  amostra_uf <- amostra_mestra%>%
    filter(uf_codigo==uf_codigo_now)%>%
    mutate(ano_mes=lubridate::make_date(ano,mes_codigo), agencia_codigo=as.character(agencia_codigo))%>%
    distinct(setor=setor, upa, agencia_codigo)%>%
    bind_rows(amostra_pof_uf)


  agencias_uf <- agencias_bdo%>%
    semi_join(amostra_uf, by="agencia_codigo")


  distancias_amostra_toget_1 <- pontos_upas%>%
    semi_join(amostra_uf, by=c("upa"))

  municipios_toget <- amostra_uf%>%
    ungroup%>%
    distinct(upa)%>%
    anti_join(distancias_amostra_toget_1,by=c("upa"))%>%
    mutate(municipio_codigo=substr(upa,1,7))


  distancias_amostra_toget_2 <- municipios_22%>%
    inner_join(amostra_uf%>%
                 ungroup%>%
                 distinct(upa)%>%
                 anti_join(distancias_amostra_toget_1,by=c("upa"))%>%
                 mutate(municipio_codigo=substr(upa,1,7)))

  distancias_amostra_toget <- rbind(
    distancias_amostra_toget_1%>%
      transmute(upa,ponto_origem, upa_lat=upa_cnefe_lat, upa_lon=upa_cnefe_lon),
    distancias_amostra_toget_2%>%
      ##FIX! Distancias até municipios quando nao sabemos onde é o setor
      transmute(upa,ponto_origem="municipios_22", upa_lat=municipio_sede_lat,upa_lon=municipio_sede_lon)
  )%>%
    ungroup%>%
    distinct()

  distancias_amostra_1 <- calcula_distancias(distancias_amostra_toget, agencias_uf, nmax = 5000)

  distancias_agencias_upas_osrm <- bind_rows(distancias_amostra_1)%>%
    distinct(upa,agencia_codigo,distancia_km, duracao_horas, agencia_lat, agencia_lon, upa_lat, upa_lon)%>%
    left_join(distancias_amostra_toget%>%sf::st_drop_geometry())

  distancias_agencias_upas_osrm%>%anti_join(amostra_uf, by="upa")
  amostra_uf%>%anti_join(distancias_agencias_upas_osrm, by="upa")
  readr::write_rds(distancias_agencias_upas_osrm, fname)
}
purrr::walk(ufs$uf_codigo, calcula_distancias_setores_agencias, .progress = TRUE)

