## code to prepare `agencias_bdo` dataset goes here
library(readxl)
library(dplyr)
sapply(dir(here::here("R"), full.names = TRUE), source)

load("data/municipios_22.rda")

# bdo ---------------------------------------------------------------------
# agencia.csv from BDO: CSV de Agências (versão 2 - UTF8)
agencias_bdo_0 <- readr::read_csv2(here::here("data-raw/bdo_agencias/agencia20260210.csv"), col_types = "cccccc")%>%
  rename_ibge()%>%
  mutate(agencia_lat=as.numeric(lat), agencia_lon=as.numeric(lon), lat=NULL, lon=NULL,
         municipio_codigo=substr(agencia_codigo,1,7))%>%
  left_join(municipios_22%>%sf::st_drop_geometry()%>%select(municipio_codigo, municipio_sede_lat, municipio_sede_lon), by = "municipio_codigo")%>%
  select(-municipio_codigo)%>%
  transmute(agencia_codigo, agencia_nome,
            agencia_lon=coalesce(agencia_lon, municipio_sede_lon),
            agencia_lat=coalesce(agencia_lat, municipio_sede_lat)
            )




library(geobr)
municipios_agencias <- sort(unique(substr(agencias_bdo_0$agencia_codigo,1,7)))
mmaps <- geobr::read_municipality(code_muni = "all", year = 2022)
agencias_bdo_sp <- sf::st_as_sf(agencias_bdo_0%>%filter(!is.na(agencia_lat)), coords=c("agencia_lon", "agencia_lat"), remove=FALSE, crs=sf::st_crs("EPSG:4674"))
check_m <- agencias_bdo_sp%>%sf::st_join(mmaps, suffix=c(".x", ".y"))%>%
  filter(code_muni!=substr(agencia_codigo,1,7))%>%
  transmute(espacial=code_muni, bdo=substr(agencia_codigo,1,7), agencia_codigo, agencia_nome, municipio_espacial=name_muni)

agencias_bdo <- agencias_bdo_0%>%
  sf::st_as_sf(coords=c('agencia_lon', "agencia_lat"), remove=FALSE, crs=sf::st_crs("EPSG:4674"))

usethis::use_data(agencias_bdo, overwrite = TRUE)

# grid-export.csv from BDO: Relação de Municípios (opção TODAS AS UFS)
agencias_bdo_mun <- readr::read_csv(here::here("data-raw/bdo_agencias/grid-export_20260210.csv"), col_types = "c")%>%
  rename_ibge()%>%
  mutate(agencia_codigo=format(as.numeric(agencia_codigo), scientific=FALSE))%>%
  rename(uf_sigla=uf, agencia_nome_bdo=agencia_nome)%>%
  full_join(agencias_bdo%>%sf::st_drop_geometry()%>%select(-agencia_lat, -agencia_lon), by="agencia_codigo")

load("data/pontos_municipios.rda")
# A tibble: 2 × 6
# uf_sigla agencia_codigo agencia_nome_bdo municipio_codigo municipio_nome     agencia_nome
# <chr>    <chr>          <chr>            <chr>            <chr>              <chr>
#   1 MT       510792500      SORRISO          5101837          BOA ESPERANÇA DO … SORRISO
# 2 NA       290790500      NA               NA               NA                 CIPÓ
agencias_bdo_mun <- agencias_bdo_mun%>%filter(agencia_codigo!="290790500")

stopifnot(0==nrow(agencias_bdo_mun%>%anti_join(pontos_municipios)))
stopifnot(0==nrow(pontos_municipios%>%anti_join(agencias_bdo_mun, by='municipio_codigo')))

## diárias
#quando o deslocamento ocorrer dentro da mesma região metropolitana, aglomeração urbana ou microrregião, constituídas por Municípios limítrofes e regularmente instituídas, ou em áreas de controle integrado mantidas com países limítrofes, cuja jurisdição e competência dos órgãos, entidades e servidores brasileiros considera-se estendida, salvo se houver pernoite fora da sede, hipóteses em que as diárias pagas serão sempre as fixadas para os afastamentos dentro do território nacional.

## Regiões Metropolitanas https://www.ibge.gov.br/geociencias/organizacao-do-territorio/divisao-regional/18354-regioes-metropolitanas-aglomeracoes-urbanas-e-regioes-integradas-de-desenvolvimento.html
hlink <- 'https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/municipios_por_regioes_metropolitanas/Situacao_2020a2029/Composicao_RM_2024.xlsx'
fname <- file.path(here::here("data-raw/"), basename(hlink))
download.file(hlink, fname)

rm <- readxl::read_excel(fname, sheet=1)%>%
  filter(grepl(pattern="^RM ", x = LABEL_CATMETROPOL))%>%
  distinct(municipio_codigo=COD_MUN, rm_codigo=COD_CATMETROPOL)%>%
  group_by(municipio_codigo)%>%
  ## o mesmo município pode fazer parte de mais de uma RM
  tidyr::nest(data=rm_codigo)
  #tidyr::nest(data=c(rm_codigo))
aglomeracoes <- readxl::read_excel(fname, sheet=2)%>%
  distinct(municipio_codigo=COD_MUN, aglomeracao_codigo=COD_CATAU)
municipios_micro <- censobr::read_tracts(year=2022, dataset = "Preliminares")%>%
  collect()%>%
  rename_ibge()%>%
  filter(!is.na(municipio_codigo))%>%
  distinct(municipio_codigo, micro_codigo)

municipios_codigos <- municipios_micro%>%
  as_tibble()%>%
  left_join(aglomeracoes)%>%
  left_join(rm)
usethis::use_data(municipios_codigos, overwrite = TRUE)

agencias <- agencias_bdo%>%
  mutate(agencia_municipio_codigo=substr(agencia_codigo,1,7))%>%
  left_join(municipios_codigos, by=c("agencia_municipio_codigo"="municipio_codigo"))

agencias_mun <- agencias%>%
  distinct(agencia_municipio_codigo, micro_codigo, aglomeracao_codigo, data)%>%
  left_join(agencias_bdo_mun%>%distinct(agencia_municipio_codigo=substr(agencia_codigo,1,7), municipio_codigo))
#%>%left_join(municipios_codigos, by=c("municipio_codigo"), suffix = c("_agencia", "_municipio"))

# agencias_mun <- agencias_bdo_mun%>%
#    distinct(agencia_municipio_codigo=substr(agencia_codigo,1,7), municipio_codigo)



usethis::use_data(agencias_mun, overwrite = TRUE)
