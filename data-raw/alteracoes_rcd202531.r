library(dplyr)
library(sf)
load("data/agencias_bdo_mun.rda")
load("data/agencias_bdo.rda")
load("data/municipios_codigos.rda")

ba2025031 <- readr::read_csv("data-raw/bdo_agencias/ba_rcd0031_20250905.csv")%>%
  janitor::clean_names()%>%
  left_join(agencias_bdo, by="agencia_nome")%>%
  mutate(agencia_codigo,municipio_codigo)%>%
  mutate(across(ends_with("codigo"), as.character))

agencias_bdo_mun_ba <- agencias_bdo_mun%>%
  filter(uf_sigla=="BA")

agencia_micro <- municipios_codigos%>%
  distinct(agencia_codigo=paste0(municipio_codigo,"00"),
           micro_codigo)

alteracoes <- ba2025031%>%
  filter(municipio_codigo!=2927408)%>%
  full_join(
  agencias_bdo_mun_ba%>%
    filter(municipio_codigo!=2927408)
  , by=c("municipio_codigo"), suffix = c("_nova", "_original"))%>%
  filter(municipio_codigo!=2927408)%>%
  filter(agencia_codigo_original!=agencia_codigo_nova)%>%
  left_join(agencia_micro%>%distinct(agencia_codigo_original=agencia_codigo, micro_codigo_agencia_original=micro_codigo),by = "agencia_codigo_original")%>%
  left_join(agencia_micro%>%distinct(agencia_codigo_nova=agencia_codigo, micro_codigo_agencia_nova=micro_codigo),by = "agencia_codigo_nova")%>%
  mutate(alteracao_diarias=if_else(
    micro_codigo_agencia_nova!=micro_codigo_agencia_original,
    "Meia-diária na agência antiga",
    "Sem alteração"))%>%
  distinct(municipio_nome=municipio_nome_original, agencia_nome_original, agencia_nome_nova, alteracao_diarias, municipio_codigo, agencia_codigo_original, agencia_codigo_nova)


alteracoes_rcd202531 <- alteracoes
pof2024ba::excel_pof(alteracoes_rcd202531)

usethis::use_data(alteracoes_rcd202531, overwrite = TRUE)
