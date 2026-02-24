library(dplyr)


load(here::here("data/agencias_bdo.rda"))
load(here::here("data/agencias_mun.rda"))
load(here::here("data/municipios_22.rda"))
load(here::here("data/municipios_codigos.rda"))

source(here::here("R/calcula_distancias.R"))
#load(here::here("data/distancias_agencias_municipios_osrm.rda"))
#devtools::load_all()

# calcula_distancias(origens = agencias%>%filter(agencia_codigo%in%c("292740800", "290070200")),
#                     destinos = pontos_municipios%>%filter(municipio_codigo%in%c("2927408", '2900702')))


ufs <- unique(agencias_bdo$uf_codigo)

distancias_list <- vector(mode="list", length = length(ufs))
names(distancias_list) <- ufs
for (j in ufs) {
  print(j)
  distancias_list[[as.character(j)]] <- calcula_distancias(origens = agencias_bdo%>%filter(uf_codigo==j), destinos=municipios_22%>%filter(substr(municipio_codigo,1,2)==j))
}
patch <- FALSE
if (patch) {
  load(here::here("data/distancias_agencias_municipios_osrm.rda"))
  distancias_agencias_municipios_osrm_0 <- distancias_agencias_municipios_osrm
  distancias_agencias_municipios_osrm_1 <- bind_rows(distancias_list)%>%select(agencia_codigo, municipio_codigo, distancia_km, duracao_horas)
  distancias_agencias_municipios_osrm_2 <- distancias_agencias_municipios_osrm_0%>%
    anti_join(distancias_agencias_municipios_osrm_1, by=c("agencia_codigo", "municipio_codigo"))
  distancias_agencias_municipios_osrm <- bind_rows(
    distancias_agencias_municipios_osrm_1,
    distancias_agencias_municipios_osrm_2)
}

d <- distancias_agencias_municipios_osrm%>%
  mutate(agencia_municipio_codigo=substr(agencia_codigo,1,7))%>%
  ## agencia do calculo de distancia
  left_join(agencias_mun%>%distinct(agencia_municipio_codigo, micro_codigo, aglomeracao_codigo, data), by=c("agencia_municipio_codigo"))%>%
  ## agencia de jurisdicao
  left_join(agencias_mun%>%distinct(municipio_codigo, agencia_municipio_codigo_jurisdicao=agencia_municipio_codigo), by=c("municipio_codigo"="municipio_codigo"))%>%
  left_join(municipios_codigos, by=c("municipio_codigo"), suffix = c("_distancia", "_municipio"))

du <- d%>%
  rename(
    distancia=data_distancia, municipio=data_municipio)%>%
  tidyr::unnest(c(distancia, municipio), names_sep = "_", keep_empty = TRUE)
du%>%
  group_by(municipio_codigo, agencia_municipio_codigo_distancia=substr(agencia_codigo,1,7))%>%
  summarise(
    jurisdicao_agencia=any(agencia_municipio_codigo_jurisdicao==agencia_municipio_codigo_distancia),
    jurisdicao_micro=any(micro_codigo_distancia==micro_codigo_municipio),
    jurisdicao_rm=any(municipio_rm_codigo==distancia_rm_codigo),
    jurisdicao_aglo=any(aglomeracao_codigo_municipio==aglomeracao_codigo_distancia))%>%
  mutate(across(starts_with("jurisdicao_"), ~coalesce(.x,FALSE)),
         sem_diaria=jurisdicao_agencia|jurisdicao_micro|jurisdicao_rm|jurisdicao_aglo) -> duf


distancias_agencias_municipios_diaria_0 <- distancias_agencias_municipios_osrm%>%
  mutate(agencia_municipio_codigo=substr(agencia_codigo,1,7))%>%
  left_join(duf%>%mutate(agencia_municipio_codigo=agencia_municipio_codigo_distancia, diaria_municipio=!sem_diaria), by=c("municipio_codigo", "agencia_municipio_codigo"))

agencias_municipios_diaria <- distancias_agencias_municipios_diaria_0%>%
  select(agencia_codigo,municipio_codigo, diaria_municipio)


load(here::here("data/pontos_municipios.rda"))
a_m <- municipios_22%>%arrange(municipio_codigo)
b_m <- pontos_municipios%>%arrange(municipio_codigo)
stopifnot(all(a_m$municipio_codigo==b_m$municipio_codigo))
checa_ll_municipio <- sf::st_distance(a_m,b_m, by_element = TRUE)
a_m$distancia_cnefe_km <- (checa_ll_municipio)%>%units::set_units("km")%>%as.numeric()
a_m%>%arrange(-distancia_cnefe_km)%>%head()
## nova viçosa 2923001 está ~correto

a_a <- agencias_bdo%>%mutate(municipio_codigo=substr(agencia_codigo,1,7))
b_a <- a_m%>%inner_join(a_a%>%sf::st_drop_geometry())
stopifnot(all(a_a$municipio_codigo==b_a$municipio_codigo))
checa_ll_agencia <- sf::st_distance(a_a,b_a, by_element = TRUE)
a_a$distancia_cnefe_km <- (checa_ll_agencia)%>%units::set_units("km")%>%as.numeric()
a_a%>%arrange(-distancia_cnefe_km)%>%head()


usethis::use_data(distancias_agencias_municipios_osrm, overwrite = TRUE)
usethis::use_data(agencias_municipios_diaria, overwrite = TRUE)
