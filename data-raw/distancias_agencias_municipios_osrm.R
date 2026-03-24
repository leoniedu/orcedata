library(dplyr)
library(sf)
devtools::load_all()

load(here::here("data/agencias_bdo.rda"))
load(here::here("data/agencias_mun.rda"))
load(here::here("data/municipios.rda"))
load(here::here("data/municipios_codigos.rda"))

osrm_local_start("brazil-260323.osm.pbf")

ufs <- unique(agencias_bdo$uf_codigo)

distancias_list <- vector(mode = "list", length = length(ufs))
names(distancias_list) <- ufs

for (j in ufs) {
  cli::cli_inform("Processing UF {j}...")
  ag_uf <- agencias_bdo |> filter(uf_codigo == j)
  mun_uf <- municipios |> filter(substr(municipio_codigo, 1, 2) == j)
  for (i in 1:nrow(mun_uf)) {
    res <- get_distancias_osrm(
      src = ag_uf[1:2, ],
      dst = mun_uf[i, ],
      completar = TRUE
    )
  }

  distancias_list[[as.character(j)]] <- res |>
    select(
      agencia_codigo = agencia_codigo_orig,
      municipio_codigo = municipio_codigo_dest,
      distancia_km,
      duracao_horas
    )
}

distancias_agencias_municipios_osrm <- bind_rows(distancias_list)

stopifnot(
  nrow(
    distancias_agencias_municipios_osrm |>
      count(agencia_codigo, municipio_codigo) |>
      filter(n > 1)
  ) ==
    0
)

d <- distancias_agencias_municipios_osrm |>
  mutate(agencia_municipio_codigo = substr(agencia_codigo, 1, 7)) |>
  ## agencia do calculo de distancia
  left_join(
    agencias_mun |>
      distinct(
        agencia_municipio_codigo,
        micro_codigo,
        aglomeracao_codigo,
        data
      ),
    by = "agencia_municipio_codigo"
  ) |>
  ## agencia de jurisdicao
  left_join(
    agencias_mun |>
      distinct(
        municipio_codigo,
        agencia_municipio_codigo_jurisdicao = agencia_municipio_codigo
      ),
    by = "municipio_codigo"
  ) |>
  left_join(
    municipios_codigos,
    by = "municipio_codigo",
    suffix = c(
      "_distancia",
      "_mun
    icipio"
    )
  )

du <- d |>
  rename(distancia = data_distancia, municipio = data_municipio) |>
  tidyr::unnest(c(distancia, municipio), names_sep = "_", keep_empty = TRUE)

du |>
  group_by(
    municipio_codigo,
    agencia_municipio_codigo_distancia = substr(agencia_codigo, 1, 7)
  ) |>
  summarise(
    jurisdicao_agencia = any(
      agencia_municipio_codigo_jurisdicao == agencia_municipio_codigo_distancia
    ),
    jurisdicao_micro = any(micro_codigo_distancia == micro_codigo_municipio),
    jurisdicao_rm = any(municipio_rm_codigo == distancia_rm_codigo),
    jurisdicao_aglo = any(
      aglomeracao_codigo_municipio == aglomeracao_codigo_distancia
    ),
    .groups = "drop"
  ) |>
  mutate(
    across(starts_with("jurisdicao_"), ~ coalesce(.x, FALSE)),
    sem_diaria = jurisdicao_agencia |
      jurisdicao_micro |
      jurisdicao_rm |
      jurisdicao_aglo
  ) -> duf

distancias_agencias_municipios_diaria_0 <- distancias_agencias_municipios_osrm |>
  mutate(agencia_municipio_codigo = substr(agencia_codigo, 1, 7)) |>
  left_join(
    duf |>
      mutate(
        agencia_municipio_codigo = agencia_municipio_codigo_distancia,
        diaria_municipio = !sem_diaria
      ),
    by = c("municipio_codigo", "agencia_municipio_codigo")
  )

agencias_municipios_diaria <- distancias_agencias_municipios_diaria_0 |>
  select(agencia_codigo, municipio_codigo, diaria_municipio)

# Ensure all agencies in agencias_bdo have rows
# Unroutable agencies default to diaria_municipio = TRUE (remote/inaccessible)
missing_ag <- setdiff(
  agencias_bdo$agencia_codigo,
  unique(agencias_municipios_diaria$agencia_codigo)
)
if (length(missing_ag) > 0) {
  all_mun <- unique(agencias_municipios_diaria$municipio_codigo)
  missing_rows <- tidyr::expand_grid(
    agencia_codigo = missing_ag,
    municipio_codigo = all_mun
  ) |>
    mutate(diaria_municipio = TRUE)
  agencias_municipios_diaria <- bind_rows(
    agencias_municipios_diaria,
    missing_rows
  )
  message(
    "Added ",
    length(missing_ag),
    " unroutable agencies to agencias_municipios_diaria: ",
    paste(missing_ag, collapse = ", ")
  )
}

load(here::here("data/pontos_municipios.rda"))
a_m <- municipios |> arrange(municipio_codigo)
b_m <- pontos_municipios |> arrange(municipio_codigo)

municipios

stopifnot(all(a_m$municipio_codigo == b_m$municipio_codigo))


checa_ll_municipio <- sf::st_distance(a_m, b_m, by_element = TRUE)
a_m$distancia_cnefe_km <- as.numeric(units::set_units(checa_ll_municipio, "km"))
a_m |> arrange(-distancia_cnefe_km) |> head()
## nova viçosa 2923001 está ~correto

a_a <- agencias_bdo |> mutate(municipio_codigo = substr(agencia_codigo, 1, 7))
b_a <- a_m |> inner_join(a_a |> sf::st_drop_geometry())
stopifnot(all(a_a$municipio_codigo == b_a$municipio_codigo))
checa_ll_agencia <- sf::st_distance(a_a, b_a, by_element = TRUE)
a_a$distancia_cnefe_km <- as.numeric(units::set_units(checa_ll_agencia, "km"))
a_a |> arrange(-distancia_cnefe_km) |> head()

usethis::use_data(distancias_agencias_municipios_osrm, overwrite = TRUE)
usethis::use_data(agencias_municipios_diaria, overwrite = TRUE)

osrm_local_stop()
