library(dplyr)
library(sf)
devtools::load_all()
load("data/agencias_bdo.rda")

osrm_local_start("south-america/brazil/nordeste-latest.osm.pbf")

ufs <- unique(substr(agencias_bdo$agencia_codigo, 1, 2))
agencias_dist_list <- vector(mode = "list", length = length(ufs))
names(agencias_dist_list) <- ufs

for (ufnow in ufs) {
  cli::cli_inform("Processing UF {ufnow}...")
  ag_uf <- agencias_bdo |> filter(substr(agencia_codigo, 1, 2) == ufnow)
  res <- get_distancias_osrm(src = ag_uf)
  agencias_dist_list[[ufnow]] <- res |>
    select(
      agencia_codigo_orig, agencia_codigo_dest,
      distancia_km, duracao_horas,
      snap_km_orig, snap_km_dest
    )
}

distancias_agencias_osrm <- bind_rows(agencias_dist_list)
stopifnot(
  nrow(
    distancias_agencias_osrm |>
      count(agencia_codigo_orig, agencia_codigo_dest) |>
      filter(n > 1)
  ) == 0
)

usethis::use_data(distancias_agencias_osrm, overwrite = TRUE)

osrm_local_stop()
