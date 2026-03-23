library(dplyr)
library(sf)
devtools::load_all()
load("data/agencias_bdo.rda")

osrm_local_start("south-america/brazil/nordeste-latest.osm.pbf")

get_distancias_agencias <- function(agencias_ll) {
  res <- osrm::osrmTable(
    src = agencias_ll,
    measure = c("distance", "duration")
  )
  data.frame(
    agencia_codigo_orig = rep(agencias_ll$agencia_codigo,
                              length = nrow(agencias_ll)^2),
    agencia_codigo_dest = rep(agencias_ll$agencia_codigo,
                              each = nrow(agencias_ll)),
    distancia_km = round(as.vector(res$distances) / 1000, 2),
    duracao_horas = round(as.vector(res$durations) / 60, 2)
  )
}

ufs <- unique(agencias_bdo$uf_codigo)
agencias_dist_list <- vector(mode = "list", length = length(ufs))
names(agencias_dist_list) <- ufs

for (ufnow in ufs) {
  cli::cli_inform("Processing UF {ufnow}...")
  agencias_dist_list[[ufnow]] <- get_distancias_agencias(
    agencias_bdo |> filter(substr(agencia_codigo, 1, 2) == ufnow)
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
