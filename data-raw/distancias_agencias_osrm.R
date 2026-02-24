library(dplyr)
load("data/agencias_bdo.rda")

get_distancias_agencias <- function(agencias_ll) {
  options(OutDec = ".")
  require(osrm)
  res <- osrm::osrmTable(
    src = agencias_ll, # Origem
    measure = c("distance", "duration"
    )) # Medidas
  data.frame(agencia_codigo_orig=rep(agencias_ll$agencia_codigo, length=nrow(agencias_ll)^2),
             agencia_codigo_dest=rep(agencias_ll$agencia_codigo, each=nrow(agencias_ll)),
             distancia_km=round(as.vector(res$distances)/1000,2)
             ,duracao_horas=round(as.vector(res$durations)/60,2)
  )
}

ufs <- unique(agencias_bdo$uf_codigo)
agencias_dist_list <- vector(mode="list", length = length(ufs))
names(agencias_dist_list) <- ufs
library(sf)
for (ufnow in ufs) {
  print(ufnow)
  agencias_dist_list[[ufnow]] <- get_distancias_agencias(
    agencias_bdo%>%filter(substr(agencia_codigo,1,2)==ufnow))
}


distancias_agencias_osrm <- bind_rows(agencias_dist_list)
stopifnot(nrow(distancias_agencias_osrm%>%count(agencia_codigo_orig, agencia_codigo_dest)%>%filter(n>1))==0)

usethis::use_data(distancias_agencias_osrm, overwrite = TRUE)
