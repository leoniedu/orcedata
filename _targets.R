# =============================================================================
# orcedata {targets} pipeline
#
# Usage:
#   targets::tar_make()          # run full pipeline
#   targets::tar_visnetwork()    # visualize DAG
#   targets::tar_outdated()      # check what needs re-running
# =============================================================================

library(targets)

# --- Configuration -----------------------------------------------------------
ufs_filter <- NULL # NULL = all 27 UFs; e.g. c("29","28") for testing

# Data years — changing any of these invalidates dependent targets
ano_ufs <- 2020 # geobr::read_state()
ano_setores <- 2022 # geobr::read_census_tract()
ano_municipios <- 2024 # geobr::read_municipality()
ano_sedes <- 2010 # geobr::read_municipal_seat()
ano_cnefe <- 2022 # cnefetools::read_cnefe()
ano_censo_tracts <- 2022 # censobr::read_tracts() (microregion codes)
ano_populacao <- "2024" # sidrar::get_sidra() period
ano_rm <- 2024 # Composição RM (IBGE geoftp)

# BDO source files
bdo_agencias_csv <- "data-raw/bdo_agencias/agencia20260210.csv"
bdo_grid_csv <- "data-raw/bdo_agencias/grid-export_20260210.csv"
agencias_excluir <- c("290790500") # inactive agencies (e.g. CIPÓ)

# Cache management
limpar_cache <- TRUE # TRUE = delete downloaded caches after processing each UF

# OSRM data
osm_pbf <- "brazil-260323.osm.pbf"

# --- Load packages and functions ---------------------------------------------
tar_option_set(
  packages = c("dplyr", "sf"),
  error = "stop"
)

source("data-raw/_targets_functions.R")

# --- Target list --------------------------------------------------------------
list(
  # Phase 1: Geographic Base Data
  tar_target(ufs, make_ufs(ano_ufs)),
  tar_target(municipios_map, make_municipios_map(ano_municipios)),

  # Phase 2: CNEFE Processing
  tar_target(
    pontos_setores,
    make_pontos_setores(
      municipios_map,
      ufs_filter,
      ano_cnefe,
      ano_setores,
      limpar_cache
    )
  ),
  tar_target(
    pontos_municipios,
    make_pontos_municipios(pontos_setores, municipios_map)
  ),

  # Phase 3: Municipality + Agency Data
  tar_target(municipios_geo, make_municipios_geo(municipios_map)),
  tar_target(pop, make_pop(ano_populacao)),
  tar_target(
    municipios,
    make_municipios(pontos_municipios, municipios_geo, pop, ano_sedes)
  ),
  tar_target(bdo_agencias_file, bdo_agencias_csv, format = "file"),
  tar_target(bdo_grid_file, bdo_grid_csv, format = "file"),
  tar_target(
    agencias_bdo,
    make_agencias_bdo(municipios, bdo_agencias_file, agencias_excluir)
  ),
  tar_target(
    agencias_bdo_mun,
    make_agencias_bdo_mun(agencias_bdo, pontos_municipios, bdo_grid_file)
  ),
  tar_target(
    municipios_codigos,
    make_municipios_codigos(ano_censo_tracts, ano_rm)
  ),
  tar_target(
    agencias_mun,
    make_agencias_mun(agencias_bdo, agencias_bdo_mun, municipios_codigos)
  ),

  # Phase 4: OSRM Distance Calculations
  tar_target(
    osrm_start,
    make_osrm_start(osm_pbf, agencias_bdo, agencias_mun, municipios),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    distancias_osrm,
    make_distancias_osrm(
      agencias_bdo,
      agencias_mun,
      municipios,
      municipios_codigos,
      osrm_start,
      ufs_filter
    )
  ),
  tar_target(
    osrm_stop,
    make_osrm_stop(distancias_osrm),
    cue = tar_cue(mode = "always")
  ),

  # Pipeline metadata — records parameters and run info
  tar_target(
    pipeline_metadata,
    make_pipeline_metadata(
      osrm_stop = osrm_stop,
      ufs_filter = ufs_filter,
      ano_ufs = ano_ufs,
      ano_setores = ano_setores,
      ano_municipios = ano_municipios,
      ano_sedes = ano_sedes,
      ano_cnefe = ano_cnefe,
      ano_censo_tracts = ano_censo_tracts,
      ano_populacao = ano_populacao,
      ano_rm = ano_rm,
      bdo_agencias_csv = bdo_agencias_csv,
      bdo_grid_csv = bdo_grid_csv,
      osm_pbf = osm_pbf
    )
  ),

  # Document and install the package
  tar_target(
    install,
    make_install(pipeline_metadata),
    cue = tar_cue(mode = "always")
  )
)
