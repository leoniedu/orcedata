# Targets Pipeline for orcedata

## Goal

Replace the manually-ordered `data-raw/*.R` scripts with a `{targets}` pipeline that tracks dependencies, caches results, and allows partial re-runs. A `ufs_filter` parameter enables testing with a subset of states.

## Scope

The pipeline covers 7 of the 9 data-raw scripts. Excluded:

- `cnefe_calcula_pontos_upas_uf.R` — depends on external survey master sample file
- `distancias_agencias_upas_osrm.R` — depends on UPA points and external POF allocation file

These remain as manual scripts.

Datasets not produced by data-raw scripts (`alteracoes_rcd202531`, `assistencias_ba`) are outside scope and remain manually maintained.

## Configuration

```r
# Top of _targets.R
ufs_filter <- NULL # NULL = all 27 UFs; e.g. c("29","28") for testing

# Data years — changing any of these invalidates dependent targets
ano_ufs           <- 2020  # geobr::read_state()
ano_setores       <- 2022  # geobr::read_census_tract()
ano_municipios    <- 2024  # geobr::read_municipality()
ano_sedes         <- 2010  # geobr::read_municipal_seat()
ano_cnefe         <- 2022  # cnefetools::read_cnefe()
ano_censo_tracts  <- 2022  # censobr::read_tracts() (microregion codes)
ano_populacao     <- "2024" # sidrar::get_sidra() period
ano_rm            <- 2024  # Composição RM (IBGE geoftp)

# BDO source files
bdo_agencias_csv  <- "data-raw/bdo_agencias/agencia20260210.csv"
bdo_grid_csv      <- "data-raw/bdo_agencias/grid-export_20260210.csv"

# OSRM data
osm_pbf           <- "brazil-260323.osm.pbf"
```

`ufs_filter` is passed to targets that iterate by UF: `pontos_setores`, `distancias_agencias_osrm`, `distancias_agencias_mun_osrm`. Geographic base data targets always download national data.

Year parameters are passed to the corresponding target functions. Changing any year invalidates that target and all downstream targets (standard targets behavior).

## File Structure

- `_targets.R` — pipeline definition (target list + configuration)
- `data-raw/_targets_functions.R` — one function per target, sourced by `_targets.R`

Existing `data-raw/*.R` scripts remain for reference but are not used by the pipeline.

## Saving Data

Target functions call `usethis::use_data()` (or `readr::write_rds()` for intermediates) as a side effect and also return the object for downstream targets to consume. This keeps `data/*.rda` in sync with the targets pipeline.

## Target DAG

```
ufs
municipios_map ──────────────────────────────┐
setores_map ─────────┐                   │
                         v                   v
                   pontos_setores      municipios_geo
                         │                   │
                         v                   v
                   pontos_municipios ──> municipios <── pop
                         │                   │
                         v                   v
                   agencias_bdo ────────────-┘
                   agencias_bdo_mun
                   municipios_codigos
                   agencias_mun
                         │
                         v
                   osrm_start
                    │        │
    distancias_agencias    distancias_agencias_mun
                    │        │
                   osrm_stop
```

## Targets

### Phase 1: Geographic Base Data

**`ufs`** — Brazilian states with centroids.
- Source: `geobr::read_state(year = ano_ufs)` + `sf::st_centroid()` + `orce::add_coordinates()` + `rename_ibge()`
- Output: `data/ufs.rda`
- Replaces: `geobrcache.R` lines 1-10

**`setores_map`** — Census sector boundaries.
- Source: `geobr::read_census_tract(year = ano_setores, code_tract = "all")`
- Output: `data-raw/setores_map.rds`
- Replaces: `geobrcache.R` lines 13-14
- `setores2010_map` excluded (only needed for legacy analyses; can be added later)

**`municipios_map`** — Municipality boundaries.
- Source: `geobr::read_municipality(year = ano_municipios)`
- Output: `data-raw/municipios_map.rds`
- Replaces: `geobrcache.R` lines 21-22

### Phase 2: CNEFE Processing

**`pontos_setores`** — High-density representative points per census sector.
- Merges `cnefe_2022.R` + `cnefe_calcula_pontos_setores.R` into one target.
- For each UF (filtered by `ufs_filter`):
  1. Get municipality codes from `municipios_map` for that UF
  2. Loop through municipalities: `cnefetools::read_cnefe(code_muni, year = ano_cnefe, output = "arrow")`
  3. For each municipality's CNEFE data, group by sector and compute `orce::ponto_densidade()` — this produces density points with address counts (`n`) per sector
  4. Delete CNEFE raw cache for that UF via `cnefetools` cache dir (free disk space)
- After all UFs: join with `setores_map` centroids as fallback coordinates for sectors without CNEFE data (existing behavior from `cnefe_calcula_pontos_setores.R` lines 72-89)
- The output includes an `n` column (CNEFE address count per sector) so that `pontos_municipios` can weight its aggregation without needing raw CNEFE data
- Inputs: `municipios_map`, `setores_map`, `ufs_filter`, `ano_cnefe`
- Output: `data/pontos_setores.rda`
- Replaces: `cnefe_2022.R` + `cnefe_calcula_pontos_setores.R`

**`pontos_municipios`** — High-density representative points per municipality.
- Aggregate sector points to municipality level via `orce::ponto_densidade()`, weighted by `n` from `pontos_setores`
- Fallback: geobr centroids from `municipios_map` for municipalities without CNEFE data (post-2022 municipalities)
- Inputs: `pontos_setores`, `municipios_map`
- Output: `data/pontos_municipios.rda`
- Replaces: `cnefe_calcula_pontos_municipios.R`

### Phase 3: Municipality + Agency Data

**`municipios_geo`** — Municipality centroids (intermediate, not saved).
- Source: `municipios_map` centroids + `rename_ibge()`
- In-memory only; consumed by `municipios` target

**`pop`** — Municipality population estimates (intermediate, not saved).
- Source: `sidrar::get_sidra(6579, variable = 9324, period = ano_populacao, geo = "City")`
- In-memory only; consumed by `municipios` target

**`municipios`** — Municipal seat locations with metadata and population.
- Three-tier fallback for seat coordinates:
  1. `geobr::read_municipal_seat(year = ano_sedes)`
  2. `pontos_municipios` (CNEFE density points)
  3. `municipios_geo` (geobr centroids)
- Join with `municipios_geo` attributes (left_join, not full_join) + `pop` (left_join)
- Inputs: `pontos_municipios`, `municipios_geo`, `pop`
- Output: `data/municipios.rda`
- Replaces: `geobrcache.R` lines 39-74

**`agencias_bdo`** — IBGE agency locations from BDO.
- Source: `bdo_agencias_csv` (tracked as file target)
- Coordinate fallback from `municipios` seat locations
- Adds `uf_codigo` from first 2 digits of `agencia_codigo`
- Inputs: `municipios`, `bdo_agencias_csv`
- Output: `data/agencias_bdo.rda`
- Replaces: `agencias.R` lines 1-36

**`agencias_bdo_mun`** — Agency-municipality mapping from BDO (saved dataset).
- Source: `bdo_grid_csv` (tracked as file target)
- Validated against `pontos_municipios` (stopifnot checks)
- Inputs: `agencias_bdo`, `pontos_municipios`, `bdo_grid_csv`
- Output: `data/agencias_bdo_mun.rda`
- Replaces: `agencias.R` lines 38-54

**`municipios_codigos`** — Municipality administrative codes (microregion, RM, agglomeration).
- Sources: `censobr::read_tracts(year=ano_censo_tracts)`, RM xlsx download from IBGE geoftp (`Composicao_RM_{ano_rm}.xlsx`)
- Output: `data/municipios_codigos.rda`
- Replaces: `agencias.R` lines 56-83

**`agencias_mun`** — Agency-municipality jurisdiction mapping.
- Links agencies to municipalities with microregion/RM/agglomeration codes
- Inputs: `agencias_bdo`, `agencias_bdo_mun`, `municipios_codigos`
- Output: `data/agencias_mun.rda`
- Replaces: `agencias.R` lines 85-99

### Phase 4: OSRM Distance Calculations

**`osrm_start`** — Start local OSRM server.
- Calls `osrm_local_start(osm_pbf)`
- Depends on: `agencias_bdo`, `agencias_mun`, `municipios`, `pontos_municipios` (ensures all inputs ready before starting server)
- Uses `cue = tar_cue(mode = "always")` so the server starts on every `tar_make()` run
- Returns a sentinel value (e.g. timestamp) to establish dependency

**`distancias_agencias_osrm`** — Inter-agency road distances.
- Loops through UFs (filtered by `ufs_filter`): `get_distancias_osrm(src = ag_uf, completar = TRUE)`
- Wraps computation in `on.exit(osrm_local_stop())` for cleanup on failure
- Inputs: `agencias_bdo`, `osrm_start`, `ufs_filter`
- Output: `data/distancias_agencias_osrm.rda`
- Replaces: `distancias_agencias_osrm.R`

**`distancias_agencias_mun_osrm`** — Agency-municipality road distances + diária flags.
- Loops through UFs (filtered by `ufs_filter`): `get_distancias_osrm(src = ag_uf, dst = mun_uf, completar = TRUE)`
- Computes jurisdictional relationships and `diaria_municipio` flag
- Wraps computation in `on.exit(osrm_local_stop())` for cleanup on failure
- Inputs: `agencias_bdo`, `agencias_mun`, `municipios`, `municipios_codigos`, `pontos_municipios`, `osrm_start`, `ufs_filter`
- Outputs:
  - `data/distancias_agencias_municipios_osrm.rda` (distances only)
  - `data/agencias_municipios_diaria.rda` (diária lookup)
  - `data/distancias_agencias_municipios_diaria.rda` (distances + diária combined)
- Replaces: `distancias_agencias_municipios_osrm.R`

**`osrm_stop`** — Stop local OSRM server.
- Calls `osrm_local_stop()`
- Depends on: `distancias_agencias_osrm`, `distancias_agencias_mun_osrm`
- Uses `cue = tar_cue(mode = "always")`

## Error Handling

- `stopifnot` validations from current scripts are preserved in target functions
- If a target fails, `tar_make()` stops and the failed target can be inspected with `tar_meta()`
- Distance target functions include `on.exit(osrm_local_stop())` to ensure the OSRM server is stopped even on failure. The `osrm_stop` target is a belt-and-suspenders safeguard.

## Package Dependencies

Add to DESCRIPTION Suggests:
- `targets` — pipeline orchestration
- `cnefetools` — CNEFE data download/read
- `sidrar` — SIDRA population tables
- `geobr` — geographic base data
- `censobr` — census microregion codes
- `arrow` — data format for CNEFE processing
- `readxl` — Excel file reading (RM composition)
- `readr` — CSV/RDS reading
- `furrr` — parallel processing (pontos_setores)

Already in DESCRIPTION: `httr2`, `igraph`, `osrm.backend`, `testthat`, `cli`, `dplyr`, `janitor`, `rlang`, `sf`, `tidyselect`
