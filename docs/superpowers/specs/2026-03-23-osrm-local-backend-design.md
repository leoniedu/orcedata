# Local OSRM Backend for Distance Matrices

## Problem

The existing `data-raw/distancias_agencias_osrm.R` relies on a remote/default OSRM server. This is slow, unreliable, and not reproducible. We want to use `osrm.backend` to manage a local OSRM server with Brazil Nordeste routing data.

## Design

### Component 1: `R/osrm_local.R` — Exported helper functions

Three exported functions for managing a local OSRM server:

#### `osrm_local_start(region_pbf_url, profile = "car")`

1. **Download PBF**: Checks `rappdirs::user_cache_dir("orcedata")` for the file (keyed by filename extracted from URL). Downloads via `utils::download.file()` if missing.
2. **Install OSRM binaries**: Calls `osrm.backend::osrm_install()` if `osrm.backend::osrm_which()` returns nothing. Uses `rlang::check_installed("osrm.backend")` to give a clear error if the package isn't installed.
3. **Prepare graph in tempdir**: Copies the cached PBF to a tempdir, runs `osrm.backend::osrm_extract()` then `osrm.backend::osrm_contract()` with the given profile.
4. **Start server**: Calls `osrm.backend::osrm_start()`, configures the `osrm` package to point at `http://localhost:5000` via `options(osrm.server = "http://localhost:5000/")`.
5. **No auto-cleanup**: Server stays running until explicitly stopped.

#### `osrm_local_stop()`

Stops all running OSRM servers via `osrm.backend::osrm_stop_all()`.

#### `osrm_local_status()`

Checks if a local server is running (via `osrm.backend::osrm_servers()` or HTTP ping to localhost:5000). Returns `TRUE`/`FALSE` with an informative message.

### Component 2: Revised `data-raw/distancias_agencias_osrm.R`

Replaces the current script. Structure:

1. Start local server: `osrm_local_start("https://download.geofabrik.de/south-america/brazil/nordeste-latest.osm.pbf")`
2. Load data: `load("data/agencias_bdo.rda")`
3. Compute distances by UF using `osrm::osrmTable()` (same loop logic as current script, hits local server)
4. Validate: `stopifnot` duplicate check (same as current)
5. Save: `usethis::use_data(distancias_agencias_osrm, overwrite = TRUE)`
6. Stop server: `osrm_local_stop()`

### Component 3: Dependencies

- **`rappdirs`** added to `Imports` in DESCRIPTION (needed for PBF caching)
- **`osrm.backend`** added to `Suggests` (only needed when running a local server; runtime check via `rlang::check_installed()`)
- **`osrm`** added to `Suggests` (only needed when computing distances)

## Decisions

- PBF cached in `rappdirs::user_cache_dir("orcedata")` — survives across sessions
- Processed graph data (extract/contract output) goes in tempdir — ephemeral, reprocessed each run
- Server is not auto-cleaned; user calls `osrm_local_stop()` explicitly
- All three functions are exported with roxygen docs
- This is the first migration; `distancias_agencias_municipios_osrm.R` and `distancias_agencias_upas_osrm.R` will follow the same pattern later

## Future work

- Migrate `distancias_agencias_municipios_osrm.R` to use `osrm_local_start()`
- Migrate `distancias_agencias_upas_osrm.R` to use `osrm_local_start()`
