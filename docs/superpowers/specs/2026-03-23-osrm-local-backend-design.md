# Local OSRM Backend for Distance Matrices

## Problem

The existing `data-raw/distancias_agencias_osrm.R` relies on a remote/default OSRM server. This is slow, unreliable, and not reproducible. We want to use `osrm.backend` to manage a local OSRM server with Brazil Nordeste routing data.

## Design

### Component 1: `R/osrm_local.R` — Exported helper functions

Three exported functions for managing a local OSRM server:

#### `osrm_local_start(region_pbf, max_table_size = 10000L, force_download = FALSE)`

The `region_pbf` argument accepts flexible input:
- **Full URL**: `"https://download.geofabrik.de/south-america/brazil/nordeste-latest.osm.pbf"` — used as-is
- **Geofabrik path**: `"south-america/brazil/nordeste-latest.osm.pbf"` — prepends `https://download.geofabrik.de/`
- **Local file path**: `"/path/to/nordeste-latest.osm.pbf"` — if the file exists on disk, skip download entirely

Steps:

1. **Check for running server**: If a local OSRM server is already running, warn and return early (invisible server info).
2. **Save current `osrm.server` option**: Store the current value so `osrm_local_stop()` can restore it.
3. **Resolve PBF path**: If `region_pbf` is a local file that exists, use it directly. Otherwise, treat as URL (prepending Geofabrik base if needed). Check `tools::R_user_dir("orcedata", which = "cache")` for cached file (keyed by filename). Download via `utils::download.file()` if missing or if `force_download = TRUE`.
4. **Start server via `osrm.backend::osrm_start()`**: This high-level function handles OSRM binary installation, graph extraction, preparation, and server start in one call. Pass the PBF path directly. Pass `max_table_size` to allow large distance matrices.
5. **Configure `osrm` package**: Set `options(osrm.server = "http://localhost:5001/")` (5001 is the `osrm.backend` default port).
6. **No auto-cleanup**: Server stays running until explicitly stopped.

Uses `rlang::check_installed("osrm.backend")` to give a clear error if the package isn't installed.

Returns the server URL invisibly.

#### `osrm_local_stop()`

Stops all running OSRM servers via `osrm.backend::osrm_stop_all()`. Restores the previous `osrm.server` option that was saved by `osrm_local_start()`.

#### `osrm_local_status()`

Checks if a local server is running via `osrm.backend::osrm_servers()`. Returns `TRUE`/`FALSE` with an informative cli message.

### Component 2: Revised `data-raw/distancias_agencias_osrm.R`

Replaces the current script. Structure:

1. Start local server: `osrm_local_start("https://download.geofabrik.de/south-america/brazil/nordeste-latest.osm.pbf")`
2. Load data: `load("data/agencias_bdo.rda")`
3. Compute distances by UF using `osrm::osrmTable()` (same loop logic as current script, hits local server)
4. Validate: `stopifnot` duplicate check (same as current)
5. Save: `usethis::use_data(distancias_agencias_osrm, overwrite = TRUE)`
6. Stop server: `osrm_local_stop()`

### Component 3: Dependencies

- **`osrm.backend`** added to `Suggests` (only needed when running a local server; runtime check via `rlang::check_installed()`)
- **`osrm`** added to `Suggests` (only needed when computing distances)
- PBF caching uses `tools::R_user_dir()` (base R since 4.0) — no extra dependency needed

## Decisions

- PBF cached in `tools::R_user_dir("orcedata", which = "cache")` (base R, no extra dep) — survives across sessions
- `region_pbf` accepts full URL, Geofabrik relative path, or local file path
- Graph preparation happens in tempdir via `osrm_start()` — ephemeral, reprocessed each run
- Use `osrm.backend::osrm_start()` (high-level) instead of manual extract/contract pipeline — it handles install, extract, prepare, and server start
- Default port is 5001 (osrm.backend default)
- `max_table_size` defaults to 10000 to support large UF distance matrices
- Server is not auto-cleaned; user calls `osrm_local_stop()` explicitly
- `osrm_local_stop()` restores the previous `osrm.server` option
- `osrm_local_start()` warns and returns early if a server is already running
- All three functions are exported with roxygen docs
- This is the first migration; `distancias_agencias_municipios_osrm.R` and `distancias_agencias_upas_osrm.R` will follow the same pattern later

## Future work

- Migrate `distancias_agencias_municipios_osrm.R` to use `osrm_local_start()`
- Migrate `distancias_agencias_upas_osrm.R` to use `osrm_local_start()`
