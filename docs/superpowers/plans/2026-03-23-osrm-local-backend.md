# Local OSRM Backend Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the remote OSRM server dependency with a local `osrm.backend`-managed server for computing inter-agency distance matrices.

**Architecture:** Three exported helper functions in `R/osrm_local.R` manage the local OSRM server lifecycle (start/stop/status). The existing `data-raw/distancias_agencias_osrm.R` is updated to use them. PBF files are cached via `tools::R_user_dir()`. Package state for the previous `osrm.server` option is stored in a package-level environment.

**Tech Stack:** R, osrm.backend (Suggests), osrm (Suggests), cli, rlang

**Spec:** `docs/superpowers/specs/2026-03-23-osrm-local-backend-design.md`

---

### Task 1: Add dependencies to DESCRIPTION

**Files:**
- Modify: `DESCRIPTION`

- [ ] **Step 1: Add osrm.backend and osrm to Suggests**

In `DESCRIPTION`, add to the `Suggests` section:

```
Suggests:
    osrm,
    osrm.backend,
    testthat (>= 3.0.0)
```

Also add `cli` to `Imports` (it is not currently listed but is used by the new functions):

```
Imports:
    cli,
    dplyr,
    janitor,
    rlang,
    sf,
    tidyselect
```

- [ ] **Step 2: Commit**

```bash
git add DESCRIPTION
git commit -m "feat: add osrm and osrm.backend to Suggests"
```

---

### Task 2: Create package-level environment and `osrm_local_start()`

**Files:**
- Create: `R/osrm_local.R`

- [ ] **Step 1: Write `R/osrm_local.R` with the internal environment and `osrm_local_start()`**

```r
# Internal environment to store state across start/stop calls
.osrm_local_env <- new.env(parent = emptyenv())
.osrm_local_env$prev_server <- NULL

#' Start a Local OSRM Server
#'
#' Downloads an OSM PBF file (if needed), prepares the routing graph, and starts
#' a local OSRM server. Configures the `osrm` package to use this server.
#'
#' @param region_pbf Path or URL to an `.osm.pbf` file. Accepts:
#'   - A full URL (e.g., `"https://download.geofabrik.de/south-america/brazil/nordeste-latest.osm.pbf"`)
#'   - A Geofabrik relative path (e.g., `"south-america/brazil/nordeste-latest.osm.pbf"`) —
#'     `https://download.geofabrik.de/` is prepended automatically
#'   - A local file path — used directly if the file exists
#' @param max_table_size Maximum number of table entries for the OSRM server.
#'   Default: `10000L`. Increase if computing large distance matrices.
#' @param force_download If `TRUE`, re-download the PBF file even if it exists
#'   in the cache. Default: `FALSE`.
#'
#' @return The server URL (invisibly).
#'
#' @details
#' The PBF file is cached in `tools::R_user_dir("orcedata", which = "cache")`.
#' The OSRM graph is prepared in a temporary directory and is ephemeral.
#'
#' Uses [osrm.backend::osrm_start()] which automatically installs OSRM
#' binaries and prepares the routing graph if needed.
#'
#' If a server is already running, a warning is issued and the function returns
#' early without starting a new server.
#'
#' @export
osrm_local_start <- function(region_pbf,
                             max_table_size = 10000L,
                             force_download = FALSE) {
  rlang::check_installed("osrm.backend",
                         reason = "to run a local OSRM server")

  # Check for already-running server

  servers <- osrm.backend::osrm_servers()
  if (nrow(servers) > 0) {
    cli::cli_warn("An OSRM server is already running. Use {.fn osrm_local_stop} to stop it first.")
    server_url <- paste0("http://localhost:", servers$port[1], "/")
    return(invisible(server_url))
  }

  # Save current osrm.server option
  .osrm_local_env$prev_server <- getOption("osrm.server")

  # Resolve PBF path
  pbf_path <- resolve_pbf_path(region_pbf, force_download = force_download)

  # Start server (handles install + graph prep + server start)
  cli::cli_inform("Starting OSRM server from {.file {pbf_path}}...")
  osrm.backend::osrm_start(
    path = pbf_path,
    max_table_size = max_table_size
  )

  servers <- osrm.backend::osrm_servers()
  server_url <- paste0("http://localhost:", servers$port[1], "/")
  options(osrm.server = server_url)
  cli::cli_inform("OSRM server running at {.url {server_url}}")

  invisible(server_url)
}
```

- [ ] **Step 2: Add the internal `resolve_pbf_path()` helper below `osrm_local_start()`**

```r
#' Resolve a PBF path from URL, Geofabrik path, or local file
#'
#' @param region_pbf Path or URL to a PBF file.
#' @param force_download Re-download even if cached.
#' @return Local file path to the PBF file.
#' @noRd
resolve_pbf_path <- function(region_pbf, force_download = FALSE) {
  # If it's an existing local file, use directly

  if (file.exists(region_pbf)) {
    cli::cli_inform("Using local PBF file: {.file {region_pbf}}")
    return(region_pbf)
  }

  # Build full URL if it's a Geofabrik relative path
  if (!grepl("^https?://", region_pbf)) {
    url <- paste0("https://download.geofabrik.de/", region_pbf)
  } else {
    url <- region_pbf
  }

  # Cache directory
  cache_dir <- tools::R_user_dir("orcedata", which = "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # Extract filename from URL
  filename <- basename(url)
  cached_path <- file.path(cache_dir, filename)

  if (file.exists(cached_path) && !force_download) {
    cli::cli_inform("Using cached PBF: {.file {cached_path}}")
    return(cached_path)
  }

  cli::cli_inform("Downloading {.url {url}} to {.file {cached_path}}...")
  utils::download.file(url, cached_path, mode = "wb")
  cli::cli_inform("Download complete.")

  cached_path
}
```

- [ ] **Step 3: Verify file parses**

```bash
Rscript -e 'parse(file = "R/osrm_local.R"); cat("OK\n")'
```

Expected: `OK`

- [ ] **Step 4: Commit**

```bash
git add R/osrm_local.R
git commit -m "feat: add osrm_local_start() with PBF caching and flexible input"
```

---

### Task 3: Add `osrm_local_stop()` and `osrm_local_status()`

**Files:**
- Modify: `R/osrm_local.R`

- [ ] **Step 1: Append `osrm_local_stop()` to `R/osrm_local.R`**

```r
#' Stop the Local OSRM Server
#'
#' Stops all running OSRM servers and restores the previous `osrm.server`
#' option.
#'
#' @return `NULL` (invisibly).
#'
#' @export
osrm_local_stop <- function() {
  rlang::check_installed("osrm.backend",
                         reason = "to manage OSRM servers")

  osrm.backend::osrm_stop_all()

  # Restore previous osrm.server option
  prev <- .osrm_local_env$prev_server
  if (!is.null(prev)) {
    options(osrm.server = prev)
    cli::cli_inform("Restored osrm.server to {.url {prev}}")
  } else {
    options(osrm.server = NULL)
  }
  .osrm_local_env$prev_server <- NULL

  cli::cli_inform("OSRM server stopped.")
  invisible(NULL)
}
```

- [ ] **Step 2: Append `osrm_local_status()` to `R/osrm_local.R`**

```r
#' Check Local OSRM Server Status
#'
#' Reports whether a local OSRM server is currently running.
#'
#' @return `TRUE` if a server is running, `FALSE` otherwise (invisibly).
#'
#' @export
osrm_local_status <- function() {
  rlang::check_installed("osrm.backend",
                         reason = "to check OSRM server status")

  servers <- osrm.backend::osrm_servers()
  running <- nrow(servers) > 0

  if (running) {
    cli::cli_inform("OSRM server is running on port {servers$port[1]}.")
  } else {
    cli::cli_inform("No OSRM server is running.")
  }

  invisible(running)
}
```

- [ ] **Step 3: Verify file parses**

```bash
Rscript -e 'parse(file = "R/osrm_local.R"); cat("OK\n")'
```

Expected: `OK`

- [ ] **Step 4: Commit**

```bash
git add R/osrm_local.R
git commit -m "feat: add osrm_local_stop() and osrm_local_status()"
```

---

### Task 4: Update `data-raw/distancias_agencias_osrm.R`

**Files:**
- Modify: `data-raw/distancias_agencias_osrm.R`

- [ ] **Step 1: Replace the script contents**

The new script uses `osrm_local_start()` / `osrm_local_stop()` and keeps the same distance computation logic:

```r
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
```

- [ ] **Step 2: Verify file parses**

```bash
Rscript -e 'parse(file = "data-raw/distancias_agencias_osrm.R"); cat("OK\n")'
```

Expected: `OK`

- [ ] **Step 3: Commit**

```bash
git add data-raw/distancias_agencias_osrm.R
git commit -m "feat: update distancias_agencias_osrm.R to use local OSRM backend"
```

---

### Task 5: Generate documentation and verify package

**Files:**
- Modified by roxygen: `NAMESPACE`, `man/osrm_local_start.Rd`, `man/osrm_local_stop.Rd`, `man/osrm_local_status.Rd`

- [ ] **Step 1: Run roxygen**

```bash
Rscript -e 'devtools::document()'
```

Verify that `NAMESPACE` now exports `osrm_local_start`, `osrm_local_stop`, `osrm_local_status`.

- [ ] **Step 2: Run R CMD check (no tests, no vignettes)**

```bash
Rscript -e 'devtools::check(args = c("--no-tests", "--no-vignettes"))'
```

Expected: 0 errors, 0 warnings. Notes about large data files are acceptable.

- [ ] **Step 3: Commit generated docs**

```bash
git add NAMESPACE man/
git commit -m "docs: add roxygen docs for osrm_local functions"
```
