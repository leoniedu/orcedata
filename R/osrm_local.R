# Internal environment to store state across start/stop calls
.osrm_local_env <- new.env(parent = emptyenv())
.osrm_local_env$prev_server <- NULL

#' Start a Local OSRM Server
#'
#' Downloads an OSM PBF file (if needed), prepares the routing graph, and starts
#' a local OSRM server. Sets `options(osrm.server = ...)` for use by
#' [get_distancias_osrm()].
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
    server_url <- paste0("http://localhost:", servers$port[1], "/")
    options(osrm.server = server_url)
    cli::cli_warn("An OSRM server is already running at {.url {server_url}}. Use {.fn osrm_local_stop} to stop it first.")
    return(invisible(server_url))
  }

  # Save current osrm.server option
  .osrm_local_env$prev_server <- getOption("osrm.server")

  # Resolve PBF path
  pbf_path <- resolve_pbf_path(region_pbf, force_download = force_download)

  # Estimate resource usage from PBF file size
  warn_resource_usage(pbf_path)

  # Start server (handles install + graph prep + server start)
  cli::cli_inform("Starting OSRM server from {.file {pbf_path}}...")
  osrm.backend::osrm_start(
    path = pbf_path,
    max_table_size = max_table_size,
    overwrite = TRUE
  )

  servers <- osrm.backend::osrm_servers()
  server_url <- paste0("http://localhost:", servers$port[1], "/")
  options(osrm.server = server_url)
  cli::cli_inform("OSRM server running at {.url {server_url}}")

  invisible(server_url)
}

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
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = 3600)
  utils::download.file(url, cached_path, mode = "wb")
  cli::cli_inform("Download complete.")

  cached_path
}

#' Warn about estimated resource usage based on PBF file size
#'
#' @param pbf_path Path to the PBF file.
#' @noRd
warn_resource_usage <- function(pbf_path) {
  pbf_mb <- file.size(pbf_path) / 1024^2
  if (is.na(pbf_mb)) return(invisible(NULL))

  # Graph files are ~3x PBF size; server memory is ~2x PBF size
  disk_gb <- round(pbf_mb * 3 / 1024, 1)
  mem_gb <- round(pbf_mb * 2 / 1024, 1)

  cli::cli_inform(c(
    "i" = "PBF file size: {round(pbf_mb)} MB",
    "i" = "Estimated disk for graph preparation: ~{disk_gb} GB",
    "i" = "Estimated server memory usage: ~{mem_gb} GB"
  ))
}

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

#' Clear Cached PBF Files
#'
#' Removes downloaded `.osm.pbf` files from the orcedata cache directory.
#'
#' @return A character vector of deleted file paths (invisibly).
#'
#' @export
osrm_local_clear_cache <- function() {
  cache_dir <- tools::R_user_dir("orcedata", which = "cache")

  if (!dir.exists(cache_dir)) {
    cli::cli_inform("Cache directory does not exist. Nothing to clear.")
    return(invisible(character(0)))
  }

  files <- list.files(cache_dir, full.names = TRUE)
  if (length(files) == 0) {
    cli::cli_inform("Cache is empty.")
    return(invisible(character(0)))
  }

  sizes_mb <- round(file.size(files) / 1024^2)
  total_mb <- sum(sizes_mb, na.rm = TRUE)

  cli::cli_inform(c(
    "Removing {length(files)} cached file{?s} ({total_mb} MB):",
    set_names(basename(files), rep("*", length(files)))
  ))

  unlink(files)
  invisible(files)
}
