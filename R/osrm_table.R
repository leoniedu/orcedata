#' Call OSRM table API directly
#'
#' Sends coordinates to the OSRM `/table/v1/driving/` endpoint and returns
#' distance/duration matrices plus snap information for each waypoint.
#'
#' @param src_coords Matrix with columns X, Y (lon, lat) for origins.
#' @param dst_coords Matrix with columns X, Y (lon, lat) for destinations.
#'   If `NULL`, computes a symmetric src-to-src table.
#' @param server OSRM server URL. Default: `getOption("osrm.server")`.
#' @return A list with:
#'   \describe{
#'     \item{distances}{Numeric matrix (n_src x n_dst) of distances in meters.
#'       `NA` for unroutable pairs.}
#'     \item{durations}{Numeric matrix (n_src x n_dst) of durations in seconds.
#'       `NA` for unroutable pairs.}
#'     \item{src_snap_m}{Numeric vector of snap distances in meters for sources.}
#'     \item{dst_snap_m}{Numeric vector of snap distances in meters for destinations.}
#'     \item{src_snapped}{Matrix (n_src x 2) of snapped [lon, lat] coordinates.}
#'     \item{dst_snapped}{Matrix (n_dst x 2) of snapped [lon, lat] coordinates.}
#'   }
#' @noRd
osrm_table <- function(src_coords, dst_coords = NULL, server = NULL) {
  server <- server %||% getOption("osrm.server")
  if (is.null(server)) {
    cli::cli_abort(
      "No OSRM server configured. Set {.code options(osrm.server = ...)} or use {.fn osrm_local_start}."
    )
  }

  symmetric <- is.null(dst_coords)

  n_src <- nrow(src_coords)

  # Build coordinate string

  if (symmetric) {
    # Symmetric: send src coords only, all are both sources and destinations
    all_coords <- src_coords
  } else {
    # Asymmetric: src coords followed by dst coords
    all_coords <- rbind(src_coords, dst_coords)
  }
  coord_str <- paste(
    sprintf("%.6f,%.6f", all_coords[, 1], all_coords[, 2]),
    collapse = ";"
  )
  n_dst <- if (symmetric) n_src else nrow(dst_coords)

  # Build URL
  base_url <- sub("/$", "", server)
  url <- paste0(base_url, "/table/v1/driving/", coord_str)

  # Source indices: 0..(n_src-1), destination indices: n_src..(n_src+n_dst-1)
  if (symmetric) {
    # No sources/destinations params needed — all points are both
    query <- list(annotations = "distance,duration")
  } else {
    src_idx <- paste(seq(0, n_src - 1), collapse = ";")
    dst_idx <- paste(seq(n_src, n_src + n_dst - 1), collapse = ";")
    query <- list(
      sources = src_idx,
      destinations = dst_idx,
      annotations = "distance,duration"
    )
  }

  req <- httr2::request(url) |>
    httr2::req_url_query(!!!query)

  # Retry on connection errors (server may still be starting)
  resp <- NULL
  for (attempt in seq_len(10L)) {
    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) {
        if (attempt < 10L) Sys.sleep(1)
        NULL
      }
    )
    if (!is.null(resp)) break
  }
  if (is.null(resp)) {
    cli::cli_abort("Could not connect to OSRM server at {.url {server}} after 10 attempts.")
  }

  body <- httr2::resp_body_json(resp)

  if (body$code != "Ok") {
    cli::cli_abort("OSRM returned error: {body$code}")
  }

  # Parse distance matrix (list of lists -> matrix)
  dist_mat <- do.call(rbind, lapply(body$distances, function(row) {
    vapply(row, function(x) if (is.null(x)) NA_real_ else as.numeric(x),
           numeric(1))
  }))

  dur_mat <- do.call(rbind, lapply(body$durations, function(row) {
    vapply(row, function(x) if (is.null(x)) NA_real_ else as.numeric(x),
           numeric(1))
  }))

  # Parse snap info from sources/destinations
  src_snap_m <- vapply(body$sources, function(w) {
    if (is.null(w$distance)) NA_real_ else as.numeric(w$distance)
  }, numeric(1))

  dst_snap_m <- vapply(body$destinations, function(w) {
    if (is.null(w$distance)) NA_real_ else as.numeric(w$distance)
  }, numeric(1))

  src_snapped <- do.call(rbind, lapply(body$sources, function(w) {
    as.numeric(w$location)
  }))

  dst_snapped <- do.call(rbind, lapply(body$destinations, function(w) {
    as.numeric(w$location)
  }))

  list(
    distances = dist_mat,
    durations = dur_mat,
    src_snap_m = src_snap_m,
    dst_snap_m = dst_snap_m,
    src_snapped = src_snapped,
    dst_snapped = dst_snapped
  )
}
