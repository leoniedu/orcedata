#' Complete a Distance Matrix via Augmented Graph Routing
#'
#' Takes the output of [get_distancias_osrm()] (with NAs for unroutable
#' pairs) and completes it by building an augmented graph with road, snap,
#' and Euclidean bridging edges, then computing shortest paths via igraph.
#'
#' @param dist_df A data.frame from [get_distancias_osrm()] with
#'   `preencher_na = FALSE`. May contain NAs for unroutable pairs. Must
#'   have `.id_orig` and `.id_dest` columns.
#' @param src An `sf` POINT object with origin points (same as passed to
#'   [get_distancias_osrm()]).
#' @param dst An `sf` POINT object with destination points. `NULL` for
#'   symmetric (src-to-src) matrices.
#' @param kmh_snap Speed (km/h) for Euclidean and snap edges. Default: `2`.
#' @param max_snap_h Maximum snap duration (hours) before reclassifying a
#'   point as unsnappable. Default: `1` (2 km at 2 km/h).
#'
#' @return A data.frame in the same format as [get_distancias_osrm()], with
#'   all NAs replaced by graph-routed durations. Includes a `metodo` column:
#'   `"osrm"` for OSRM-routed pairs, `"grafo"` for graph-completed pairs.
#'
#' @details
#' The augmented graph connects disconnected road network components via
#' Euclidean edges at `kmh_snap` speed. Points that snapped beyond
#' `max_snap_h` hours are reclassified as unsnappable and connected via
#' Euclidean edges to each other and to every road component.
#'
#' For graph-routed pairs, `distancia_km` is computed as
#' `duracao_horas * kmh_snap` — a travel-time-equivalent distance, not
#' actual road distance. The primary output for optimization is
#' `duracao_horas`.
#'
#' If snap attributes are not present on `dist_df` (e.g., from pre-computed
#' results), the graph uses original point coordinates directly and assumes
#' distances already account for snapping.
#'
#' @seealso [get_distancias_osrm()] to compute the initial distance matrix.
#'
#' @export
completar_distancias <- function(dist_df, src, dst = NULL,
                                  kmh_snap = 2,
                                  max_snap_h = 1) {
  rlang::check_installed("igraph",
                         reason = "to complete distances via graph routing")

  if (!inherits(src, "sf")) {
    cli::cli_abort("{.arg src} must be an {.cls sf} object.")
  }

  symmetric <- is.null(dst)
  if (symmetric) dst <- src

  if (!inherits(dst, "sf")) {
    cli::cli_abort("{.arg dst} must be an {.cls sf} object.")
  }

  # Check for required ID columns
  if (!all(c(".id_orig", ".id_dest") %in% names(dist_df))) {
    cli::cli_abort(
      "{.arg dist_df} must have {.code .id_orig} and {.code .id_dest} columns."
    )
  }

  # Check kmh_snap consistency
  stored_kmh <- attr(dist_df, "kmh_snap")
  if (!is.null(stored_kmh) && stored_kmh != kmh_snap) {
    cli::cli_warn(
      "kmh_snap = {kmh_snap} differs from the value used in {.fn get_distancias_osrm} ({stored_kmh})."
    )
  }

  # Read snap attributes
  snap_src <- attr(dist_df, "snap_src")
  snap_dst <- attr(dist_df, "snap_dst")
  has_snap <- !is.null(snap_src) && !is.null(snap_dst)

  if (!has_snap) {
    cli::cli_inform("No snap attributes found. Using original coordinates.")
  }

  n_src <- nrow(src)
  n_dst <- nrow(dst)

  # Detect missing pairs (from failed chunks)
  expected <- expand.grid(
    .id_orig = seq_len(n_src),
    .id_dest = seq_len(n_dst)
  )
  existing_keys <- paste(dist_df$.id_orig, dist_df$.id_dest, sep = "|")
  expected_keys <- paste(expected$.id_orig, expected$.id_dest, sep = "|")
  missing_keys <- setdiff(expected_keys, existing_keys)

  if (length(missing_keys) > 0) {
    cli::cli_inform(
      "{length(missing_keys)} missing pair{?s} detected (failed chunks)."
    )
    missing_parts <- strsplit(missing_keys, "\\|")
    missing_df <- data.frame(
      .id_orig = as.integer(
        vapply(missing_parts, `[`, 1, FUN.VALUE = character(1))
      ),
      .id_dest = as.integer(
        vapply(missing_parts, `[`, 2, FUN.VALUE = character(1))
      ),
      duracao_horas = NA_real_,
      distancia_km = NA_real_,
      snap_km_orig = NA_real_,
      snap_km_dest = NA_real_,
      metodo = NA_character_,
      stringsAsFactors = FALSE
    )
    dist_df <- dplyr::bind_rows(dist_df, missing_df)
  }

  # Build augmented graph
  cli::cli_inform(
    "Building augmented graph ({n_src} origins, {n_dst} destinations)..."
  )
  g <- construir_grafo_aumentado(
    dist_df = dist_df,
    src = src, dst = dst,
    snap_src = snap_src, snap_dst = snap_dst,
    kmh_snap = kmh_snap, max_snap_h = max_snap_h
  )

  # Compute shortest paths for NA pairs
  na_rows <- is.na(dist_df$duracao_horas)
  n_na <- sum(na_rows)

  if (n_na == 0) {
    cli::cli_inform("No NA pairs to complete.")
    return(dist_df)
  }

  cli::cli_inform("Computing shortest paths for {n_na} NA pair{?s}...")

  # Build src/dst node IDs for NA rows
  # In symmetric case, the graph uses only src_* IDs (no dst_* nodes)
  na_src_ids <- paste0("src_", dist_df$.id_orig[na_rows])
  if (symmetric) {
    na_dst_ids <- paste0("src_", dist_df$.id_dest[na_rows])
  } else {
    na_dst_ids <- paste0("dst_", dist_df$.id_dest[na_rows])
  }

  unique_from <- unique(na_src_ids)
  unique_to <- unique(na_dst_ids)

  sp_matrix <- igraph::distances(
    g, v = unique_from, to = unique_to, mode = "all"
  )

  # Map back to dist_df rows
  na_indices <- which(na_rows)
  for (k in seq_along(na_indices)) {
    i <- na_indices[k]
    s <- na_src_ids[k]
    d <- na_dst_ids[k]
    dur <- sp_matrix[s, d]

    if (is.finite(dur)) {
      dist_df$duracao_horas[i] <- round(dur, 4)
      dist_df$distancia_km[i] <- round(dur * kmh_snap, 2)
      dist_df$metodo[i] <- "grafo"
    } else {
      cli::cli_warn(
        "No path found for .id_orig={dist_df$.id_orig[i]}, .id_dest={dist_df$.id_dest[i]}."
      )
    }
  }

  # Force self-pairs to zero
  self_rows <- dist_df$.id_orig == dist_df$.id_dest
  dist_df$duracao_horas[self_rows] <- 0
  dist_df$distancia_km[self_rows] <- 0

  n_completed <- sum(dist_df$metodo == "grafo", na.rm = TRUE)
  cli::cli_inform("Completed {n_completed} pair{?s} via augmented graph.")

  dist_df
}
