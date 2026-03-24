#' Compute OSRM Distance Matrix with Snap Correction
#'
#' Computes road distances and durations between points using OSRM, with
#' correction for snap distances (the gap between input coordinates and the
#' nearest road in the network).
#'
#' @param src An `sf` POINT object with origin points.
#' @param dst An `sf` POINT object with destination points. If `NULL` (default),
#'   computes a src-to-src matrix.
#' @param kmh_snap Speed in km/h assumed for the snap distance (off-road
#'   portion). Default: `2`. This converts the snap distance to additional
#'   travel time. A low value penalizes locations far from roads.
#' @param chunk_size Number of origins per OSRM request. Default: `5000L`.
#' @param preencher_na If `TRUE`, fill unroutable pairs with Euclidean
#'   distance at `kmh_snap` speed. If `FALSE` (default), preserve `NA`s.
#'
#' @return A `data.frame` with columns from `src` and `dst` (dropped geometry),
#'   plus:
#'   \describe{
#'     \item{.id_orig}{Integer index of the origin point in `src`.}
#'     \item{.id_dest}{Integer index of the destination point in `dst`.}
#'     \item{distancia_km}{Total distance: road + snap (origin + destination).}
#'     \item{duracao_horas}{Total duration: road + snap time at `kmh_snap`.}
#'     \item{snap_km_orig}{Snap distance for origin point (km).}
#'     \item{snap_km_dest}{Snap distance for destination point (km).}
#'     \item{metodo}{`"osrm"` for OSRM-routed pairs, `"euclidiano"` for
#'       Euclidean-filled pairs (when `preencher_na = TRUE`), or `NA` for
#'       unroutable pairs (when `preencher_na = FALSE`).}
#'   }
#'
#'   The returned data.frame carries three attributes used by
#'   [completar_distancias()]: `"snap_src"` and `"snap_dst"` (snapped `sf`
#'   points) and `"kmh_snap"` (the speed used).
#'
#' @details
#' OSRM snaps input coordinates to the nearest point on the road network.
#' The road distance returned by the OSRM table API only covers the
#' road-to-road portion. This function adds the snap distances (origin +
#' destination) to both distance and duration, using `kmh_snap` to convert
#' snap distance to travel time.
#'
#' @seealso [completar_distancias()] to complete NA pairs via augmented graph.
#'
#' @export
get_distancias_osrm <- function(src, dst = NULL,
                                kmh_snap = 2,
                                chunk_size = 5000L,
                                preencher_na = FALSE) {
  rlang::check_installed("httr2", reason = "to compute distances via OSRM")

  if (!inherits(src, "sf")) {
    cli::cli_abort("{.arg src} must be an {.cls sf} object.")
  }

  symmetric <- is.null(dst)
  if (symmetric) dst <- src

  if (!inherits(dst, "sf")) {
    cli::cli_abort("{.arg dst} must be an {.cls sf} object.")
  }

  src_1 <- src |>
    dplyr::ungroup() |>
    dplyr::mutate(.id_orig = seq_len(dplyr::n()))
  dst_1 <- dst |>
    dplyr::ungroup() |>
    dplyr::mutate(.id_dest = seq_len(dplyr::n()))

  src_coords <- sf::st_coordinates(src_1)
  dst_coords <- sf::st_coordinates(dst_1)

  n <- nrow(src_1)
  chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  n_failed <- 0L
  results <- vector("list", length(chunks))

  # Pre-allocate snap info vectors (filled per chunk)
  snap_km_src <- rep(NA_real_, n)
  snapped_src_coords <- matrix(NA_real_, nrow = n, ncol = 2)
  snap_km_dst <- NULL
  snapped_dst_coords <- NULL

  cli::cli_progress_bar("Computing distances", total = length(chunks))

  for (i in seq_along(chunks)) {
    idx <- chunks[[i]]
    r <- tryCatch(
      osrm_table(
        src_coords = src_coords[idx, , drop = FALSE],
        dst_coords = if (symmetric) NULL else dst_coords
      ),
      error = function(e) NULL
    )

    if (!is.null(r)) {
      # Collect dst snap info from first successful chunk
      # (destinations are the same across all chunks)
      if (is.null(snap_km_dst)) {
        snap_km_dst <- r$dst_snap_m / 1000
        snapped_dst_coords <- r$dst_snapped
      }

      # Accumulate src snap info for this chunk
      snap_km_src[idx] <- r$src_snap_m / 1000
      snapped_src_coords[idx, ] <- r$src_snapped

      results[[i]] <- data.frame(
        .id_dest = rep(dst_1$.id_dest, each = length(idx)),
        .id_orig = rep(src_1$.id_orig[idx], times = nrow(dst_1)),
        road_km = round(as.vector(r$distances) / 1000, 2),
        road_hours = round(as.vector(r$durations) / 3600, 2)
      )
    } else {
      n_failed <- n_failed + 1L
    }
    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  if (n_failed > 0L) {
    cli::cli_warn(
      "{n_failed} chunk{?s} failed and {?was/were} skipped."
    )
  }

  res <- dplyr::bind_rows(results)

  if (nrow(res) == 0L) {
    cli::cli_abort("All chunks failed. Cannot compute distances.")
  }

  # In symmetric case, dst snap = src snap
  if (symmetric) {
    snap_km_dst <- snap_km_src
    snapped_dst_coords <- snapped_src_coords
  }

  # Warn about far/failed snaps
  n_far_src <- sum(snap_km_src > 1, na.rm = TRUE)
  n_na_src <- sum(is.na(snap_km_src))
  if (n_far_src > 0L) {
    cli::cli_warn(
      "{n_far_src} origin{?s} snapped more than 1 km from the road network."
    )
  }
  if (n_na_src > 0L) {
    cli::cli_warn(
      "{n_na_src} origin{?s} could not be snapped."
    )
  }

  res$snap_km_orig <- snap_km_src[res$.id_orig]
  res$snap_km_dest <- snap_km_dst[res$.id_dest]

  res$distancia_km <- round(
    res$road_km + res$snap_km_orig + res$snap_km_dest, 2
  )
  res$duracao_horas <- round(
    res$road_hours + (res$snap_km_orig + res$snap_km_dest) / kmh_snap, 2
  )

  # Add metodo column
  res$metodo <- ifelse(is.na(res$road_km), NA_character_, "osrm")

  # Zero out self-routes (same point in src and dst)
  coords_orig <- sf::st_coordinates(src[res$.id_orig, ])
  coords_dest <- sf::st_coordinates(dst[res$.id_dest, ])
  self <- coords_orig[, 1] == coords_dest[, 1] &
    coords_orig[, 2] == coords_dest[, 2]
  res$distancia_km[self] <- 0
  res$duracao_horas[self] <- 0
  res$snap_km_orig[self] <- 0
  res$snap_km_dest[self] <- 0

  # Handle NA (unroutable) pairs
  na_rows <- is.na(res$distancia_km)
  if (any(na_rows) && preencher_na) {
    eucl_m <- sf::st_distance(
      src_1[res$.id_orig[na_rows], ],
      dst_1[res$.id_dest[na_rows], ],
      by_element = TRUE
    )
    eucl_km <- as.numeric(eucl_m) / 1000
    res$distancia_km[na_rows] <- round(eucl_km, 2)
    res$duracao_horas[na_rows] <- round(eucl_km / kmh_snap, 2)
    res$snap_km_orig[na_rows] <- 0
    res$snap_km_dest[na_rows] <- 0
    res$metodo[na_rows] <- "euclidiano"

    cli::cli_warn(
      "{sum(na_rows)} unroutable pair{?s} filled with Euclidean distance at {kmh_snap} km/h."
    )
  } else if (any(na_rows)) {
    cli::cli_inform(
      "{sum(na_rows)} unroutable pair{?s} preserved as NA. Use {.fn completar_distancias} to complete."
    )
  }

  # Join back source/destination attributes (drop geometry)
  src_attrs <- sf::st_drop_geometry(src_1) |>
    dplyr::rename_with(\(x) paste0(x, "_orig"), .cols = -".id_orig")
  dst_attrs <- sf::st_drop_geometry(dst_1) |>
    dplyr::rename_with(\(x) paste0(x, "_dest"), .cols = -".id_dest")

  res <- res |>
    dplyr::left_join(src_attrs, by = ".id_orig") |>
    dplyr::left_join(dst_attrs, by = ".id_dest") |>
    dplyr::select(-"road_km", -"road_hours")

  # Build snapped sf objects for completar_distancias()
  src_snap_df <- sf::st_drop_geometry(src)
  src_snap_df$lon <- snapped_src_coords[, 1]
  src_snap_df$lat <- snapped_src_coords[, 2]
  snap_src_sf <- sf::st_as_sf(
    src_snap_df, coords = c("lon", "lat"), crs = sf::st_crs(src)
  )

  dst_snap_df <- sf::st_drop_geometry(dst)
  dst_snap_df$lon <- snapped_dst_coords[, 1]
  dst_snap_df$lat <- snapped_dst_coords[, 2]
  snap_dst_sf <- sf::st_as_sf(
    dst_snap_df, coords = c("lon", "lat"), crs = sf::st_crs(dst)
  )

  attr(res, "snap_src") <- snap_src_sf
  attr(res, "snap_dst") <- snap_dst_sf
  attr(res, "kmh_snap") <- kmh_snap

  res
}
