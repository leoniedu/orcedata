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
#' @param chunk_size Number of origins per OSRM request. Default: `100L`.
#' @param preencher_na If `TRUE`, fill unroutable pairs with Euclidean
#'   distance at `kmh_snap` speed. If `FALSE` (default), preserve `NA`s.
#'
#' @return A `data.frame` with columns from `src` and `dst` (dropped geometry),
#'   plus:
#'   \describe{
#'     \item{distancia_km}{Total distance: road + snap (origin + destination)}
#'     \item{duracao_horas}{Total duration: road + snap time at `kmh_snap`}
#'     \item{snap_km_orig}{Snap distance for origin point (km)}
#'     \item{snap_km_dest}{Snap distance for destination point (km)}
#'   }
#'
#' @details
#' OSRM snaps input coordinates to the nearest point on the road network.
#' The road distance returned by `osrmTable` only covers the road-to-road
#' portion. This function adds the snap distances (origin + destination) to
#' both distance and duration, using `kmh_snap` to convert snap distance to
#' travel time.
#'
#' For points far from roads (e.g., across a river, on an island), the snap
#' distance can be significant. The low default speed (2 km/h) reflects
#' uncertainty about off-road travel conditions.
#'
#' When OSRM cannot route between a pair of points (returns `NA`), the
#' Euclidean (great-circle) distance is used as a fallback, with duration
#' computed at `kmh_snap` speed. A warning reports how many pairs were filled.
#'
#' @export
get_distancias_osrm <- function(src, dst = NULL,
                                kmh_snap = 2,
                                chunk_size = 100L,
                                preencher_na = FALSE) {
  rlang::check_installed("osrm", reason = "to compute distances via OSRM")

  if (!inherits(src, "sf")) {
    cli::cli_abort("{.arg src} must be an {.cls sf} object.")
  }

  symmetric <- is.null(dst)
  if (symmetric) dst <- src

  if (!inherits(dst, "sf")) {
    cli::cli_abort("{.arg dst} must be an {.cls sf} object.")
  }

  old_outdec <- getOption("OutDec")
  on.exit(options(OutDec = old_outdec), add = TRUE)
  options(OutDec = ".")

  # Step 1: Snap all unique points
  cli::cli_inform("Snapping {nrow(src)} origins and {nrow(dst)} destinations...")
  snap_src_result <- snap_points(src)
  snap_dst_result <- if (symmetric) snap_src_result else snap_points(dst)

  # Step 2: Compute osrmTable in chunks
  src_1 <- src |>
    dplyr::ungroup() |>
    dplyr::mutate(.id_orig = seq_len(dplyr::n()))
  dst_1 <- dst |>
    dplyr::ungroup() |>
    dplyr::mutate(.id_dest = seq_len(dplyr::n()))

  n <- nrow(src_1)
  chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  n_failed <- 0L
  results <- vector("list", length(chunks))

  cli::cli_progress_bar("Computing distances", total = length(chunks))

  for (i in seq_along(chunks)) {
    idx <- chunks[[i]]
    results[[i]] <- tryCatch({
      r <- osrm::osrmTable(
        src = src_1[idx, ],
        dst = dst_1,
        measure = c("distance", "duration")
      )
      data.frame(
        .id_dest = rep(dst_1$.id_dest, each = length(idx)),
        .id_orig = rep(src_1$.id_orig[idx], times = nrow(dst_1)),
        road_km = round(as.vector(r$distances) / 1000, 2),
        road_hours = round(as.vector(r$durations) / 60, 2)
      )
    }, error = function(e) NULL)
    if (is.null(results[[i]])) n_failed <- n_failed + 1L
    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  if (n_failed > 0L) {
    cli::cli_warn(
      "{n_failed} chunk{?s} failed and {?was/were} skipped."
    )
  }

  res <- dplyr::bind_rows(results)

  # Step 3: Join snap distances and compute totals
  res$snap_km_orig <- snap_src_result$snap_km[res$.id_orig]
  res$snap_km_dest <- snap_dst_result$snap_km[res$.id_dest]

  res$distancia_km <- round(
    res$road_km + res$snap_km_orig + res$snap_km_dest, 2
  )
  res$duracao_horas <- round(
    res$road_hours + (res$snap_km_orig + res$snap_km_dest) / kmh_snap, 2
  )

  # Add metodo column (must be before road_km is dropped in select)
  res$metodo <- ifelse(is.na(res$road_km), NA_character_, "osrm")

  # Step 4: Handle NA (unroutable) pairs
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

  # Attach snap info for completar_distancias()
  attr(res, "snap_src") <- snap_src_result$pontos
  attr(res, "snap_dst") <- snap_dst_result$pontos
  attr(res, "kmh_snap") <- kmh_snap

  res
}
