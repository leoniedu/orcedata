#' Snap points to the road network
#'
#' Uses OSRM to find the nearest point on the road network for each input
#' point. Returns both the snapped coordinates and the snap distances.
#'
#' @param points An `sf` POINT object.
#' @return A list with:
#'   \describe{
#'     \item{pontos}{An `sf` POINT object with snapped coordinates. Points that
#'       fail to snap retain their original geometry.}
#'     \item{snap_km}{Numeric vector of snap distances in km. `NA` for points
#'       that could not be snapped.}
#'   }
#' @noRd
snap_points <- function(points) {
  n <- nrow(points)
  snap_km <- numeric(n)
  snapped_coords <- sf::st_coordinates(points)

  cli::cli_progress_bar("Snapping points", total = n)
  for (i in seq_len(n)) {
    result <- tryCatch({
      nearest <- osrm::osrmNearest(points[i, ])
      list(km = nearest$distance / 1000, coords = sf::st_coordinates(nearest))
    }, error = function(e) NULL)

    if (is.null(result)) {
      snap_km[i] <- NA_real_
    } else {
      snap_km[i] <- result$km
      snapped_coords[i, ] <- result$coords
    }
    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  n_far <- sum(snap_km > 1, na.rm = TRUE)
  n_na <- sum(is.na(snap_km))

  if (n_far > 0) {
    cli::cli_warn(
      "{n_far} point{?s} snapped more than 1 km from the road network."
    )
  }
  if (n_na > 0) {
    cli::cli_warn(
      "{n_na} point{?s} could not be snapped (no nearby road found)."
    )
  }

  df <- sf::st_drop_geometry(points)
  df$lon <- snapped_coords[, 1]
  df$lat <- snapped_coords[, 2]
  pontos <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = sf::st_crs(points))

  list(pontos = pontos, snap_km = snap_km)
}
