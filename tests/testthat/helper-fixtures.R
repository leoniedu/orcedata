# Shared fixtures for orcedata tests
# These simulate OSRM output without requiring a running server.

#' Create mock sf points on two disconnected "islands"
#' Island 1: points 1-3 (connected via road)
#' Island 2: points 4-5 (connected via road)
#' Point 6: unsnappable (no road nearby)
#' Point 7: far-snapped (snap_km > threshold)
make_mock_points <- function() {
  sf::st_as_sf(
    data.frame(
      id = paste0("pt_", 1:7),
      lon = c(-40.0, -40.1, -40.2, -42.0, -42.1, -44.0, -41.0),
      lat = c(-10.0, -10.1, -10.0, -12.0, -12.1, -14.0, -11.0)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )
}

#' Create mock snapped points
#' Points 1-5: snapped normally (small offset from original)
#' Point 6: snap failed (NA — retains original geometry)
#' Point 7: far-snapped (large offset)
make_mock_snap_points <- function(pts) {
  original_coords <- sf::st_coordinates(pts)

  # Small snap offsets for points 1-5
  snapped_coords <- original_coords
  snapped_coords[1:5, 1] <- snapped_coords[1:5, 1] + 0.001  # ~100m offset
  snapped_coords[7, 1] <- snapped_coords[7, 1] + 0.05       # ~5km offset

  snap_km <- c(0.1, 0.1, 0.1, 0.1, 0.1, NA, 5.0)

  pontos <- sf::st_as_sf(
    data.frame(id = pts$id, lon = snapped_coords[, 1], lat = snapped_coords[, 2]),
    coords = c("lon", "lat"),
    crs = 4326
  )

  list(pontos = pontos, snap_km = snap_km)
}

#' Create mock OSRM distance output (long format)
#' Simulates `get_distancias_osrm(preencher_na = FALSE)` output.
#' Island 1 (pts 1-3): all pairs routable
#' Island 2 (pts 4-5): all pairs routable
#' Cross-island and unsnappable pairs: NA
make_mock_dist_df <- function(pts) {
  n <- nrow(pts)
  pairs <- expand.grid(.id_dest = 1:n, .id_orig = 1:n)

  # Island 1: points 1-3 connected
  # Island 2: points 4-5 connected
  # Everything else: NA
  island1 <- 1:3
  island2 <- 4:5

  pairs$duracao_horas <- NA_real_
  pairs$distancia_km <- NA_real_

  for (i in seq_len(nrow(pairs))) {
    o <- pairs$.id_orig[i]
    d <- pairs$.id_dest[i]
    if (o == d) {
      pairs$duracao_horas[i] <- 0
      pairs$distancia_km[i] <- 0
    } else if (o %in% island1 && d %in% island1) {
      eucl <- as.numeric(sf::st_distance(pts[o, ], pts[d, ])) / 1000
      pairs$duracao_horas[i] <- round(eucl / 50, 4)
      pairs$distancia_km[i] <- round(eucl * 1.3, 2)
    } else if (o %in% island2 && d %in% island2) {
      eucl <- as.numeric(sf::st_distance(pts[o, ], pts[d, ])) / 1000
      pairs$duracao_horas[i] <- round(eucl / 50, 4)
      pairs$distancia_km[i] <- round(eucl * 1.3, 2)
    }
  }

  snap_info <- make_mock_snap_points(pts)

  pairs$snap_km_orig <- snap_info$snap_km[pairs$.id_orig]
  pairs$snap_km_dest <- snap_info$snap_km[pairs$.id_dest]
  pairs$metodo <- ifelse(is.na(pairs$duracao_horas), NA_character_, "osrm")

  # Attach snap attributes (same as get_distancias_osrm output)
  attr(pairs, "snap_src") <- snap_info$pontos
  attr(pairs, "snap_dst") <- snap_info$pontos
  attr(pairs, "kmh_snap") <- 2

  pairs
}
