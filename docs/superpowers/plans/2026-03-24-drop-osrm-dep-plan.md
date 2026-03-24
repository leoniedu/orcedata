# Drop `osrm` Dependency: Direct OSRM HTTP API via httr2

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the `osrm` R package with direct HTTP calls to the OSRM `/table` endpoint via `httr2`, getting snap info and distance matrices in a single request per chunk.

**Architecture:** A new internal helper `osrm_table()` calls the OSRM `/table/v1/driving/` endpoint directly, sending src+dst coordinates with `sources`/`destinations` index params. The JSON response provides snap distances (`sources[].distance`, `destinations[].distance`), snapped coordinates (`sources[].location`, `destinations[].location`), and the distance/duration matrices — all in one call per chunk. This eliminates `snap_points.R` entirely.

**Tech Stack:** httr2 (includes jsonlite transitively), sf, cli

---

## File Structure

| Action | File | Responsibility |
|--------|------|----------------|
| Create | `R/osrm_table.R` | Internal helper: build URL, call OSRM `/table` API, parse response |
| Modify | `R/get_distancias_osrm.R` | Replace `snap_points()` + `osrm::osrmTable()` with `osrm_table()` |
| Delete | `R/snap_points.R` | No longer needed |
| Modify | `R/osrm_local.R:8` | Remove "Configures the `osrm` package" from docs |
| Modify | `DESCRIPTION` | Add `httr2` to Suggests; remove `osrm` |
| Create | `tests/testthat/test-osrm_table.R` | Unit tests for the new helper |
| Modify | `tests/testthat/test-get_distancias_osrm.R` | Tests still pass (integration, requires server) |
| Delete | `tests/testthat/test-snap_points.R` | No longer needed |

---

### Task 1: Create `osrm_table()` internal helper

**Files:**
- Create: `R/osrm_table.R`
- Create: `tests/testthat/test-osrm_table.R`

- [ ] **Step 1: Write the helper function**

Create `R/osrm_table.R`:

```r
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

  resp <- httr2::request(url) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_perform()

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
```

- [ ] **Step 2: Write tests for `osrm_table()`**

Create `tests/testthat/test-osrm_table.R`:

```r
test_that("osrm_table returns correct structure (symmetric)", {
  skip_if_not(
    suppressMessages(osrm_local_status()),
    "OSRM server not running"
  )

  pts <- make_mock_points()[1:3, ]
  coords <- sf::st_coordinates(pts)
  result <- osrm_table(coords)

  expect_type(result, "list")
  expect_equal(dim(result$distances), c(3, 3))
  expect_equal(dim(result$durations), c(3, 3))
  expect_length(result$src_snap_m, 3)
  expect_length(result$dst_snap_m, 3)
  expect_equal(nrow(result$src_snapped), 3)
  expect_equal(nrow(result$dst_snapped), 3)

  # Diagonal should be 0
  expect_equal(diag(result$distances), c(0, 0, 0))
  expect_equal(diag(result$durations), c(0, 0, 0))
})

test_that("osrm_table returns correct structure (asymmetric)", {
  skip_if_not(
    suppressMessages(osrm_local_status()),
    "OSRM server not running"
  )

  pts <- make_mock_points()
  src_coords <- sf::st_coordinates(pts[1:3, ])
  dst_coords <- sf::st_coordinates(pts[4:5, ])
  result <- osrm_table(src_coords, dst_coords)

  expect_equal(dim(result$distances), c(3, 2))
  expect_equal(dim(result$durations), c(3, 2))
  expect_length(result$src_snap_m, 3)
  expect_length(result$dst_snap_m, 2)
})

test_that("osrm_table errors without server", {
  withr::local_options(osrm.server = NULL)
  coords <- matrix(c(-40, -10), ncol = 2)
  expect_error(osrm_table(coords), "No OSRM server")
})
```

- [ ] **Step 3: Run tests**

Run: `Rscript -e 'devtools::test(filter = "osrm_table")'`
Expected: tests requiring server SKIP, "errors without server" PASSES

- [ ] **Step 4: Commit**

```bash
git add R/osrm_table.R tests/testthat/test-osrm_table.R
git commit -m "feat: add osrm_table() internal helper for direct OSRM HTTP API calls"
```

---

### Task 2: Rewrite `get_distancias_osrm()` to use `osrm_table()`

**Files:**
- Modify: `R/get_distancias_osrm.R`

- [ ] **Step 1: Rewrite the function**

Replace the entire body of `get_distancias_osrm()` in `R/get_distancias_osrm.R`. Key changes:
- Remove `rlang::check_installed("osrm", ...)` — replace with check for `httr2`
- Remove `snap_points()` calls — snap info comes from `osrm_table()` response
- Replace `osrm::osrmTable()` with `osrm_table()`
- Extract snap distances from the first chunk's response (since all src/dst snaps are returned per chunk, we only need the snap info once for each unique set of points)

The full rewritten function:

```r
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
    chunk_result <- tryCatch({
      r <- osrm_table(
        src_coords = src_coords[idx, , drop = FALSE],
        dst_coords = if (symmetric) NULL else dst_coords
      )

      # Collect dst snap info from first successful chunk
      # (destinations are the same across all chunks)
      if (is.null(snap_km_dst)) {
        snap_km_dst <<- r$dst_snap_m / 1000
        snapped_dst_coords <<- r$dst_snapped
      }

      # Accumulate src snap info for this chunk
      snap_km_src[idx] <<- r$src_snap_m / 1000
      snapped_src_coords[idx, ] <<- r$src_snapped

      data.frame(
        .id_dest = rep(dst_1$.id_dest, each = length(idx)),
        .id_orig = rep(src_1$.id_orig[idx], times = nrow(dst_1)),
        road_km = round(as.vector(r$distances) / 1000, 2),
        road_hours = round(as.vector(r$durations) / 3600, 2)
      )
    }, error = function(e) NULL)
    results[[i]] <- chunk_result
    if (is.null(chunk_result)) n_failed <- n_failed + 1L
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
```

**Key differences from old version:**
- `osrm::osrmTable()` durations were in minutes → `/ 60` for hours. OSRM raw API returns seconds → `/ 3600`.
- Snap info extracted from API response, no separate `snap_points()` call.
- Removed `OutDec` workaround (was needed by `osrm` package internals, not by us).

- [ ] **Step 2: Update roxygen to mention httr2 instead of osrm**

In the `@details` section, replace the reference to `osrmTable`:

```r
#' @details
#' OSRM snaps input coordinates to the nearest point on the road network.
#' The road distance returned by the OSRM table API only covers the
#' road-to-road portion. This function adds the snap distances (origin +
#' destination) to both distance and duration, using `kmh_snap` to convert
#' snap distance to travel time.
```

- [ ] **Step 3: Run existing tests**

Run: `Rscript -e 'devtools::test(filter = "get_distancias_osrm")'`
Expected: tests SKIP (require server) or PASS if server is running

- [ ] **Step 4: Commit**

```bash
git add R/get_distancias_osrm.R
git commit -m "refactor: replace osrm::osrmTable with direct HTTP API calls via httr2"
```

---

### Task 3: Delete `snap_points.R` and its tests

**Files:**
- Delete: `R/snap_points.R`
- Delete: `tests/testthat/test-snap_points.R`

- [ ] **Step 1: Verify no other code references `snap_points`**

Run: `grep -r "snap_points" R/ tests/`
Expected: only hits in `R/snap_points.R` and `tests/testthat/test-snap_points.R` (the old `get_distancias_osrm.R` references should already be gone from Task 2)

- [ ] **Step 2: Delete the files**

```bash
git rm R/snap_points.R tests/testthat/test-snap_points.R
```

- [ ] **Step 3: Run full test suite**

Run: `Rscript -e 'devtools::test()'`
Expected: all tests pass (snap_points tests gone, others unaffected)

- [ ] **Step 4: Commit**

```bash
git commit -m "chore: remove snap_points.R, replaced by osrm_table() snap extraction"
```

---

### Task 4: Update DESCRIPTION and osrm_local.R docs

**Files:**
- Modify: `DESCRIPTION:26-28`
- Modify: `R/osrm_local.R:8`

- [ ] **Step 1: Update DESCRIPTION**

In `DESCRIPTION` Suggests section:
- Remove `osrm,`
- Add `httr2,`

Result:
```
Suggests:
    httr2,
    igraph,
    osrm.backend,
    testthat (>= 3.0.0)
```

- [ ] **Step 2: Update osrm_local.R roxygen**

In `R/osrm_local.R` line 8, change:
```r
#' a local OSRM server. Configures the `osrm` package to use this server.
```
to:
```r
#' a local OSRM server. Sets `options(osrm.server = ...)` for use by
#' [get_distancias_osrm()].
```

- [ ] **Step 3: Rebuild docs and check**

Run: `Rscript -e 'devtools::document(); devtools::check(error_on = "warning")'`
Expected: no warnings about missing `osrm` dependency, docs build clean

- [ ] **Step 4: Commit**

```bash
git add DESCRIPTION R/osrm_local.R man/
git commit -m "chore: drop osrm dependency, add httr2 to Suggests"
```
