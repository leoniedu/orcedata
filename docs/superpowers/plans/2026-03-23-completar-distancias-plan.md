# `completar_distancias()` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `completar_distancias()` to orcedata — a function that completes an OSRM distance matrix by building an augmented graph (road + snap + Euclidean bridging edges) to route through disconnected road network components.

**Architecture:** Post-processing function that takes `get_distancias_osrm()` output (with NAs) + original sf points, constructs an igraph with road/snap/Euclidean edges, computes shortest paths, and returns a complete long data.frame. Requires refactoring `snap_points()` to return snapped coordinates (not just distances) and adding a `preencher_na` parameter to `get_distancias_osrm()`.

**Tech Stack:** R, igraph (Suggests), sf, cli, dplyr, rlang

**Spec:** `docs/superpowers/specs/2026-03-23-completar-distancias-design.md`

---

## Important Implementation Notes

### Column naming convention

`get_distancias_osrm()` currently drops its internal `.id_orig` / `.id_dest` columns before returning. For `completar_distancias()` to reliably map rows back to source/destination indices, these columns **must be preserved** in the output. Task 3 modifies the `dplyr::select()` at line 148 to keep them.

### Undirected graph with averaged OSRM durations

The graph is undirected. OSRM durations are slightly asymmetric (one-way streets). For each pair (A,B), the road edge weight is the **average** of `duration(A→B)` and `duration(B→A)` when both are available. The graph construction handles this by averaging before adding edges, then using `igraph::simplify(edge.attr.comb = list(weight = "mean"))`.

### `distancia_km` for graph-routed pairs

For graph-routed pairs, `distancia_km = duracao_horas * kmh_snap`. This is a travel-time-equivalent distance, NOT road distance. Graph paths may combine road edges (at ~50 km/h) with Euclidean edges (at `kmh_snap`), so this value is approximate. The primary output for the optimizer is `duracao_horas`.

---

## File Structure

| File | Responsibility | Action |
|------|---------------|--------|
| `R/snap_points.R` | Extract `snap_points()` from `get_distancias_osrm.R`, return list with sf POINT + distances | Create |
| `R/get_distancias_osrm.R` | Add `preencher_na` param, keep `.id_orig`/`.id_dest`, attach snap attributes, add `metodo` column | Modify |
| `R/completar_distancias.R` | Main `completar_distancias()` function | Create |
| `R/construir_grafo.R` | Internal graph construction: `construir_grafo_aumentado()` and edge-building helpers | Create |
| `DESCRIPTION` | Add igraph to Suggests | Modify |
| `tests/testthat/test-snap_points.R` | Tests for refactored snap_points | Create |
| `tests/testthat/test-get_distancias_osrm.R` | Tests for new params/attributes | Create |
| `tests/testthat/test-construir_grafo.R` | Tests for graph construction | Create |
| `tests/testthat/test-completar_distancias.R` | Integration tests for full pipeline | Create |
| `tests/testthat/helper-fixtures.R` | Shared test fixtures (mock sf points, mock OSRM output) | Create |

---

## Task 1: Test Fixtures and testthat Setup

Create shared test fixtures that all subsequent tasks will use. These mock the OSRM pipeline without requiring a running server.

**Files:**
- Create: `tests/testthat.R`
- Create: `tests/testthat/helper-fixtures.R`

- [ ] **Step 1: Create testthat runner**

```r
# tests/testthat.R
library(testthat)
library(orcedata)

test_check("orcedata")
```

- [ ] **Step 2: Create test fixtures**

Create `tests/testthat/helper-fixtures.R` with:

```r
# Shared fixtures for orcedata tests
# These simulate OSRM output without requiring a running server.

#' Create mock sf points on two disconnected "islands"
#' Island 1: points 1-3 (connected via road)
#' Island 2: points 4-5 (connected via road)
#' Point 6: unsnappable (no road nearby)
#' Point 7: far-snapped (snap_km > threshold)
make_mock_points <- function() {
  coords <- matrix(c(
    -40.0, -10.0,   # 1: island 1
    -40.1, -10.1,   # 2: island 1
    -40.2, -10.0,   # 3: island 1
    -42.0, -12.0,   # 4: island 2
    -42.1, -12.1,   # 5: island 2
    -44.0, -14.0,   # 6: unsnappable
    -41.0, -11.0    # 7: far-snapped
  ), ncol = 2, byrow = TRUE)

  sf::st_as_sf(
    data.frame(id = paste0("pt_", 1:7)),
    coords = coords,
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
    data.frame(id = pts$id),
    coords = snapped_coords,
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
```

- [ ] **Step 3: Verify fixtures load without error**

Run: `Rscript -e 'devtools::load_all(); testthat::test_dir("tests/testthat")'`
Expected: 0 tests (no test files yet), no errors from helper loading.

- [ ] **Step 4: Commit**

```bash
git add tests/testthat.R tests/testthat/helper-fixtures.R
git commit -m "test: add shared fixtures for completar_distancias tests"
```

---

## Task 2: Extract and Refactor `snap_points()`

Extract `snap_points()` from `get_distancias_osrm.R` into its own file, changing the return type from a numeric vector to a list with `$pontos` (sf POINT) and `$snap_km` (numeric).

**Files:**
- Create: `R/snap_points.R`
- Modify: `R/get_distancias_osrm.R:62-63,110-111,153-189`
- Create: `tests/testthat/test-snap_points.R`

- [ ] **Step 1: Write tests for new `snap_points()` return structure**

Create `tests/testthat/test-snap_points.R`:

```r
test_that("snap_points returns list with pontos and snap_km", {
  # This test requires a running OSRM server, so skip in CI
  skip_if_not(
    suppressMessages(osrm_local_status()),
    "OSRM server not running"
  )

  pts <- make_mock_points()[1:3, ]
  result <- snap_points(pts)

  expect_type(result, "list")
  expect_named(result, c("pontos", "snap_km"))
  expect_s3_class(result$pontos, "sf")
  expect_equal(nrow(result$pontos), 3)
  expect_type(result$snap_km, "double")
  expect_length(result$snap_km, 3)
})

test_that("snap_points preserves original geometry for failed snaps", {
  skip_if_not(
    suppressMessages(osrm_local_status()),
    "OSRM server not running"
  )

  ocean_pt <- sf::st_as_sf(
    data.frame(id = "ocean"),
    coords = c(-30, -20),
    crs = 4326
  )
  result <- snap_points(ocean_pt)

  expect_true(is.na(result$snap_km[1]))
  expect_s3_class(result$pontos, "sf")
  expect_equal(nrow(result$pontos), 1)
})
```

- [ ] **Step 2: Create `R/snap_points.R` with refactored function**

```r
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

  pontos <- sf::st_as_sf(
    sf::st_drop_geometry(points),
    coords = snapped_coords,
    crs = sf::st_crs(points)
  )

  list(pontos = pontos, snap_km = snap_km)
}
```

- [ ] **Step 3: Update `get_distancias_osrm.R` to use new `snap_points()` return**

In `R/get_distancias_osrm.R`, replace lines 62-63:

```r
# OLD:
snap_src <- snap_points(src)
snap_dst <- if (symmetric) snap_src else snap_points(dst)
```

With:

```r
# NEW:
snap_src_result <- snap_points(src)
snap_dst_result <- if (symmetric) snap_src_result else snap_points(dst)
```

Replace lines 110-111:

```r
# OLD:
res$snap_km_orig <- snap_src[res$.id_orig]
res$snap_km_dest <- snap_dst[res$.id_dest]
```

With:

```r
# NEW:
res$snap_km_orig <- snap_src_result$snap_km[res$.id_orig]
res$snap_km_dest <- snap_dst_result$snap_km[res$.id_dest]
```

Remove the old `snap_points()` definition from `get_distancias_osrm.R` (lines 153-189).

- [ ] **Step 4: Run `devtools::document()` and verify package loads**

Run: `Rscript -e 'devtools::document(); devtools::load_all()'`
Expected: No errors. NAMESPACE unchanged (snap_points is internal).

- [ ] **Step 5: Commit**

```bash
git add R/snap_points.R R/get_distancias_osrm.R man/
git commit -m "refactor: extract snap_points to own file, return sf + distances"
```

---

## Task 3: Add `preencher_na`, `.id_orig`/`.id_dest`, Snap Attributes, and `metodo` to `get_distancias_osrm()`

Modify `get_distancias_osrm()` to:
- Add `preencher_na` parameter (default `FALSE`)
- **Keep `.id_orig` and `.id_dest` in the output** (needed by `completar_distancias()`)
- Add `metodo` column **before** `road_km` is dropped
- Attach snap attributes to the result

**Files:**
- Modify: `R/get_distancias_osrm.R`
- Create: `tests/testthat/test-get_distancias_osrm.R`

- [ ] **Step 1: Write tests**

Create `tests/testthat/test-get_distancias_osrm.R`:

```r
test_that("preencher_na = FALSE preserves NAs and attaches snap attributes", {
  skip_if_not(
    suppressMessages(osrm_local_status()),
    "OSRM server not running"
  )

  pts <- make_mock_points()[1:5, ]
  result <- get_distancias_osrm(pts, preencher_na = FALSE)

  expect_true("metodo" %in% names(result))
  expect_true(".id_orig" %in% names(result))
  expect_true(".id_dest" %in% names(result))

  # Snap attributes should be present
  expect_s3_class(attr(result, "snap_src"), "sf")
  expect_s3_class(attr(result, "snap_dst"), "sf")
  expect_equal(attr(result, "kmh_snap"), 2)
})

test_that("preencher_na = TRUE fills NAs with Euclidean (legacy behavior)", {
  skip_if_not(
    suppressMessages(osrm_local_status()),
    "OSRM server not running"
  )

  pts <- make_mock_points()[1:5, ]
  result <- get_distancias_osrm(pts, preencher_na = TRUE)

  expect_false(any(is.na(result$duracao_horas)))
  expect_false(any(is.na(result$distancia_km)))
  expect_true("metodo" %in% names(result))
})

test_that("metodo column is always present", {
  skip_if_not(
    suppressMessages(osrm_local_status()),
    "OSRM server not running"
  )

  pts <- make_mock_points()[1:3, ]
  result <- get_distancias_osrm(pts, preencher_na = FALSE)

  expect_true(all(result$metodo == "osrm", na.rm = TRUE))
})
```

- [ ] **Step 2: Add `preencher_na` parameter to function signature**

In `R/get_distancias_osrm.R`, update the function signature and roxygen:

```r
#' @param preencher_na If `TRUE`, fill unroutable pairs with Euclidean
#'   distance at `kmh_snap` speed. If `FALSE` (default), preserve `NA`s.
```

```r
get_distancias_osrm <- function(src, dst = NULL,
                                kmh_snap = 2,
                                chunk_size = 100L,
                                preencher_na = FALSE) {
```

- [ ] **Step 3: Add `metodo` column after snap distance join, before the select**

After line 111 (`res$snap_km_dest <- ...`), add:

```r
  # Add metodo column (must be before road_km is dropped in select)
  res$metodo <- ifelse(is.na(res$road_km), NA_character_, "osrm")
```

- [ ] **Step 4: Wrap Euclidean fallback in `preencher_na` conditional**

Replace the existing Step 4 block (the `na_rows` / Euclidean fallback section) with:

```r
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
```

- [ ] **Step 5: Keep `.id_orig`/`.id_dest` and drop only `road_km`/`road_hours`**

Replace the select at line 148:

```r
# OLD:
    dplyr::select(-".id_orig", -".id_dest", -"road_km", -"road_hours")
```

With:

```r
# NEW: keep .id_orig and .id_dest for completar_distancias()
    dplyr::select(-"road_km", -"road_hours")
```

- [ ] **Step 6: Attach snap attributes before return**

Before the final `res` return statement, add:

```r
  # Attach snap info for completar_distancias()
  attr(res, "snap_src") <- snap_src_result$pontos
  attr(res, "snap_dst") <- snap_dst_result$pontos
  attr(res, "kmh_snap") <- kmh_snap
```

- [ ] **Step 7: Run `devtools::document()` and `devtools::check()`**

Run: `Rscript -e 'devtools::document(); devtools::check()'`
Expected: No errors, no new warnings.

- [ ] **Step 8: Commit**

```bash
git add R/get_distancias_osrm.R man/ NAMESPACE
git commit -m "feat: add preencher_na param, snap attributes, and metodo column"
```

---

## Task 4: Graph Construction — `construir_grafo_aumentado()`

Build the internal function that constructs the augmented igraph from OSRM output + snap info + original points.

**Files:**
- Create: `R/construir_grafo.R`
- Create: `tests/testthat/test-construir_grafo.R`
- Modify: `DESCRIPTION` (add igraph to Suggests)

- [ ] **Step 1: Add igraph to DESCRIPTION Suggests**

In `DESCRIPTION`, update the Suggests field:

```
Suggests:
    igraph,
    osrm,
    osrm.backend,
    testthat (>= 3.0.0)
```

- [ ] **Step 2: Write tests for graph construction**

Create `tests/testthat/test-construir_grafo.R`:

```r
test_that("graph has correct nodes for all point types", {
  skip_if_not_installed("igraph")

  pts <- make_mock_points()
  dist_df <- make_mock_dist_df(pts)

  g <- construir_grafo_aumentado(
    dist_df = dist_df,
    src = pts,
    dst = pts,
    snap_src = attr(dist_df, "snap_src"),
    snap_dst = attr(dist_df, "snap_dst"),
    kmh_snap = 2,
    max_snap_h = 1
  )

  expect_s3_class(g, "igraph")

  node_names <- igraph::V(g)$name
  expect_true("src_1" %in% node_names)
  expect_true("snap_src_1" %in% node_names)

  # Point 6 is unsnappable — no snap node
  expect_false("snap_src_6" %in% node_names)

  # Point 7 is far-snapped (5km / 2kmh = 2.5h > max_snap_h=1) — no snap node
  expect_false("snap_src_7" %in% node_names)
})

test_that("graph connects all components", {
  skip_if_not_installed("igraph")

  pts <- make_mock_points()
  dist_df <- make_mock_dist_df(pts)

  g <- construir_grafo_aumentado(
    dist_df = dist_df,
    src = pts,
    dst = pts,
    snap_src = attr(dist_df, "snap_src"),
    snap_dst = attr(dist_df, "snap_dst"),
    kmh_snap = 2,
    max_snap_h = 1
  )

  components <- igraph::components(g)
  expect_equal(components$no, 1)
})

test_that("graph works without snap info", {
  skip_if_not_installed("igraph")

  pts <- make_mock_points()
  dist_df <- make_mock_dist_df(pts)

  g <- construir_grafo_aumentado(
    dist_df = dist_df,
    src = pts,
    dst = pts,
    snap_src = NULL,
    snap_dst = NULL,
    kmh_snap = 2,
    max_snap_h = 1
  )

  expect_s3_class(g, "igraph")
  node_names <- igraph::V(g)$name
  expect_false(any(grepl("^snap_", node_names)))
  components <- igraph::components(g)
  expect_equal(components$no, 1)
})

test_that("road edges use averaged OSRM durations", {
  skip_if_not_installed("igraph")

  pts <- make_mock_points()[1:3, ]  # single island, symmetric
  dist_df <- make_mock_dist_df(pts)

  g <- construir_grafo_aumentado(
    dist_df = dist_df,
    src = pts,
    dst = pts,
    snap_src = attr(dist_df, "snap_src"),
    snap_dst = attr(dist_df, "snap_dst"),
    kmh_snap = 2,
    max_snap_h = 1
  )

  # For symmetric input, A->B and B->A durations should be averaged
  dur_ab <- dist_df$duracao_horas[
    dist_df$.id_orig == 1 & dist_df$.id_dest == 2
  ]
  dur_ba <- dist_df$duracao_horas[
    dist_df$.id_orig == 2 & dist_df$.id_dest == 1
  ]
  expected_avg <- mean(c(dur_ab, dur_ba))

  edge_id <- igraph::get.edge.ids(g, c("snap_src_1", "snap_src_2"))
  if (edge_id > 0) {
    edge_weight <- igraph::E(g)$weight[edge_id]
    expect_equal(edge_weight, expected_avg, tolerance = 0.01)
  }
})
```

- [ ] **Step 3: Implement `construir_grafo_aumentado()`**

Create `R/construir_grafo.R`:

```r
#' Build augmented graph for distance completion
#'
#' Constructs an undirected igraph with road, snap, unsnappable, and component
#' bridge edges. All edge weights are in hours.
#'
#' @param dist_df Long data.frame from `get_distancias_osrm()`.
#'   Must have `.id_orig` and `.id_dest` columns.
#' @param src Original sf POINT origins.
#' @param dst Original sf POINT destinations.
#' @param snap_src Snapped sf POINT origins (or NULL).
#' @param snap_dst Snapped sf POINT destinations (or NULL).
#' @param kmh_snap Speed for Euclidean/snap edges.
#' @param max_snap_h Max snap duration before reclassifying as unsnappable.
#' @return An undirected igraph object.
#' @noRd
construir_grafo_aumentado <- function(dist_df, src, dst,
                                       snap_src, snap_dst,
                                       kmh_snap, max_snap_h) {
  rlang::check_installed("igraph", reason = "to build the augmented graph")

  has_snap <- !is.null(snap_src) && !is.null(snap_dst)

  n_src <- nrow(src)
  n_dst <- nrow(dst)
  src_ids <- paste0("src_", seq_len(n_src))
  dst_ids <- paste0("dst_", seq_len(n_dst))

  # Snap classification
  if (has_snap) {
    snap_km_src <- as.numeric(
      sf::st_distance(src, snap_src, by_element = TRUE)
    ) / 1000
    snap_km_dst <- as.numeric(
      sf::st_distance(dst, snap_dst, by_element = TRUE)
    ) / 1000
    snap_h_src <- snap_km_src / kmh_snap
    snap_h_dst <- snap_km_dst / kmh_snap

    ok_src <- !is.na(snap_km_src) & snap_h_src <= max_snap_h
    ok_dst <- !is.na(snap_km_dst) & snap_h_dst <= max_snap_h

    snap_src_ids <- paste0("snap_src_", seq_len(n_src))
    snap_dst_ids <- paste0("snap_dst_", seq_len(n_dst))

    unsn_src <- which(!ok_src)
    unsn_dst <- which(!ok_dst)
  } else {
    ok_src <- rep(FALSE, n_src)
    ok_dst <- rep(FALSE, n_dst)
    unsn_src <- integer(0)
    unsn_dst <- integer(0)
  }

  # --- Build edge list ---
  edges <- list()

  # 1. Road edges (non-NA, non-self pairs) — averaged for undirected graph
  road_rows <- !is.na(dist_df$duracao_horas) & dist_df$duracao_horas > 0
  if (any(road_rows)) {
    road_df <- dist_df[road_rows, ]

    if (has_snap) {
      road_df$from <- ifelse(
        ok_src[road_df$.id_orig],
        snap_src_ids[road_df$.id_orig],
        src_ids[road_df$.id_orig]
      )
      road_df$to <- ifelse(
        ok_dst[road_df$.id_dest],
        snap_dst_ids[road_df$.id_dest],
        dst_ids[road_df$.id_dest]
      )
    } else {
      road_df$from <- src_ids[road_df$.id_orig]
      road_df$to <- dst_ids[road_df$.id_dest]
    }

    # Create canonical edge keys (sorted pair) and average durations
    road_df$edge_key <- ifelse(
      road_df$from < road_df$to,
      paste(road_df$from, road_df$to, sep = "|"),
      paste(road_df$to, road_df$from, sep = "|")
    )
    averaged <- stats::aggregate(
      duracao_horas ~ edge_key,
      data = road_df,
      FUN = mean
    )
    parts <- strsplit(averaged$edge_key, "\\|")
    edges$road <- data.frame(
      from = vapply(parts, `[`, 1, FUN.VALUE = character(1)),
      to = vapply(parts, `[`, 2, FUN.VALUE = character(1)),
      weight = averaged$duracao_horas,
      stringsAsFactors = FALSE
    )
  }

  # 2. Snap edges (original -> snap point)
  if (has_snap) {
    snap_edge_list <- list()
    for (i in which(ok_src)) {
      snap_edge_list[[length(snap_edge_list) + 1]] <- data.frame(
        from = src_ids[i], to = snap_src_ids[i],
        weight = snap_h_src[i], stringsAsFactors = FALSE
      )
    }
    for (j in which(ok_dst)) {
      snap_edge_list[[length(snap_edge_list) + 1]] <- data.frame(
        from = dst_ids[j], to = snap_dst_ids[j],
        weight = snap_h_dst[j], stringsAsFactors = FALSE
      )
    }
    if (length(snap_edge_list) > 0) {
      edges$snap <- dplyr::bind_rows(snap_edge_list)
    }
  }

  # 3. Unsnappable point edges (pairwise among unsnappable)
  unsn_ids <- c(src_ids[unsn_src], dst_ids[unsn_dst])
  unsn_pts <- rbind(src[unsn_src, ], dst[unsn_dst, ])
  if (length(unsn_ids) > 1) {
    unsn_edges <- list()
    for (i in seq_len(length(unsn_ids) - 1)) {
      for (j in (i + 1):length(unsn_ids)) {
        eucl_km <- as.numeric(
          sf::st_distance(unsn_pts[i, ], unsn_pts[j, ])
        ) / 1000
        unsn_edges[[length(unsn_edges) + 1]] <- data.frame(
          from = unsn_ids[i], to = unsn_ids[j],
          weight = eucl_km / kmh_snap, stringsAsFactors = FALSE
        )
      }
    }
    edges$unsnappable <- dplyr::bind_rows(unsn_edges)
  }

  # Combine all edges so far
  all_edges <- dplyr::bind_rows(edges)

  # Collect all node IDs
  all_node_ids <- unique(c(
    all_edges$from, all_edges$to,
    src_ids, dst_ids
  ))
  if (has_snap) {
    all_node_ids <- unique(c(
      all_node_ids,
      snap_src_ids[ok_src], snap_dst_ids[ok_dst]
    ))
  }

  # Build initial undirected graph
  g <- igraph::graph_from_data_frame(all_edges, directed = FALSE)
  missing_nodes <- setdiff(all_node_ids, igraph::V(g)$name)
  if (length(missing_nodes) > 0) {
    g <- igraph::add_vertices(g, length(missing_nodes), name = missing_nodes)
  }

  # 4-5. Component bridges: each vertex -> nearest in every other component
  g <- adicionar_pontes_componentes(
    g, src, dst, snap_src, snap_dst, ok_src, ok_dst, kmh_snap
  )

  # Deduplicate edges: keep minimum weight
  if (igraph::any_multiple(g)) {
    g <- igraph::simplify(g, edge.attr.comb = list(weight = "min"))
  }

  g
}

#' Add component bridge edges to ensure full connectivity
#'
#' For each vertex, adds an Euclidean edge to its nearest vertex in every
#' other connected component. Uses Euclidean distance between original
#' point coordinates.
#'
#' @noRd
adicionar_pontes_componentes <- function(g, src, dst, snap_src, snap_dst,
                                          ok_src, ok_dst, kmh_snap) {
  comps <- igraph::components(g)
  if (comps$no <= 1) return(g)

  cli::cli_inform("Bridging {comps$no} disconnected components...")

  node_geom <- construir_geometria_nos(
    src, dst, snap_src, snap_dst, ok_src, ok_dst
  )

  node_names <- igraph::V(g)$name
  membership <- comps$membership
  names(membership) <- node_names

  bridge_edges <- list()

  for (comp_id in seq_len(comps$no)) {
    nodes_in <- node_names[membership == comp_id]
    nodes_out <- node_names[membership != comp_id]
    if (length(nodes_out) == 0) next

    # Filter to nodes that have geometry in our lookup
    nodes_in <- intersect(nodes_in, names(node_geom))
    nodes_out <- intersect(nodes_out, names(node_geom))
    if (length(nodes_in) == 0 || length(nodes_out) == 0) next

    geom_in <- do.call(c, node_geom[nodes_in])
    geom_out <- do.call(c, node_geom[nodes_out])

    sf_in <- sf::st_sf(
      id = nodes_in,
      geometry = sf::st_sfc(geom_in, crs = 4326)
    )
    sf_out <- sf::st_sf(
      id = nodes_out,
      geometry = sf::st_sfc(geom_out, crs = 4326)
    )

    dists <- sf::st_distance(sf_in, sf_out)

    for (i in seq_len(nrow(sf_in))) {
      nearest_j <- which.min(dists[i, ])
      eucl_km <- as.numeric(dists[i, nearest_j]) / 1000
      bridge_edges[[length(bridge_edges) + 1]] <- data.frame(
        from = nodes_in[i],
        to = nodes_out[nearest_j],
        weight = eucl_km / kmh_snap,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(bridge_edges) > 0) {
    be <- dplyr::bind_rows(bridge_edges)
    g <- igraph::add_edges(
      g,
      as.vector(t(be[, c("from", "to")])),
      weight = be$weight
    )
  }

  g
}

#' Build named list of node geometries for spatial lookups
#' @noRd
construir_geometria_nos <- function(src, dst, snap_src, snap_dst,
                                     ok_src, ok_dst) {
  geom <- list()
  n_src <- nrow(src)
  n_dst <- nrow(dst)

  for (i in seq_len(n_src)) {
    geom[[paste0("src_", i)]] <- sf::st_geometry(src[i, ])[[1]]
  }
  for (j in seq_len(n_dst)) {
    geom[[paste0("dst_", j)]] <- sf::st_geometry(dst[j, ])[[1]]
  }
  if (!is.null(snap_src)) {
    for (i in which(ok_src)) {
      geom[[paste0("snap_src_", i)]] <- sf::st_geometry(snap_src[i, ])[[1]]
    }
  }
  if (!is.null(snap_dst)) {
    for (j in which(ok_dst)) {
      geom[[paste0("snap_dst_", j)]] <- sf::st_geometry(snap_dst[j, ])[[1]]
    }
  }
  geom
}
```

- [ ] **Step 4: Run tests**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-construir_grafo.R")'`
Expected: All tests pass.

- [ ] **Step 5: Commit**

```bash
git add R/construir_grafo.R tests/testthat/test-construir_grafo.R DESCRIPTION
git commit -m "feat: add construir_grafo_aumentado for augmented graph construction"
```

---

## Task 5: Implement `completar_distancias()`

The main function that ties everything together: reads OSRM output, builds the augmented graph, runs shortest paths, and returns the completed data.frame.

**Files:**
- Create: `R/completar_distancias.R`
- Create: `tests/testthat/test-completar_distancias.R`

- [ ] **Step 1: Write tests**

Create `tests/testthat/test-completar_distancias.R`:

```r
test_that("completar_distancias fills all NAs", {
  skip_if_not_installed("igraph")

  pts <- make_mock_points()
  dist_df <- make_mock_dist_df(pts)

  result <- completar_distancias(dist_df, pts)

  expect_false(any(is.na(result$duracao_horas)))
  expect_false(any(is.na(result$distancia_km)))
})

test_that("completar_distancias preserves OSRM values for routable pairs", {
  skip_if_not_installed("igraph")

  pts <- make_mock_points()
  dist_df <- make_mock_dist_df(pts)

  result <- completar_distancias(dist_df, pts)

  osrm_original <- dist_df[!is.na(dist_df$duracao_horas), ]
  osrm_result <- result[result$metodo == "osrm", ]

  # Merge on IDs and compare durations
  merged <- merge(
    osrm_original[, c(".id_orig", ".id_dest", "duracao_horas")],
    osrm_result[, c(".id_orig", ".id_dest", "duracao_horas")],
    by = c(".id_orig", ".id_dest"),
    suffixes = c("_orig", "_result")
  )
  expect_equal(merged$duracao_horas_orig, merged$duracao_horas_result)
})

test_that("completar_distancias adds metodo column", {
  skip_if_not_installed("igraph")

  pts <- make_mock_points()
  dist_df <- make_mock_dist_df(pts)

  result <- completar_distancias(dist_df, pts)

  expect_true("metodo" %in% names(result))
  expect_true(all(result$metodo %in% c("osrm", "grafo")))
})

test_that("self-pairs are zero", {
  skip_if_not_installed("igraph")

  pts <- make_mock_points()
  dist_df <- make_mock_dist_df(pts)

  result <- completar_distancias(dist_df, pts)

  self_pairs <- result[result$.id_orig == result$.id_dest, ]
  expect_true(all(self_pairs$duracao_horas == 0))
  expect_true(all(self_pairs$distancia_km == 0))
})

test_that("completar_distancias works without snap attributes", {
  skip_if_not_installed("igraph")

  pts <- make_mock_points()
  dist_df <- make_mock_dist_df(pts)

  attr(dist_df, "snap_src") <- NULL
  attr(dist_df, "snap_dst") <- NULL
  attr(dist_df, "kmh_snap") <- NULL

  result <- completar_distancias(dist_df, pts)

  expect_false(any(is.na(result$duracao_horas)))
})

test_that("completar_distancias warns on kmh_snap mismatch", {
  skip_if_not_installed("igraph")

  pts <- make_mock_points()
  dist_df <- make_mock_dist_df(pts)
  attr(dist_df, "kmh_snap") <- 3

  expect_warning(
    completar_distancias(dist_df, pts, kmh_snap = 2),
    "kmh_snap"
  )
})

test_that("completar_distancias detects missing pairs from failed chunks", {
  skip_if_not_installed("igraph")

  pts <- make_mock_points()
  dist_df <- make_mock_dist_df(pts)

  # Remove some rows to simulate failed chunks
  dist_df <- dist_df[
    !(dist_df$.id_orig == 1 & dist_df$.id_dest == 2),
  ]

  result <- completar_distancias(dist_df, pts)

  # Should still have the missing pair, completed via graph
  pair <- result[result$.id_orig == 1 & result$.id_dest == 2, ]
  expect_equal(nrow(pair), 1)
  expect_equal(pair$metodo, "grafo")
})
```

- [ ] **Step 2: Implement `completar_distancias()`**

Create `R/completar_distancias.R`:

```r
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
  na_src_ids <- paste0("src_", dist_df$.id_orig[na_rows])
  na_dst_ids <- paste0("dst_", dist_df$.id_dest[na_rows])

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
```

- [ ] **Step 3: Run tests**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-completar_distancias.R")'`
Expected: All tests pass.

- [ ] **Step 4: Run `devtools::document()` and `devtools::check()`**

Run: `Rscript -e 'devtools::document(); devtools::check()'`
Expected: No errors. `completar_distancias` added to NAMESPACE.

- [ ] **Step 5: Commit**

```bash
git add R/completar_distancias.R tests/testthat/test-completar_distancias.R man/ NAMESPACE
git commit -m "feat: add completar_distancias for augmented graph routing"
```

---

## Task 6: Documentation and Final Check

Update roxygen documentation, run full package check, ensure everything is clean.

**Files:**
- Modify: `R/completar_distancias.R` (verify roxygen is complete)
- Modify: `R/get_distancias_osrm.R` (update roxygen for new params)
- Run: `devtools::document()`, `devtools::check()`

- [ ] **Step 1: Verify `get_distancias_osrm` roxygen includes `preencher_na`**

Ensure the `@param preencher_na` and `@return` sections in `R/get_distancias_osrm.R` document:
- New `preencher_na` parameter behavior
- `.id_orig` and `.id_dest` columns in output
- `metodo` column in output
- Snap attributes on the return value

- [ ] **Step 2: Add `@seealso` cross-reference in `get_distancias_osrm`**

In `R/get_distancias_osrm.R`:
```r
#' @seealso [completar_distancias()] to complete NA pairs via augmented graph.
```

- [ ] **Step 3: Run full package check**

Run: `Rscript -e 'devtools::document(); devtools::check()'`
Expected: 0 errors, 0 warnings, 0 notes (or only pre-existing notes).

- [ ] **Step 4: Run all tests**

Run: `Rscript -e 'devtools::test()'`
Expected: All tests pass.

- [ ] **Step 5: Commit**

```bash
git add R/ man/ NAMESPACE
git commit -m "docs: update roxygen for completar_distancias and new params"
```
