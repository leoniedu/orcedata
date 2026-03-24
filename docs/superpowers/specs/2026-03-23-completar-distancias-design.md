# `completar_distancias()` — Augmented Graph Routing for Disconnected Components

## Problem

`get_distancias_osrm()` returns NA for point pairs on disconnected road network components. The current Euclidean fallback treats each pair independently — no route structure. For the optimizer in `orce`, this overestimates the cost of serving clusters of disconnected tracts, because there's no notion of intermediate routing through shared paths.

## Solution

A post-processing function `completar_distancias()` that takes the output of `get_distancias_osrm()` (with NAs preserved) and the original sf points, builds an augmented graph (road + snap + Euclidean bridging edges), computes shortest paths via igraph, and returns a complete distance matrix with no NAs.

## API

### Changes to `get_distancias_osrm()`

```r
get_distancias_osrm <- function(src, dst = NULL,
                                kmh_snap = 2,
                                chunk_size = 100L,
                                preencher_na = FALSE)
```

- **`preencher_na`**: defaults to `FALSE` (breaking change — acceptable since the package is pre-release at 0.0.0.9000). When `FALSE`, NAs are preserved in `distancia_km` and `duracao_horas`. When `TRUE`, Euclidean fallback fills NAs (legacy behavior).
- `get_distancias_osrm()` also emits `metodo = "osrm"` for all rows when `preencher_na = FALSE`, and `metodo = "osrm"` or `"euclidiano"` when `preencher_na = TRUE`.

### Changes to `snap_points()`

Returns a **list** with two elements:

- `$pontos`: an sf POINT object of snapped coordinates. Points that fail to snap retain their original geometry.
- `$snap_km`: numeric vector of snap distances in km. `NA` for points that failed to snap.

This replaces the current plain numeric vector return. `get_distancias_osrm()` internals are updated to use `snap_result$snap_km` where it currently indexes the vector directly.

### Snap attributes on output

`get_distancias_osrm()` attaches snapped sf objects as attributes on the result:

```r
attr(result, "snap_src") <- snap_src_result$pontos  # sf POINT
attr(result, "snap_dst") <- snap_dst_result$pontos  # sf POINT
attr(result, "kmh_snap") <- kmh_snap                # for consistency check
```

These carry both the snapped coordinates and snap distances, needed by `completar_distancias()`.

### New function: `completar_distancias()`

```r
completar_distancias <- function(dist_df, src, dst = NULL,
                                  kmh_snap = 2,
                                  max_snap_h = 1)
```

**Parameters:**

- `dist_df`: output of `get_distancias_osrm(preencher_na = FALSE)` — long data.frame with NAs for unroutable pairs.
- `src`: sf POINT object with origin points (same as passed to `get_distancias_osrm()`).
- `dst`: sf POINT object with destination points. `NULL` for symmetric (src-to-src) matrices.
- `kmh_snap`: speed (km/h) for Euclidean and snap edges. Default: `2`. If `attr(dist_df, "kmh_snap")` exists and differs, a warning is emitted.
- `max_snap_h`: maximum snap duration (hours) before a point is reclassified as unsnappable. Default: `1` (equivalent to 2 km at 2 km/h). Points with snap time > `max_snap_h` are treated as snap = NA.

**Snap info handling:**

1. If `attr(dist_df, "snap_src")` and `attr(dist_df, "snap_dst")` exist: use snap locations to build snap edges and identify unsnappable points.
2. If snap attributes are missing: no snap edges. Graph uses original points as nodes. Road edges use OSRM durations as-is. Assumes distances already account for snapping (or user chose to ignore it).

**Diagnostics:** Uses `cli::cli_inform()` for progress messages and `cli::cli_warn()` for reclassification/bridging reports, consistent with `get_distancias_osrm()`.

## Graph Construction

All edge weights are in **hours** (duration).

The graph is **undirected**. OSRM durations are slightly asymmetric (one-way streets), but the optimization in `orce` treats travel cost as symmetric. Euclidean and snap edges are inherently symmetric. Using undirected simplifies the graph and halves edge count. For each OSRM pair, the edge weight is the average of the two directions (when both are available).

### Nodes

Each node is identified by a type prefix + row index: `"src_1"`, `"dst_5"`, `"snap_src_1"`, `"snap_dst_5"`. This allows unambiguous mapping back to input rows.

All unique points from `src` and `dst` are nodes. When snap locations are available, snapped points are also nodes (one per original point that snapped successfully within `max_snap_h`).

### Edge Types

| Edge type | Condition | Weight (hours) |
|-----------|-----------|----------------|
| **Road** | Non-NA pairs in `dist_df` | `duracao_horas` from OSRM |
| **Snap** | Snap locations available, `snap_h <= max_snap_h` | `snap_km / kmh_snap` |
| **Unsnappable** | `snap_km = NA` or `snap_h > max_snap_h`: connect to each other + nearest vertex of each component | `euclidean_km / kmh_snap` |
| **Component bridge** | >1 connected component: each vertex → nearest vertex in every other component | `euclidean_km / kmh_snap` |

### Construction Steps

1. Add road edges from non-NA OSRM durations. Road edges connect snap nodes (if available) or original nodes (if no snap info).
2. If snap info available: add snap edges (original point → snap point) for points with `snap_h <= max_snap_h`.
3. Reclassify points with `snap_h > max_snap_h` as unsnappable. Their snap geometry is discarded (not used as a graph node).
4. Add Euclidean edges among all unsnappable points (pairwise). "Nearest" is always Euclidean distance between original point coordinates.
5. Add Euclidean edges from each unsnappable point to the nearest vertex of each connected component.
6. Detect connected components. If >1: for each vertex, add Euclidean edge to its nearest vertex in every other component.
7. If duplicate edges exist between the same pair of nodes, keep the one with minimum weight.

### Self-pairs

In symmetric mode (`dst = NULL`), self-pairs (same origin and destination) are forced to `duracao_horas = 0` and `distancia_km = 0` in the output, regardless of graph routing.

### Missing rows from failed chunks

`get_distancias_osrm()` may drop entire chunks on error (rows absent, not NA). `completar_distancias()` detects missing src-dst combinations by comparing expected pairs against `dist_df` rows, and treats missing pairs the same as NA pairs — they are completed via graph routing.

### Routing

`igraph::distances()` computes shortest paths between all src-dst pairs on the augmented graph.

For connected pairs: road duration dominates (faster than Euclidean at `kmh_snap`).
For disconnected pairs: best path through intermediate nodes, crossing component bridges.

## Output

Same long data.frame format as `get_distancias_osrm()`:

- `duracao_horas`: OSRM duration for routable pairs; graph shortest-path duration for previously-NA pairs.
- `distancia_km`: retained from OSRM for routable pairs; for graph-routed pairs, computed as `duracao_horas * kmh_snap` (a travel-time-equivalent distance, not road distance).
- `snap_km_orig`, `snap_km_dest`: retained from input.
- `metodo`: `"osrm"` for pairs routed by OSRM, `"grafo"` for pairs completed by the augmented graph.
- All other columns from `src`/`dst` attributes preserved.

## Dependencies

- **igraph**: added to `Suggests` (used for graph construction and shortest paths). Guarded with `rlang::check_installed("igraph")`.
- **sf**: already in `Imports` (used for Euclidean distance computation and nearest-neighbor lookups).

## Package Location

`orcedata` — this is a distance computation function, not optimization logic.

## File Structure

- `R/completar_distancias.R`: main function + graph construction helpers.
- `R/get_distancias_osrm.R`: modified — `preencher_na` parameter, snap attributes on output, `metodo` column.
- `R/snap_points.R`: extracted from `get_distancias_osrm.R`, returns list with sf POINT object + snap distances.

## Example Usage

```r
# Start OSRM server
osrm_local_start("south-america/brazil/nordeste-latest.osm.pbf")

# Compute distances with NAs preserved
dist <- get_distancias_osrm(agencias, setores)

# Complete the matrix via augmented graph
dist_completa <- completar_distancias(dist, agencias, setores)

# Check what was graph-routed
table(dist_completa$metodo)
#> osrm  grafo
#> 45200   800
```
