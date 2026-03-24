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

  # Detect symmetric case: src and dst are the same set of points
  symmetric <- identical(nrow(src), nrow(dst)) &&
    identical(sf::st_coordinates(src), sf::st_coordinates(dst))

  n_src <- nrow(src)
  n_dst <- nrow(dst)
  src_ids <- paste0("src_", seq_len(n_src))

  # For symmetric case, reuse src_* IDs for destinations
  if (symmetric) {
    dst_ids <- src_ids
  } else {
    dst_ids <- paste0("dst_", seq_len(n_dst))
  }

  # Snap classification — extract snap_km from dist_df columns
  # (st_distance would give 0 for unsnappable points that retain original geom)
  if (has_snap) {
    # Get unique snap_km per source index
    snap_km_src <- vapply(seq_len(n_src), function(i) {
      vals <- dist_df$snap_km_orig[dist_df$.id_orig == i]
      if (all(is.na(vals))) NA_real_ else vals[!is.na(vals)][1]
    }, numeric(1))
    snap_h_src <- snap_km_src / kmh_snap
    ok_src <- !is.na(snap_km_src) & snap_h_src <= max_snap_h

    snap_src_ids <- paste0("snap_src_", seq_len(n_src))

    if (symmetric) {
      snap_km_dst <- snap_km_src
      snap_h_dst <- snap_h_src
      ok_dst <- ok_src
      snap_dst_ids <- snap_src_ids
    } else {
      snap_km_dst <- vapply(seq_len(n_dst), function(j) {
        vals <- dist_df$snap_km_dest[dist_df$.id_dest == j]
        if (all(is.na(vals))) NA_real_ else vals[!is.na(vals)][1]
      }, numeric(1))
      snap_h_dst <- snap_km_dst / kmh_snap
      ok_dst <- !is.na(snap_km_dst) & snap_h_dst <= max_snap_h
      snap_dst_ids <- paste0("snap_dst_", seq_len(n_dst))
    }

    unsn_src <- which(!ok_src)
    unsn_dst <- if (symmetric) integer(0) else which(!ok_dst)
  } else {
    ok_src <- rep(FALSE, n_src)
    ok_dst <- rep(FALSE, n_dst)
    unsn_src <- seq_len(n_src)
    unsn_dst <- if (symmetric) integer(0) else seq_len(n_dst)
    snap_src_ids <- character(0)
    snap_dst_ids <- character(0)
  }

  # --- Build edge list ---
  edges <- list()

  # 1. Road edges (non-NA, non-self pairs) -- averaged for undirected graph
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
    # For asymmetric case, also add dst snap edges
    if (!symmetric) {
      for (j in which(ok_dst)) {
        snap_edge_list[[length(snap_edge_list) + 1]] <- data.frame(
          from = dst_ids[j], to = snap_dst_ids[j],
          weight = snap_h_dst[j], stringsAsFactors = FALSE
        )
      }
    }
    if (length(snap_edge_list) > 0) {
      edges$snap <- dplyr::bind_rows(snap_edge_list)
    }
  }

  # 3. Unsnappable point edges (pairwise Euclidean among unsnappable points)
  unsn_ids <- c(src_ids[unsn_src], dst_ids[unsn_dst])
  unsn_ids <- unique(unsn_ids)
  unsn_pts_list <- list()
  for (uid in unsn_ids) {
    if (grepl("^src_", uid)) {
      idx <- as.integer(sub("^src_", "", uid))
      unsn_pts_list[[uid]] <- src[idx, ]
    } else {
      idx <- as.integer(sub("^dst_", "", uid))
      unsn_pts_list[[uid]] <- dst[idx, ]
    }
  }
  if (length(unsn_ids) > 1) {
    unsn_pts <- do.call(rbind, unsn_pts_list)
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
  all_node_ids <- unique(c(src_ids, dst_ids))
  if (has_snap) {
    all_node_ids <- unique(c(
      all_node_ids,
      snap_src_ids[ok_src], snap_dst_ids[ok_dst]
    ))
  }

  if (nrow(all_edges) == 0) {
    # No edges at all — create empty graph with all nodes
    g <- igraph::make_empty_graph(n = 0, directed = FALSE)
    g <- igraph::add_vertices(g, length(all_node_ids), name = all_node_ids)
  } else {
    # Build initial undirected graph
    g <- igraph::graph_from_data_frame(all_edges, directed = FALSE)
    missing_nodes <- setdiff(all_node_ids, igraph::V(g)$name)
    if (length(missing_nodes) > 0) {
      g <- igraph::add_vertices(g, length(missing_nodes), name = missing_nodes)
    }
  }

  # 4. Component bridges: each vertex -> nearest in every other component
  g <- adicionar_pontes_componentes(
    g, src, dst, snap_src, snap_dst, ok_src, ok_dst, kmh_snap, symmetric
  )

  # Deduplicate edges: keep minimum weight
  if (igraph::any_multiple(g)) {
    g <- igraph::simplify(g, edge.attr.comb = list(weight = "min"))
  }

  g
}

#' Add component bridge edges to ensure full connectivity
#'
#' For each vertex in each component, adds an Euclidean edge to its nearest
#' vertex in every other connected component.
#'
#' @noRd
adicionar_pontes_componentes <- function(g, src, dst, snap_src, snap_dst,
                                          ok_src, ok_dst, kmh_snap,
                                          symmetric) {
  comps <- igraph::components(g)
  if (comps$no <= 1) return(g)

  cli::cli_inform("Bridging {comps$no} disconnected components...")

  node_geom <- construir_geometria_nos(
    src, dst, snap_src, snap_dst, ok_src, ok_dst, symmetric
  )

  node_names <- igraph::V(g)$name
  membership <- comps$membership
  names(membership) <- node_names

  bridge_edges <- list()

  # For each pair of components, connect each node in comp_a to its nearest

  # node in comp_b. This guarantees full connectivity even when multiple

  # small components are closer to each other than to the main component.
  comp_ids <- seq_len(comps$no)
  for (a in comp_ids) {
    nodes_a <- unname(intersect(node_names[membership == a], names(node_geom)))
    if (length(nodes_a) == 0) next

    sf_a <- sf::st_sf(
      id = nodes_a,
      geometry = sf::st_sfc(unname(node_geom[nodes_a]), crs = 4326)
    )

    for (b in comp_ids) {
      if (b <= a) next
      nodes_b <- unname(intersect(node_names[membership == b], names(node_geom)))
      if (length(nodes_b) == 0) next

      sf_b <- sf::st_sf(
        id = nodes_b,
        geometry = sf::st_sfc(unname(node_geom[nodes_b]), crs = 4326)
      )

      dists <- sf::st_distance(sf_a, sf_b)

      # From a → nearest in b
      for (i in seq_len(nrow(sf_a))) {
        nearest_j <- which.min(dists[i, ])
        eucl_km <- as.numeric(dists[i, nearest_j]) / 1000
        bridge_edges[[length(bridge_edges) + 1]] <- data.frame(
          from = nodes_a[i], to = nodes_b[nearest_j],
          weight = eucl_km / kmh_snap, stringsAsFactors = FALSE
        )
      }

      # From b → nearest in a
      for (j in seq_len(nrow(sf_b))) {
        nearest_i <- which.min(dists[, j])
        eucl_km <- as.numeric(dists[nearest_i, j]) / 1000
        bridge_edges[[length(bridge_edges) + 1]] <- data.frame(
          from = nodes_b[j], to = nodes_a[nearest_i],
          weight = eucl_km / kmh_snap, stringsAsFactors = FALSE
        )
      }
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
                                     ok_src, ok_dst, symmetric) {
  geom <- list()
  n_src <- nrow(src)

  for (i in seq_len(n_src)) {
    geom[[paste0("src_", i)]] <- sf::st_geometry(src[i, ])[[1]]
  }

  if (!symmetric) {
    n_dst <- nrow(dst)
    for (j in seq_len(n_dst)) {
      geom[[paste0("dst_", j)]] <- sf::st_geometry(dst[j, ])[[1]]
    }
  }

  if (!is.null(snap_src)) {
    for (i in which(ok_src)) {
      geom[[paste0("snap_src_", i)]] <- sf::st_geometry(snap_src[i, ])[[1]]
    }
  }
  if (!is.null(snap_dst) && !symmetric) {
    for (j in which(ok_dst)) {
      geom[[paste0("snap_dst_", j)]] <- sf::st_geometry(snap_dst[j, ])[[1]]
    }
  }

  geom
}
