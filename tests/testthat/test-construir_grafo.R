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
