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

  pair <- result[result$.id_orig == 1 & result$.id_dest == 2, ]
  expect_equal(nrow(pair), 1)
  expect_equal(pair$metodo, "grafo")
})
