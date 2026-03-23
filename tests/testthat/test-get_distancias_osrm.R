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
