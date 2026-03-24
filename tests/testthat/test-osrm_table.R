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
