test_that("snap_points returns list with pontos and snap_km", {
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
