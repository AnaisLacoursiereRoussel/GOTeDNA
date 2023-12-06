test_that("smooth_fig returns a ggplot2 object", {
  p <- smooth_fig(
    data = D_mb_ex, species.name = "Acartia longiremis",
    primer.select = "COI1", ecodistrict.select = "Scotian Shelf"
  )

  expect_s3_class(p$coordinates, "CoordPolar")
  expect_identical(p$layers[[1]]$geom$required_aes, "yintercept")
  expect_identical(p$layers[[2]]$geom$required_aes, "xintercept")
  expect_s3_class(p$layers[[3]]$geom, "GeomPath")
  expect_s3_class(p$layers[[4]]$geom, "GeomPoint")
  expect_error(
    smooth_fig(
      data = D_mb_ex, species.name = "A. longiremis",
      primer.select = "COI1", ecodistrict.select = "Scotian Shelf"
    ),
    "Species not found in data"
  )
  expect_error(
    smooth_fig(
      data = D_mb_ex, species.name = "Acartia longiremis",
      primer.select = "COI1", ecodistrict.select = "Bay of Fundy"
    ),
    "Ecodistrict not found in data"
  )
  expect_error(
    smooth_fig(
      data = D_mb_ex, species.name = "Acartia longiremis",
      primer.select = "COI", ecodistrict.select = "Scotian Shelf"
    ),
    "Primer not found in data"
  )
  expect_warning(
    smooth_fig(
      data = D_mb_ex, species.name = "Acartia longiremis",
      primer.select = "COI1", ecodistrict.select = "Scotian Shelf"
    ),
    "minimal value for n is 3, returning requested palette with 3 different levels"
  )
})
