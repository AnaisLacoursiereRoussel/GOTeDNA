test_that("smooth_fig returns a ggplot2 object", {
  D_mb_ex_slc <- D_mb_ex |>
    dplyr::filter(
      scientificName == "Acartia longiremis",
      target_subfragment == "COI1"
    )

  suppressWarnings(p <- smooth_fig(
    data = D_mb_ex_slc, species.name = "Acartia longiremis"
  ))

  expect_s3_class(p$coordinates, "CoordPolar")
  expect_identical(p$layers[[1]]$geom$required_aes, "yintercept")
  expect_identical(p$layers[[2]]$geom$required_aes, "xintercept")
  expect_s3_class(p$layers[[3]]$geom, "GeomPath")
  expect_s3_class(p$layers[[4]]$geom, "GeomPoint")

})
