test_that("smooth_fig returns a ggplot2 object", {
  D_mb_ex_slc <- D_mb_ex |> dplyr::filter(target_subfragment == "COI1")
  suppressWarnings(p <- smooth_fig(
    data = D_mb_ex_slc, species.name = "Acartia longiremis"
  ))

  expect_s3_class(p$coordinates, "CoordPolar")
  expect_identical(p$layers[[1]]$geom$required_aes, "yintercept")
  expect_identical(p$layers[[2]]$geom$required_aes, "xintercept")
  expect_s3_class(p$layers[[3]]$geom, "GeomPath")
  expect_s3_class(p$layers[[4]]$geom, "GeomPoint")
  # expect_error(
  #   smooth_fig(
  #     data = D_mb_ex_slc, species.name = "A. longiremis"
  #   ),
  #   "Species not found in data"
  # )
  # expect_error(
  #   smooth_fig(
  #     data = D_mb_ex_slc, species.name = "Acartia longiremis"
  #   ),
  #   "Primer not found in data"
  # )
  expect_warning(
    smooth_fig(data = D_mb_ex_slc, species.name = "Acartia longiremis"),
    "minimal value for n is 3, returning requested palette with 3 different levels"
  )
})
