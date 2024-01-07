test_that("sample_size_fig returns a ggplot2 object", {
  p <- sample_size_fig(data = D_mb_ex, species.name = "Acartia longiremis")

  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_error(
    sample_size_fig(data = D_mb_ex, species.name = "A. longiremis"),
    "Species not found in data"
  )
})
