test_that("sample_size_fig returns a ggplot2 object", {

  p <- sample_size_fig(data = D_mb, species.name = "Acartia longiremis", ecodistrict.select = "Scotian Shelf")

  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_error(sample_size_fig(data = D_mb, species.name = "A. longiremis", ecodistrict.select = "Scotian Shelf"),
               "Species not found in data")
  expect_error(sample_size_fig(data = D_mb, species.name = "Acartia longiremis", ecodistrict.select = "Bay of Fundy"),
               "Ecodistrict not found in data")
})
