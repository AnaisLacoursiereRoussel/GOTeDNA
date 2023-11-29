test_that("monthly probabilities are scaled for each year and missing months are interpolated", {
  expect_equal(Pscaled_yr <- scale_prob_by_year(data = D_mb, ecodistrict.select = "Scotian Shelf"),
               dplyr::tibble(Pscaled_yr))
})

