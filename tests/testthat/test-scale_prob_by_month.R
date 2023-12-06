test_that("monthly probabilities are scaled and missing months are interpolated", {
  expect_equal(
    Pscaled_agg <- scale_prob_by_month(
      data = D_mb_ex, 
      ecodistrict.select = "Scotian Shelf"
    ),
               dplyr::tibble(Pscaled_agg))
})
