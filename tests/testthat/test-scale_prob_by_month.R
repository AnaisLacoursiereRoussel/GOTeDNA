test_that("monthly probabilities are scaled and missing months are interpolated", {
  expect_equal(scale_prob_by_month(data = D_mb, ecodistrict.select = "Scotian Shelf"),
               data.frame())
})
