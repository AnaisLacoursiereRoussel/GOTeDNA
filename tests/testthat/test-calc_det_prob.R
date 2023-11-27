test_that("calc_det_prob() calculates detection probability", {
  expect_equal(calc_det_prob(D_mb, ecodistrict.select = "Scotian Shelf"),
               list("newP_agg", "newP_yr"))
})
