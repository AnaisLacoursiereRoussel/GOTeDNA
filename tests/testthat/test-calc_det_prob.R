test_that("calc_det_prob() calculates detection probability", {
  expect_equal(newprob <- calc_det_prob(data = D_mb, ecodistrict.select = "Scotian Shelf"),
               as.list(newprob))
})
