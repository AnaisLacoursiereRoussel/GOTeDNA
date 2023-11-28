test_that("calc_det_prob() calculates detection probability", {
  expect_equal(newprob <- calc_det_prob(data = read_data("metabarcoding",test_path("testdata")),
                                        ecodistrict.select = "Scotian Shelf"),
               as.list(newprob))
})

