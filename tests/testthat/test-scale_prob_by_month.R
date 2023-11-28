test_that("monthly probabilities are scaled and missing months are interpolated", {

  newP_agg <<- calc_det_prob(read_data("metabarcoding",test_path("testdata")), "Scotian Shelf")[["newP_agg"]]

  expect_equal(scale_mth <- scale_prob_by_month(data = read_data("metabarcoding",test_path("testdata")),
                                                ecodistrict.select = "Scotian Shelf"),
               dplyr::tibble(scale_mth))
})
