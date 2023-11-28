test_that("monthly probabilities are scaled for each year and missing months are interpolated", {

  newP_yr <<- calc_det_prob(read_data("metabarcoding",test_path("testdata")), "Scotian Shelf")[["newP_yr"]]

  expect_equal(scale_yr <- scale_prob_by_year(data = read_data("metabarcoding",test_path("testdata")),
                                              ecodistrict.select = "Scotian Shelf"),
               dplyr::tibble(scale_yr))
})

