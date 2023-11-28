test_that("monthly probabilities are scaled and missing months are interpolated", {
  expect_equal(scale_mth <- scale_prob_by_month(data = read_data("metabarcoding",test_path("testdata")),
                                                ecodistrict.select = "Scotian Shelf"),
               dplyr::tibble(scale_mth))
})
