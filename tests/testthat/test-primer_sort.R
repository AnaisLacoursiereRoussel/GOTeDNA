test_that("primer_sort() sorts primers by percent rank for each data request", {
  expect_equal(
    sorted_pr <- primer_sort(
      taxon.level = "genus", scaledprobs$Pscaled_month),
    class(sorted_pr) = c("tbl_df", "tbl", "data.frame")
    )
})
