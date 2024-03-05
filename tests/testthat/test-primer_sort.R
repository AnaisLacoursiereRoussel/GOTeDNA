test_that("primer_sort() sorts primers by percent rank for each data request", {
  sorted_pr <- primer_sort(
    taxon.level = "genus", scaledprobs$Pscaled_month
  )

  expect_true(inherits(sorted_pr, "tbl_df"))
  expect_true(inherits(sorted_pr, "data.frame"))
})
