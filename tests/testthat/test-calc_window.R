test_that("calc_window() returns optimal detection windows with confidence values", {
  expect_equal(
    win_df <- calc_window(
      data = D_mb_ex, threshold = "90", species.name = "Acartia longiremis", scaledprobs),
    structure(list(
      length = 2L, threshold = "90",
      period = "Jan-Dec",
      GOTeDNA_ID = "8",
      species = "Acartia longiremis",
      primer = "COI1",
      phylum = "Arthropoda",
      class = "Copepoda",
      order = "Calanoida",
      family = "Acartiidae", genus = "Acartia",
      `odds ratio` = 5.51172767056909,
      `p value` = "<0.001",
      `Lower CI` = 2.74524770500307,
      `Upper CI` = 11.2555740748163,
      confidence = "Very high"
    ), row.names = c(
      NA,
      -1L
    ), class = c("tbl_df", "tbl", "data.frame"))
  )
  expect_warning(
    calc_window(
      data = D_mb_ex, threshold = "90", species.name = "Acartia hudsonica", scaledprobs
    )
  )
})

# test_that("error is thrown when value doesn't exist in data", {
#   expect_error(
#     calc_window(
#       data = D_mb_ex, threshold = "90", species.name = "A. longiremis", scaledprobs
#     ),
#     "Species not found in data"
#   )
# })
