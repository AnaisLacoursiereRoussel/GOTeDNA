test_that("calc_window() returns optimal detection windows with confidence values", {
  expect_equal(
    win_df <- calc_window(
      data = D_mb_ex, ecodistrict.select = "Scotian Shelf", threshold = "90",
      show.variation = "within_year", species.name = "Acartia longiremis"
    ),
    as.data.frame(win_df)
  )
  expect_equal(
    calc_window(
      data = D_mb_ex, ecodistrict.select = "Scotian Shelf", threshold = "90",
      show.variation = "within_year", species.name = "Acartia hudsonica"
    ),
    "No optimal detection window"
  )
})

test_that("error is thrown when value doesn't exist in data", {
  expect_error(
    calc_window(
      data = D_mb_ex, ecodistrict.select = "Bay of Fundy", threshold = "90",
      show.variation = "within_year", species.name = "Acartia longiremis"
    ),
    "Ecodistrict not found in data"
  )
  expect_error(
    calc_window(
      data = D_mb_ex, ecodistrict.select = "Scotian Shelf", threshold = "90",
      show.variation = "within_year", species.name = "A. longiremis"
    ),
    "Species not found in data"
  )
})
