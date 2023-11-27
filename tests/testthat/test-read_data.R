test_that("read_data() works", {
  expect_equal(D_mb <- read_data(choose.method="metabarcoding", path.folder=NULL),
               dplyr::tibble(D_mb))
  expect_equal(D_mb <- read_data(choose.method="metabarcoding", path.folder="C:/Users/morrisonme/Documents/R/data/"),
               dplyr::tibble(D_mb))
  expect_equal(D_qp <- read_data(choose.method="qPCR", path.folder="C:/Users/morrisonme/Documents/R/data/"),
               data.frame(D_qp))

})
