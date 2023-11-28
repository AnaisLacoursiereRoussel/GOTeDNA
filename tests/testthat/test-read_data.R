test_that("read_data() works", {
  expect_equal(D_mb <- read_data(choose.method="metabarcoding", path.folder=test_path("testdata")),
               dplyr::tibble(D_mb))
  expect_equal(D_qp <- read_data(choose.method="qPCR", path.folder=test_path("testdata")),
               data.frame(D_qp))

})
