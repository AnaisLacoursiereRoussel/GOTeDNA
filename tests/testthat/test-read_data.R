test_that("read_data() works", {
  D_mb <- read_data(
    choose.method = "metabarcoding",
    path.folder = test_path("testdata")
  )
  expect_identical(dim(D_mb), c(40L, 22L))

  D_qp <- read_data(
    choose.method = "qPCR",
    path.folder = test_path("testdata")
  )
  expect_identical(dim(D_qp), c(50L, 23L))
  expect_identical(names(D_qp)[21], "year")
  expect_identical(names(D_qp)[1], "protocol_ID")

  expect_error(read_data("metawrong", test_path("testdata")))
})
