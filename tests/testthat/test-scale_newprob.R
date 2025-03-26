test_that("monthly probabilities are scaled and missing months are interpolated, years aggregated", {
  expect_identical(dim(scaledprobs$Pscaled_month), c(2988L, 14L))
  tmp <- scaledprobs$Pscaled_month[1L, ]
  tmp$scaleP <- round(tmp$scaleP, 5)
  tmp$fill <- round(tmp$fill, 5)
  expect_identical(
    tmp,
    structure(list(
      id = "8;Prionospio steenstrupi;COI1",
      month = 1L, detect = 2, nondetect = 13, scaleP = 0.17778,
      protocol_ID = "8", species = "Prionospio steenstrupi", primer = "COI1",
      phylum = "Annelida", class = "Polychaeta", order = "Spionida",
      family = "Spionidae", genus = "Prionospio", fill = 0.17778
    ), row.names = c(
      NA,
      -1L
    ), class = c("tbl_df", "tbl", "data.frame"))
  )
})


test_that("monthly probabilities are scaled and missing months are interpolated", {
  expect_identical(dim(scaledprobs$Pscaled_year), c(5976L, 15L))
  tmp <- scaledprobs$Pscaled_year[1L, ]
  expect_identical(
    tmp,
    structure(list(
      id = "8;Prionospio steenstrupi;COI1;2021",
      month = 1L,
      detect = NA_real_,
      nondetect = NA_real_,
      scaleP = NA_real_,
      protocol_ID = "8",
      species = "Prionospio steenstrupi",
      primer = "COI1",
      year = "2021",
      phylum = "Annelida",
      class = "Polychaeta",
      order = "Spionida",
      family = "Spionidae",
      genus = "Prionospio",
      fill = 0.5
    ),
    row.names = c(NA, -1L),
    class = c("tbl_df", "tbl", "data.frame"))
  )
  tmp <- scaledprobs$Pscaled_year[12L, ]
  expect_identical(
    tmp,
    structure(list(
      id = "8;Prionospio steenstrupi;COI1;2021",
      month = 12L,
      detect = 4,
      nondetect = 25,
      scaleP = 1,
      protocol_ID = "8",
      species = "Prionospio steenstrupi",
      primer = "COI1",
      year = "2021",
      phylum = "Annelida",
      class = "Polychaeta",
      order = "Spionida",
      family = "Spionidae",
      genus = "Prionospio",
      fill = 1
    ),
    row.names = c(NA, -1L),
    class = c("tbl_df", "tbl", "data.frame"))
  )
})
