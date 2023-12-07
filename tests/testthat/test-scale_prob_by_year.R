test_that("monthly probabilities are scaled and missing months are interpolated", {
  expect_identical(dim(Pscaled_year), c(5976L, 16L))
  tmp <- Pscaled_year[1L, ]
  expect_identical(
    tmp,
    structure(list(
      id = "8;Prionospio steenstrupi;COI1;2021",
      ecodistrict = "Scotian Shelf",
      month = 1L,
      detect = NA_real_,
      nondetect = NA_real_,
      scaleP = NA_real_,
      GOTeDNA_ID = "8",
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
  tmp <- Pscaled_year[12L, ]
  expect_identical(
    tmp,
    structure(list(
      id = "8;Prionospio steenstrupi;COI1;2021",
      ecodistrict = "Scotian Shelf",
      month = 12L,
      detect = 4,
      nondetect = 25,
      scaleP = 1,
      GOTeDNA_ID = "8",
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
