test_that("hm_fig returns a ggplot2 object", {
  p <- hm_fig(taxon.level = "class", taxon.name = "Copepoda", ecodistrict.select = "Scotian Shelf", scaledprobs)
  expect_s3_class(p$layers[[1]]$geom, "GeomRect")
  expect_s3_class(p$layers[[2]]$geom, "GeomTile")

  expect_error(
    hm_fig(taxon.level = "class", taxon.name = "Copepoda", ecodistrict.select = "Bay of Fundy", scaledprobs),
    "Ecodistrict not found in data"
  )
  expect_error(
    hm_fig(taxon.level = "wrong", taxon.name = "Copepoda",
      ecodistrict.select = "Scotian Shelf", scaledprobs),
    'should be one of "phylum", "class"'
  )
})
