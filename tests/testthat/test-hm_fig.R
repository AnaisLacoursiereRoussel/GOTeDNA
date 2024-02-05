test_that("hm_fig returns a ggplot2 object", {
  p <- hm_fig(taxon.level = "class", taxon.name = "Copepoda", scaledprobs)
  expect_s3_class(p$layers[[1]]$geom, "GeomRect")
  expect_s3_class(p$layers[[2]]$geom, "GeomTile")
  expect_error(
    hm_fig(taxon.level = "wrong", taxon.name = "Copepoda", scaledprobs),
    'should be one of "phylum", "class"'
  )
})
