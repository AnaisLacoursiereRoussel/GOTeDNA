test_that("higher_tax_fig returns a ggplot2 object", {
  p <- higher_tax_fig(
    data = D_mb_ex, higher.taxon.select = "phylum", taxon.name = "Bryozoa",
    view.by.level = "genus", ecodistrict.select = "Scotian Shelf",
    primer.select = "COI1"
  )

  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_error(
    higher_tax_fig(
      data = D_mb_ex, higher.taxon.select = "phylum", taxon.name = "Bryozoa",
      view.by.level = "genus", ecodistrict.select = "Scotian Shelf",
      primer.select = "COI"
    ),
    "Primer not found in data"
  )
  expect_error(
    higher_tax_fig(
      data = D_mb_ex, higher.taxon.select = "phylum", taxon.name = "Bryozoa",
      view.by.level = "genus", ecodistrict.select = "Bay of Fundy",
      primer.select = "COI1"
    ),
    "Ecodistrict not found in data"
  )
  expect_error(
    higher_tax_fig(
      data = D_mb_ex, higher.taxon.select = "phylum", taxon.name = "Bryo",
      view.by.level = "genus", ecodistrict.select = "Scotian Shelf",
      primer.select = "COI1"
    ),
    "Taxon not found in data"
  )
})
