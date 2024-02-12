test_that("higher_tax_fig returns a ggplot2 object", {
  D_mb_ex_slc <- D_mb_ex |> dplyr::filter(target_subfragment == "COI1")
  p <- higher_tax_fig(
    data = D_mb_ex_slc,
    higher.taxon.select = "phylum",
    taxon.name = "Bryozoa",
    view.by.level = "genus"
  )

  expect_s3_class(p$layers[[1L]]$geom, "GeomPoint")
  # expect_error(
  #   higher_tax_fig(
  #     data = D_mb_ex, higher.taxon.select = "phylum", taxon.name = "Bryozoa",
  #     view.by.level = "genus", primer.select = "COI"
  #   ),
  #   "Primer not found in data"
  # )
  expect_error(
    higher_tax_fig(
      data = D_mb_ex_slc,
      higher.taxon.select = "phylum",
      taxon.name = "Bryo",
      view.by.level = "genus"
    ),
    "Taxon not found in data"
  )
})
