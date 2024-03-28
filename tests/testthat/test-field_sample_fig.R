test_that("field_sample_fig returns a ggplot2 object", {
  D_mb_ex_slc <- D_mb_ex |> dplyr::filter(target_subfragment == "COI1")
  p <- field_sample_fig(
    data = D_mb_ex_slc,
    taxon.select = "phylum",
    taxon.name = "Bryozoa"
  )

  expect_s3_class(p$layers[[1L]]$geom, "GeomPoint")

  expect_error(
    field_sample_fig(
      data = D_mb_ex_slc,
      taxon.select = "phylum",
      taxon.name = "Bryo"
    ),
    "Taxon not found in data"
  )
})
