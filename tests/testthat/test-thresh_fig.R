test_that("thresh_fig returns a ggplot2 object", {
  local_edition(3)
  newP_agg <<- calc_det_prob(read_data("metabarcoding",test_path("testdata")), "Scotian Shelf")[["newP_agg"]]
  Pscaled_agg <<- scale_prob_by_month(read_data("metabarcoding",test_path("testdata")),
                                      ecodistrict.select = "Scotian Shelf")

  vdiffr::expect_doppelganger(
    title = "thresh_fig",
    fig = thresh_fig(taxon.level = "species", taxon.name = "Acartia hudsonica", threshold = "90", ecodistrict.select = "Scotian Shelf"))
})
