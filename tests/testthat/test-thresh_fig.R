test_that("thresh_fig returns a ggplot2 object", {
   p <- thresh_fig(taxon.level = "species", taxon.name = "Acartia hudsonica", threshold = "90", ecodistrict.select = "Scotian Shelf")

   expect_s3_class(p$coordinates, "CoordPolar")
   expect_identical(p$layers[[1]]$geom$required_aes, "yintercept")
   expect_identical(p$layers[[2]]$geom$required_aes, "xintercept")
   expect_s3_class(p$layers[[3]]$geom, "GeomCol")
   expect_s3_class(p$layers[[4]]$geom, "GeomCol")
   expect_s3_class(p$layers[[5]]$geom, "GeomColPattern")
   expect_error(
      thresh_fig(taxon.level = "species", taxon.name = "Acartia hudsonica", threshold = "90", ecodistrict.select = "Bay of Fundy"),
      "Ecodistrict not found in data"
   )
})
