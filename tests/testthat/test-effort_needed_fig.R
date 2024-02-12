test_that("effort_needed_fig returns a ggplot2 object", {
  p <- effort_needed_fig(
    scaledprobs$Pscaled_month |> dplyr::filter(
      species == "Acartia hudsonica",
      primer == "COI1"
    )
  )
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")

})
