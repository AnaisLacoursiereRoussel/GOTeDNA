test_that("effort_needed_fig returns a ggplot2 object", {
  p <- effort_needed_fig(species.name = "Acartia hudsonica", primer.select = "COI1", scaledprobs)

  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
  expect_error(
    effort_needed_fig(species.name = "Acartia hudsonica", primer.select = "18S2", scaledprobs),
    "Primer not found in data"
  )
})
