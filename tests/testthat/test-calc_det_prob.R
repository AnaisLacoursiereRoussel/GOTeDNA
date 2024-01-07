test_that("calc_det_prob() calculates detection probability", {
  expect_true(is.list(newprob))
  expect_identical(names(newprob), c("newP_agg", "newP_yr"))
  #
  expect_identical(
    length(newprob$newP_agg),
    with(
      D_mb_ex,
      paste0(GOTeDNA_ID, ";", scientificName, ";", target_subfragment)
    ) |> unique() |> length()
  )
  expect_identical(
    names(newprob$newP_agg[1]),
    "8;Prionospio steenstrupi;COI1"
  )
  expect_identical(
    dim(newprob$newP_agg[[1]]), c(12L, 5L)
  )
  tmp <- newprob$newP_agg[[1]][1, ]
  tmp$p <- round(tmp$p, 5)
  tmp$s <- round(tmp$s, 5)
  expect_identical(
    tmp,
    structure(list(
      month = 1,
      n = 15L,
      nd = 2,
      p = 0.13333,
      s = 0.08777
    ), row.names = 1L, class = "data.frame")
  )
  expect_true(
    all(newprob$newP_agg[[1]]$ecodistrict == "Scotian Shelf")
  )
  #
  expect_identical(
    length(newprob$newP_yr),
    with(
      D_mb_ex,
      paste0(GOTeDNA_ID, ";", scientificName, ";", target_gene, ";", year)
    ) |> unique() |> length()
  )
  expect_identical(
    names(newprob$newP_yr[1]),
    "8;Prionospio steenstrupi;COI1;2021"
  )
  expect_identical(
    dim(newprob$newP_yr[[1]]), c(2L, 9L)
  )
  expect_true(
    all(newprob$newP_yr[[1]]$ecodistrict == "Scotian Shelf")
  )
})
