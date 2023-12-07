# test scientific_name_formatter()
test_that("scientific_name_formatter() works", {
    # test that it works
    expect_equal(
        scientific_name_formatter("Ailuropoda"),
        bquote(paste(italic(.("Ailuropoda"))))
    )
    expect_equal(
        scientific_name_formatter("Ailuropoda melanoleuca"),
        bquote(paste(italic(.("Ailuropoda")) ~ italic(.("melanoleuca"))))
    )
    # test edge case
    expect_error(scientific_name_formatter(NULL))
})


test_that("scale_min_max() works", {
    expect_equal(scale_min_max(c(-1, 2, 4)), c(0, 0.6, 1))
}) 
