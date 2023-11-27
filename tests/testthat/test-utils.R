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
