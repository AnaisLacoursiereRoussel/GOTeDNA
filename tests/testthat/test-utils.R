# test replace_to_snakecase()
test_that("replace_to_snakecase() works", {
    # test that it works 
    expect_equal(replace_to_snakecase("testConv"), "test_conv")
    # test edge case
    expect_equal(replace_to_snakecase(""), "")
})

# test replace_to_snakecase()
test_that("replace_to_snakecase() works", {
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