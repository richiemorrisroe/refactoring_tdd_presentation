context("integration test prep_modelling")
old <- readr::read_csv("../../inst/prep_modelling_output_old.csv")
new <- readr::read_csv("../../inst/prep_modelling_output_refactor.csv")
test_that(
        "data.frame outputs are equal",
        expect_equal(old, new)
)
