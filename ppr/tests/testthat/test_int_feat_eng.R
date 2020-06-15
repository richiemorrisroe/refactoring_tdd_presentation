context("integration test feature engineering")
old <- readr::read_csv("../../inst/feature_eng_results_old.csv")
new <- readr::read_csv("../../inst/feature_eng_results.csv")
test_that(
        "data.frame outputs are equal",
        expect_equal(old, new)
)
