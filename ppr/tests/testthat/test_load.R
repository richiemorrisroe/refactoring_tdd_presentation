require(testthat)
context("load data")
test_that('load_data exists',
  {expect_equal(
            class(load_data("~/Dropbox/PPR/PPR-ALL.xlsx"))[1], "tbl_df")})
