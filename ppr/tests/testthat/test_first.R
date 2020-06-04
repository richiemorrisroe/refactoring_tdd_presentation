require(testthat)
context("load data")
dat  <- load_data("~/Dropbox/PPR/PPR-ALL.xlsx")
test_that('load_data returns a tibble',
  {expect_equal(
            class(dat)[1], "tbl_df")})

finished_names <- as.character(c("date_of_sale_dd_mm_yyyy",
                                 "address", "postal_code", "county",                   
                                 "price", "not_full_market_price","vat_exclusive",
                                 "description_of_property",  "property_size_description"))

test_that('normalise_names works',
          {expect_equal(finished_names, names(normalise_names(dat)))})

test_that('fix price returns the correct price',
          expect_equal(3e6, fix_price("3,000,000")))

test_that('fix price returns the correct price',
          expect_equal(3000000.01, fix_price("3,000,000.01")))

test_df_output  <- readr::read_csv("~/Dropbox/Code/Rlang/refactoring_and_tdd/ppr/mark_values_as_large_test_data_done.csv")
test_df_input  <- readr::read_csv("~/Dropbox/Code/Rlang/refactoring_and_tdd/ppr/mark_values_as_large_test_data.csv")
test_that('mark values as large works', {
  expect_equal(mark_values_as_large(test_df_input, large = 1e6),
               test_df_output)
})

test_that('log(price)< price',{
          new_df <-  log_column(df, column)
          expect_lt(new_df$log_price,
                    df$price)}
          )
