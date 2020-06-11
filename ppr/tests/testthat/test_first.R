require(testthat)
context("load data")
dat  <- load_data("~/Dropbox/PPR/PPR-ALL.xlsx")
test_that('load_data returns a tibble',
  {expect_equal(
            class(dat)[1], "tbl_df")})

finished_names <-
  c("date_of_sale_dd_mm_yyyy",
    "address", "postal_code", "county",                   
    "price", "not_full_market_price","vat_exclusive",
    "description_of_property",  
    "property_size_description") %>%
  as.character()

test_that('normalise_names works',
{expect_equal(finished_names,
              names(normalise_names(dat)))
})

test_that('fix price returns the correct price',
          expect_equal(3e6, fix_price("3,000,000")))

test_that('fix price returns the correct price',
          expect_equal(3000000.01,
                       fix_price("3,000,000.01")))

test_df_output  <- readr::read_csv("../../inst/mark_values_as_large_test_data_done.csv")
test_df_input  <- readr::read_csv("../../inst/mark_values_as_large_test_data.csv")
test_that('mark values as large works', {
  expect_equal(mark_values_as_large(test_df_input,
                                    large = 1e6),
               test_df_output)
})

test_that('log(price)< price',
{
  data(ppr)
  ppr2 <- normalise_names(ppr) %>% 
    dplyr::mutate(price=fix_price(price))

  new_df <-  log_column(ppr2, price)
  # can't find a vector based expectation in testthat
  expect_lt(new_df$log_price[1],
                    ppr2$price[1])}
          )

data(ppr)
test_that('we have is_full_market_price column', {
          ppr3 <- normalise_names(ppr) %>%
            dplyr::mutate(price=fix_price(price)) %>%
            log_column(price)
          ppr4 <- invert_field(ppr3, 
                               not_full_market_price)
          expect_equal(names(ppr4)[length(ppr4)],
                       "is_full_market_price") }
          )

## test_that('new property desc logic is the same as old',{
##           data(ppr)
##           ppr_input  <- normalise_names(ppr) %>%
##             dplyr::mutate(price=fix_price(price)) %>%
##             mark_values_as_large(1e6) %>%
##             log_column(price) %>% 
##             invert_field(not_full_market_price)
##           ppr_old  <-
##             readr::read_csv("~/Dropbox/Code/Rlang/refactoring_and_tdd/ppr/inst/ppr_data_cleaning_done.csv") %>%
##             dplyr::mutate(property_size_description=as.character(property_size_description))
##           expect_equal(fix_property_description(ppr_input),
##                        ppr_old)})

data(ppr)
ppr_for_split  <- normalise_names(ppr) %>%
  dplyr::mutate(price=fix_price(price)) %>%
  mark_values_as_large(1e6) %>%
  log_column(price) %>% 
  invert_field(not_full_market_price) %>%
  fix_property_description()
test_that('split_data returns a list',
          expect_is(split_data(ppr_for_split), 'list'))

test_that('split data has test and train',
          expect_equal(names(
            split_data(ppr_for_split)), 
                       c("train", "test")))

test_that('split data returns a train tibble',
          expect_is(
            split_data(ppr_for_split)$train[1],
            'tbl_df'))

test_that('split data returns a test tibble',
          expect_is(
            split_data(ppr_for_split)$test[1],
            'tbl_df'))

test_that('split_data train has less rows than input',{
          train  <- split_data(ppr_for_split)$train
          expect_gt(
            nrow(ppr_for_split),
            nrow(train))}
          )
