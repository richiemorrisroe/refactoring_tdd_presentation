require(ppr)
require(testthat)
count_distincts_old <- load_test_data("count_distincts_test_data.rds")
test_that('count_distinct old and new are equal',
          expect_equal(count_distinct_values(ppr_pobal),
                       count_distincts_old))

maxmin_old <- load_test_data("get_max_and_min_test_data.csv")
test_that('get_max_and_min old and new are equal',
          expect_equal(get_max_and_min(ppr_pobal),
                       maxmin_old))

missings_old <- load_test_data("count_prop_missings_test_data.csv")
test_that('count_prop_missings old and new are equal',
          expect_equal(count_proportion_missing(ppr_pobal),
                       missings_old))
