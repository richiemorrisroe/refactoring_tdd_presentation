context("test saving and loading")
data(ppr)
ppr_test_load_save  <- ppr
save_test_data(ppr_test_load_save)
expect_equal(ppr, load_test_data("ppr_test_load_save_test_data.rds"))
