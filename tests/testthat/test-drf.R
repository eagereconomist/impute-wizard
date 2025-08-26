test_that("impute_h2o_drf reduces missingness", {
  skip_if_not_installed("h2o")
  data_path <- testthat::test_path("fixtures", "test_data.rds")
  skip_if_not(file.exists(data_path), "Test fixture 'test_data.rds' not found")
  df <- readRDS(data_path)
  before_na <- sum(is.na(df))
  out <- imputeflow::impute_h2o_drf(
    df,
    ntrees = 10,
    max_depth = 5,
    seed = 1,
    verbose = FALSE
  )
  after_na <- sum(is.na(out))
  expect_true(after_na < before_na)
})