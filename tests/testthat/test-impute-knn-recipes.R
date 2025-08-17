test_that("impute_knn_recipes reduces missingness", {
  skip_if_not_installed("recipes")
  data_path <- testthat::test_path("fixtures", "test_data.rds")
  skip_if_not(file.exists(data_path), "Test fixture 'test_data.rds' not found")
  df <- readRDS(data_path)
  before_na <- sum(is.na(df))
  out <- imputeflow::impute_knn_recipes(df,
    neighbors = 3,
    train_frac = 0.8,
    seed = 1
  )
  after_na <- sum(is.na(out))
  expect_true(after_na < before_na)
})