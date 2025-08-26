test_that("kNN imputation with exclude_predictors works", {
  testthat::skip_if_not_installed("recipes")
  testthat::skip_if_not_installed("tibble")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyselect")
  testthat::skip_if_not_installed("rlang")

  df <- readRDS(testthat::test_path("fixtures/bank_test_data.rds"))

  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  needs <- vapply(df[num_cols], function(x) any(imputeflow:::missing_mask(x)), logical(1))
  targets <- num_cols[needs]

  # pretend these are ID-like and should be excluded if present
  exclude <- intersect(names(df), c("id", "account_id", "uuid"))

  spec <- imputeflow::fit_knn_spec(
    df,
    cols = targets,
    neighbors = 5,
    train_rows = NULL,
    exclude_predictors = exclude
  )

  # excluded predictors should not appear in predictors set
  expect_false(length(intersect(exclude, spec$predictors)) > 0)

  out <- imputeflow::apply_knn_spec(df, spec, round_digits = NA)

  for (nm in targets) {
    pre  <- imputeflow:::missing_mask(df[[nm]])
    post <- imputeflow:::missing_mask(out[[nm]])
    expect_true(any(pre))
    expect_false(any(post[pre], na.rm = TRUE))
  }
})