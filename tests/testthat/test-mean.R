test_that("mean imputation works on bank_fixture", {
  df <- readRDS(testthat::test_path("fixtures/bank_test_data.rds"))

  # targets = numeric cols with missingness (per package’s rules)
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  needs <- vapply(df[num_cols], imputeflow:::needs_imputation, logical(1))
  targets <- num_cols[needs]

  spec <- imputeflow::fit_mean_spec(df, cols = targets)
  out  <- imputeflow::apply_mean_spec(df, spec, round_digits = 2)

  # all former-missing cells in targets should be filled
  for (nm in targets) {
    pre  <- imputeflow:::missing_mask(df[[nm]])
    post <- imputeflow:::missing_mask(out[[nm]])
    expect_true(any(pre))
    expect_false(any(post[pre], na.rm = TRUE))
  }
})