test_that("mode imputation fills categorical columns", {
  df <- readRDS(testthat::test_path("fixtures/bank_test_data.rds"))

  # any column (numeric/char/factor) with missing-like values is eligible
  needs <- vapply(df, imputeflow:::needs_imputation, logical(1))
  targets <- names(df)[needs]

  spec <- imputeflow::fit_mode_spec(df, cols = targets)
  out  <- imputeflow::apply_mode_spec(df, spec)

  for (nm in targets) {
    pre  <- imputeflow:::missing_mask(df[[nm]])
    post <- imputeflow:::missing_mask(out[[nm]])
    expect_true(any(pre))
    expect_false(any(post[pre], na.rm = TRUE))
  }

  # spot-check: factors remain factors
  fct_cols <- names(df)[vapply(df, is.factor, logical(1))]
  for (nm in intersect(fct_cols, targets)) {
    expect_true(is.factor(out[[nm]]))
  }
})