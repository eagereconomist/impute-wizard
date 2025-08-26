test_that("median imputation with train_frac works on bank_fixture", {
  set.seed(123)
  df <- readRDS(testthat::test_path("fixtures/bank_test_data.rds"))

  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  needs <- vapply(df[num_cols], imputeflow:::needs_imputation, logical(1))
  targets <- num_cols[needs]

  # emulate --train-frac (e.g., 0.7) via train_rows argument
  n <- nrow(df); train_frac <- 0.7
  train_rows <- sample(seq_len(n), size = max(1L, floor(n * train_frac)))

  spec <- imputeflow::fit_median_spec(df, cols = targets, train_rows = train_rows)
  out  <- imputeflow::apply_median_spec(df, spec, round_digits = NA)

  for (nm in targets) {
    pre  <- imputeflow:::missing_mask(df[[nm]])
    post <- imputeflow:::missing_mask(out[[nm]])
    expect_true(any(pre))
    expect_false(any(post[pre], na.rm = TRUE))
  }
})