test_that("DRF imputation with mask and exclusion works", {
  testthat::skip_if_not_installed("h2o")
  # H2O can be flaky on CRAN/CI; skip on CRAN to be safe.
  testthat::skip_on_cran()

  set.seed(42)
  df <- readRDS(testthat::test_path("fixtures/bank_test_data.rds"))

  # choose eligible columns by package rules
  needs <- vapply(df, imputeflow:::needs_imputation, logical(1))
  targets <- names(df)[needs]

  # light params for speed; exclude any obvious id-like cols if present
  exclude <- intersect(names(df), c("id", "account_id", "uuid"))

  n <- nrow(df); train_rows <- sample(seq_len(n), size = max(5L, floor(0.7 * n)))

  out <- imputeflow::impute_h2o_drf_fit_apply(
    df,
    cols = targets,
    exclude_predictors = exclude,
    train_rows = train_rows,
    ntrees = 25,
    max_depth = 8,
    min_rows = 5,
    sample_rate = 0.8,
    col_sample_rate_per_tree = 0.8,
    balance_classes = FALSE,
    seed = 1,
    round_digits = NA,
    nfolds = 0,
    verbose = TRUE
  )

  # DRF should not increase missingness; for most targets it should remove it.
  for (nm in targets) {
    pre  <- mean(imputeflow:::missing_mask(df[[nm]]))
    post <- mean(imputeflow:::missing_mask(out[[nm]]))
    expect_lte(post, pre + 1e-12)
  }
})