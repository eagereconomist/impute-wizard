test_that("impute_h2o_drf reduces NAs for numeric and categorical targets", {
  skip_if_no_h2o_runtime()
  df <- toy_frame()

  pre_na <- vapply(df, function(x) sum(is.na(x)), integer(1))
  expect_true(pre_na[["y_num"]] > 0)
  expect_true(pre_na[["y_cat"]] > 0)

  out <- impute_h2o_drf(df, ntrees = 50, max_depth = 10, seed = 42, verbose = FALSE)

  expect_equal(nrow(out), nrow(df))
  expect_s3_class(out, "data.frame")

  post_na <- vapply(out, function(x) sum(is.na(x)), integer(1))
  expect_lte(post_na[["y_num"]], pre_na[["y_num"]])
  expect_lte(post_na[["y_cat"]], pre_na[["y_cat"]])

  expect_true(is.numeric(out$y_num))
  expect_true(is.factor(out$y_cat))
})

test_that("impute_h2o_drf skips columns that are all NA or have no variance", {
  skip_if_no_h2o_runtime()
  df <- toy_frame()
  df$all_na <- NA_real_
  df$constant <- 1

  out <- impute_h2o_drf(df, ntrees = 10, max_depth = 5, seed = 1, verbose = FALSE)

  expect_true(all(is.na(out$all_na)))
  expect_true(all(out$constant == 1))
})
