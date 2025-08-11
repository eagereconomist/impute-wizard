test_that("impute_knn_recipes imputes only NA columns and preserves classes", {
  skip_if_no_pkg("recipes")
  df <- toy_frame()

  pre_na <- vapply(df, function(x) sum(is.na(x)), integer(1))
  expect_true(pre_na[["x_num1"]] > 0)
  expect_true(pre_na[["y_num"]]  > 0)
  expect_true(pre_na[["y_cat"]]  > 0)

  # Use character selectors to avoid tidyselect deprecation warnings
  out <- impute_knn_recipes(df, neighbors = 3, id_cols = "id")

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(df))

  post_na <- vapply(out, function(x) sum(is.na(x)), integer(1))
  expect_lt(post_na[["x_num1"]], pre_na[["x_num1"]])
  expect_lt(post_na[["y_num"]],  pre_na[["y_num"]])
  expect_lt(post_na[["y_cat"]],  pre_na[["y_cat"]])

  expect_type(out$x_num1, "double")
  expect_s3_class(out$x_cat, "factor")
  expect_s3_class(out$y_cat, "factor")
})

test_that("impute_knn_recipes works when selecting explicit columns", {
  skip_if_no_pkg("recipes")
  df <- toy_frame()
  out <- impute_knn_recipes(df, cols = c("x_num1","y_num"), neighbors = 5)

  pre_na <- vapply(df,  function(x) sum(is.na(x)), integer(1))
  post_na <- vapply(out, function(x) sum(is.na(x)), integer(1))

  expect_lt(post_na[["x_num1"]], pre_na[["x_num1"]])
  expect_lt(post_na[["y_num"]],  pre_na[["y_num"]])
})
