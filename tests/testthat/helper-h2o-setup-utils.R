set.seed(4572)

skip_if_no_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(paste0("Package '", pkg, "' not installed"))
  }
}


skip_if_no_h2o_runtime <- function() {
  skip_if_no_pkg("h2o")
  ok <- TRUE
  err <- NULL
  tryCatch({
    imputeflow:::ensure_h2o(nthreads = 1, port = "auto", quiet = TRUE)
    suppressWarnings(h2o::h2o.clusterInfo())
  }, error = function(e) { ok <<- FALSE; err <<- conditionMessage(e) })
  if (!ok) testthat::skip(paste0("H2O runtime not available: ", err %||% "unknown error"))
}
`%||%` <- function(x, y) if (is.null(x)) y else x



toy_frame <- function() {
  data.frame(
    id = sprintf("row_%02d", 1:60),
    x_num1 = c(rnorm(50), rep(NA, 10)),
    x_num2 = rnorm(60),
    x_cat  = factor(sample(c("a", "b", "c"), 60, TRUE)),
    y_num  = c(rnorm(55, 10, 2), rep(NA, 5)),
    y_cat  = factor(c(sample(c("yes", "no"), 56, TRUE), rep(NA, 4))),
    stringsAsFactors = TRUE
  )
}
