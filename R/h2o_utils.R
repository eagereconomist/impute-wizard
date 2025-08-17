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
    ns <- try(asNamespace("imputeflow"), silent = TRUE)

    if (!inherits(ns, "try-error") && exists("ensure_h2o", envir = ns, inherits = FALSE)) {
      getFromNamespace("ensure_h2o", "imputeflow")(nthreads = 1, port = "auto", quiet = TRUE)
    } else {
      # Fallback: start H2O directly (try a few ports)
      for (p in c(54321L, 54322L, 54323L)) {
        okp <- TRUE
        tryCatch({
          h2o::h2o.init(nthreads = 1, strict_version_check = FALSE, startH2O = TRUE, port = p)
        }, error = function(e) okp <<- FALSE)
        if (okp) break
      }
    }

    suppressWarnings(h2o::h2o.clusterInfo())
  }, error = function(e) { ok <<- FALSE; err <<- conditionMessage(e) })

  if (!ok) testthat::skip(paste0("H2O runtime not available: ", if (is.null(err)) "unknown" else err))
}


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
