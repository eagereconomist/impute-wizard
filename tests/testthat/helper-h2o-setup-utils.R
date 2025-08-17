if (requireNamespace("h2o", quietly = TRUE)) {
  tryCatch(imputeflow::ensure_h2o(), error = function(e) NULL)
}