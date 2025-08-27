if (requireNamespace("h2o", quietly = TRUE)) {
  tryCatch(h2o::h2o.shutdown(prompt = FALSE), error = function(e) NULL)
}