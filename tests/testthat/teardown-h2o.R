# Run after the whole test suite finishes
try(h2o::h2o.shutdown(prompt = FALSE), silent = TRUE)
