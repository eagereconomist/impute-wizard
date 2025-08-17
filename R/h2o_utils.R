#' @param expr An expression to evaluate quietly.
#' @return The value of the expression.
quiet_run <- function(expr) {
  expr <- substitute(expr)
  out_path <- tempfile()
  msg_path <- tempfile()
  out_con  <- file(out_path, open = "wt")
  msg_con  <- file(msg_path, open = "wt")
  old_out <- sink.number()
  old_msg <- sink.number(type = "message")
  on.exit({
    while (sink.number(type = "message") > old_msg) {
      try(sink(type = "message"), silent = TRUE)
    }
    while (sink.number() > old_out) {
      try(sink(), silent = TRUE)
    }
    try(close(msg_con), silent = TRUE)
    try(close(out_con), silent = TRUE)
    unlink(c(out_path, msg_path))
  }, add = TRUE)
  sink(out_con)
  sink(msg_con, type = "message")
  res <- suppressWarnings(suppressMessages(eval(expr, parent.frame())))
  res
}

#' @title Ensure H2O Cluster
#' @name ensure_h2o
#' @description \n
# Ensure that an H2O connection is running.  If a connection already exists,
# it is returned.  Otherwise a new H2O cluster is initialised using the
# provided parameters.  The `quiet` flag controls whether progress
# messages from H2O startup are displayed.  This helper is used by
# higher-level functions to manage the H2O lifecycle.
#' @param nthreads Number of threads to use (default -1, all available)
#' @param max_mem_size Maximum memory size for the H2O cluster (e.g. "32G")
#' @param port Port on which to launch the H2O cluster ("auto" to auto-select)
#' @param quiet Whether to suppress startup messages
#' @return An invisible H2O connection object or TRUE if already connected.
#' @export
ensure_h2o <- function(nthreads = -1L,
                       max_mem_size = NULL,
                       port = "auto",
                       quiet = TRUE) {
  if (!requireNamespace("h2o", quietly = TRUE)) {
    stop("Package 'h2o' is required. Install it first.", call. = FALSE)
  }
  existing <- try(h2o::h2o.getConnection(), silent = TRUE)
  # If a connection already exists, reuse it.
  if (inherits(existing, "H2OConnection")) {
    return(invisible(existing))
  }
  base_args <- list(nthreads = nthreads, strict_version_check = FALSE)
  if (!is.null(max_mem_size) && !identical(max_mem_size, "")) base_args$max_mem_size <- max_mem_size
  ports <- if (identical(port, "auto")) c(54321L, 54322L, 54323L) else as.integer(port)
  last_err <- NULL
  for (p in ports) {
    ok <- TRUE
    args <- base_args
    args$port <- p
    tryCatch({
      if (quiet) {
        invisible(quiet_run(do.call(h2o::h2o.init, args)))
      } else {
        do.call(h2o::h2o.init, args)
      }
    }, error = function(e) {
      ok <<- FALSE
      last_err <<- e
    })
    if (ok) {
      return(invisible(h2o::h2o.getConnection()))
    }
  }
  stop(sprintf("Failed to start H2O (last error: %s)",
               if (is.null(last_err)) "unknown" else conditionMessage(last_err)))
}