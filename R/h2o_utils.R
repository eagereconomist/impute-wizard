#' Evaluate an expression with stdout/stderr silenced
#'
#' Captures console output and messages while evaluating an expression, then
#' restores sinks and returns the expression result.
#' @param expr An expression to evaluate.
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
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

#' Ensure an H2O cluster is running
#'
#' Returns an existing H2O connection if available, otherwise starts a new H2O
#' cluster using the provided settings. Tries a small set of ports when
#' `port = "auto"`.
#' @param nthreads Integer; number of threads (-1 for all).
#' @param max_mem_size Character like "32G" for max memory.
#' @param port Integer port or "auto".
#' @param quiet Logical; suppress startup messages.
#' @return An H2O connection object (invisibly).
#' @export
#' @seealso h2o::h2o.init
ensure_h2o <- function(nthreads = -1L,
                       max_mem_size = NULL,
                       port = "auto",
                       quiet = TRUE) {
  if (!requireNamespace("h2o", quietly = TRUE)) {
    stop("Package 'h2o' is required. Install it first.", call. = FALSE)
  }
  existing <- try(h2o::h2o.getConnection(), silent = TRUE)
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