# Internal CLI helpers for imputeflow
# No exports on purpose.
#' @keywords internal
#' @noRd
read_stdin_csv <- function(verbose = FALSE) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required for CLI IO. Install it first.", call. = FALSE)
  }
  if (verbose) cli::cli_alert_info("Reading CSV from stdin …")
  readr::read_csv(file("stdin"), show_col_types = FALSE)
}

#' @keywords internal
#' @noRd
write_stdout_csv <- function(df, verbose = FALSE) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required for CLI IO. Install it first.", call. = FALSE)
  }
  if (verbose) cli::cli_alert_info("Writing CSV to stdout …")
  readr::write_csv(df, stdout())
}

# start or reuse H2O with user-specified resources
#' @keywords internal
#' @noRd
ensure_h2o <- function(nthreads = -1, max_mem_size = NULL, port = "auto", quiet = TRUE) {
  if (!requireNamespace("h2o", quietly = TRUE)) {
    stop("Package 'h2o' is required for DRF imputation. Install it first.", call. = FALSE)
  }

  # Reuse an existing connection if present
  existing <- try(h2o::h2o.getConnection(), silent = TRUE)
  if (inherits(existing, "H2OConnection")) {
    if (!quiet) {
      ip <- tryCatch(existing@ip, error = function(...) "localhost")
      pr <- tryCatch(existing@port, error = function(...) NA_integer_)
      cli::cli_alert_info(sprintf("Reusing existing H2O connection at %s:%s.", ip, pr))
    }
    return(invisible(TRUE))
  }

  # Choose candidate ports
  ports <-
    if (identical(port, "auto")) {
      unique(c(54321L, 54322L, 54323L, sample(55000:59999, 6)))
    } else if (is.null(port) || identical(port, "default")) {
      54321L
    } else {
      as.integer(port)
    }

  last_err <- NULL
  for (p in ports) {
    ok <- TRUE
    tryCatch({
      h2o::h2o.init(
        nthreads = nthreads,
        strict_version_check = FALSE,
        startH2O = TRUE,
        port = p,
        max_mem_size = max_mem_size
      )
    }, error = function(e) {
      ok <<- FALSE
      last_err <<- conditionMessage(e)
    })
    if (ok) {
      if (!quiet) {
        info <- h2o::h2o.clusterInfo()
        cli::cli_alert_success(sprintf("H2O started (v%s) on port %s.", info$version, info$cloud_port))
      }
      return(invisible(TRUE))
    }
  }

  stop(sprintf("Failed to start H2O on ports %s. Last error: %s",
               paste(ports, collapse = ", "),
               if (is.null(last_err)) "unknown" else last_err),
       call. = FALSE)
}
# small infix default
`%||%` <- function(x, y) if (is.null(x) || (is.character(x) && !nzchar(x))) y else x

# central dispatcher used by the CLI
#' @keywords internal
#' @noRd
apply_imputation <- function(df, method, cols = NULL, opts = list(), verbose = FALSE) {
  stopifnot(is.data.frame(df))
  method <- tolower(method)

  # helper to replace only selected columns when requested
  replace_selected <- function(orig, imputed, cols) {
    orig[, cols] <- imputed[, cols, drop = FALSE]
    orig
  }

  if (method %in% c("mean", "median", "mode")) {
    if (!is.null(cols)) {
      miss <- setdiff(cols, colnames(df))
      if (length(miss)) cli::cli_abort(c("x Column(s) not found:", paste0("* ", miss)))
      sub <- df[, cols, drop = FALSE]
      sub_imp <- switch(
        method,
        mean   = impute_mean(sub),
        median = impute_median(sub),
        mode   = impute_mode(sub)
      )
      return(replace_selected(df, sub_imp, cols))
    }
    return(
      switch(
        method,
        mean   = impute_mean(df),
        median = impute_median(df),
        mode   = impute_mode(df)
      )
    )
  }

  if (method == "knn") {
    neighbors <- opts$neighbors %||% 5L
    id_cols   <- opts$id_cols %||% NULL
    # impute_knn_recipes accepts character vectors for cols/id_cols
    return(impute_knn_recipes(df, cols = cols, neighbors = neighbors, id_cols = id_cols))
  }

  if (method == "drf") {
    # spin up H2O with requested resources; impute_h2o_drf will reuse it
    ensure_h2o(
      nthreads     = opts$h2o_threads %||% -1L,
      max_mem_size = opts$h2o_mem %||% NULL,
      quiet        = !isTRUE(verbose)
    )
    imputed <- impute_h2o_drf(
      df,
      ntrees    = opts$ntrees %||% 200L,
      max_depth = opts$max_depth %||% 20L,
      seed      = opts$seed %||% 1L,
      verbose   = isTRUE(verbose)
    )
    # honor --cols by only replacing those columns in the original
    if (!is.null(cols)) {
      miss <- setdiff(cols, colnames(df))
      if (length(miss)) cli::cli_abort(c("x Column(s) not found:", paste0("* ", miss)))
      return(replace_selected(df, imputed, cols))
    }
    return(imputed)
  }

  cli::cli_abort("Unknown method: {method}")
}
