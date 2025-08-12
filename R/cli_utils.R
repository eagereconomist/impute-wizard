# R/cli_utils.R  (compat with older cli: no 'spinner' arg)


.progress_bar_safe <- function(title, total = 1, spinner_name = "dots") {
  if (!requireNamespace("cli", quietly = TRUE)) return(invisible(NULL))
  has_spinner <- tryCatch("spinner" %in% names(formals(cli::cli_progress_bar)),
                          error = function(e) FALSE)
  if (has_spinner) {
    cli::cli_progress_bar(title, total = total, spinner = cli::get_spinner(spinner_name))
  } else {
    cli::cli_progress_bar(title, total = total)
  }
  invisible(NULL)
}


.progress_update_safe <- function(set = 1) {
  if (requireNamespace("cli", quietly = TRUE)) {
    try(cli::cli_progress_update(set = set), silent = TRUE)
  }
  invisible(NULL)
}


#' @keywords internal
#' @noRd
read_stdin_csv <- function(verbose = FALSE, spinner = NULL) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required for CLI IO. Install it first.", call. = FALSE)
  }
  if (isTRUE(verbose)) .progress_bar_safe("Reading CSV from stdin …", total = 1)
  df <- readr::read_csv(file("stdin"), show_col_types = FALSE)
  if (isTRUE(verbose)) .progress_update_safe(1)
  df
}


#' @keywords internal
#' @noRd
write_stdout_csv <- function(df, verbose = FALSE, spinner = NULL) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required for CLI IO. Install it first.", call. = FALSE)
  }
  if (isTRUE(verbose)) .progress_bar_safe("Writing CSV to stdout …", total = 1)
  readr::write_csv(df, stdout())
  if (isTRUE(verbose)) .progress_update_safe(1)
  invisible(df)
}


#' @keywords internal
#' @noRd
apply_imputation <- function(df, method, cols = NULL, opts = list(), verbose = FALSE, spinner = NULL) {
  stopifnot(is.data.frame(df))
  method <- tolower(method)

  `%||%` <- function(x, y) if (is.null(x) || (is.character(x) && !nzchar(x))) y else x
  replace_selected <- function(orig, imputed, cols) {
    orig[, cols] <- imputed[, cols, drop = FALSE]
    orig
  }

  if (isTRUE(verbose)) .progress_bar_safe(sprintf("Imputing [%s]", method), total = 1)

  res <- NULL

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
      res <- replace_selected(df, sub_imp, cols)
    } else {
      res <- switch(
        method,
        mean   = impute_mean(df),
        median = impute_median(df),
        mode   = impute_mode(df)
      )
    }

  } else if (method == "knn") {
    neighbors <- opts$neighbors %||% 5L
    id_cols   <- opts$id_cols   %||% NULL
    res <- impute_knn_recipes(df, cols = cols, neighbors = neighbors, id_cols = id_cols)

  } else if (method == "drf") {
    # Resolve ensure_h2o from our namespace for reliability
    eh <- try(getFromNamespace("ensure_h2o", "imputeflow"), silent = TRUE)
    if (inherits(eh, "try-error")) {
      # Fallback: start H2O directly (try a few ports)
      if (!requireNamespace("h2o", quietly = TRUE)) {
        cli::cli_abort("Method 'drf' requires the 'h2o' package.")
      }
      for (p in c(54321L, 54322L, 54323L)) {
        okp <- TRUE
        tryCatch({
          h2o::h2o.init(
            nthreads = opts$h2o_threads %||% -1L,
            strict_version_check = FALSE,
            startH2O = TRUE,
            port = p,
            max_mem_size = opts$h2o_mem %||% NULL
          )
        }, error = function(e) okp <<- FALSE)
        if (okp) break
      }
    } else {
      eh(
        nthreads     = opts$h2o_threads %||% -1L,
        max_mem_size = opts$h2o_mem %||% NULL,
        port         = opts$h2o_port %||% "auto",
        quiet        = !isTRUE(verbose)
      )
    }

    imputed <- impute_h2o_drf(
      df,
      ntrees    = opts$ntrees %||% 200L,
      max_depth = opts$max_depth %||% 20L,
      seed      = opts$seed %||% 1L,
      verbose   = isTRUE(verbose)
    )

    # Optional DRF fallback (only inside DRF branch)
    target_cols <- if (!is.null(cols)) cols else names(df)
    pre_na_sum  <- sum(vapply(df[,      target_cols, drop = FALSE], function(x) sum(is.na(x)), integer(1)))
    post_na_sum <- sum(vapply(imputed[, target_cols, drop = FALSE], function(x) sum(is.na(x)), integer(1)))

    fb <- tolower(opts$fallback %||% "none")
    if (fb %in% c("mean","median","mode") && post_na_sum >= pre_na_sum) {
      if (!is.null(cols)) {
        sub <- df[, cols, drop = FALSE]
        sub_imp <- switch(fb,
          mean   = impute_mean(sub),
          median = impute_median(sub),
          mode   = impute_mode(sub)
        )
        imputed <- replace_selected(df, sub_imp, cols)
      } else {
        imputed <- switch(fb,
          mean   = impute_mean(df),
          median = impute_median(df),
          mode   = impute_mode(df)
        )
      }
      if (isTRUE(verbose)) cli::cli_alert_info("DRF could not reduce NAs; applied {fb} fallback.")
    }

    if (!is.null(cols)) {
      miss <- setdiff(cols, colnames(df))
      if (length(miss)) cli::cli_abort(c("x Column(s) not found:", paste0("* ", miss)))
      res <- replace_selected(df, imputed, cols)
    } else {
      res <- imputed
    }

  } else {
    cli::cli_abort("Unknown method: {method}")
  }

  if (isTRUE(verbose)) .progress_update_safe(1)
  res
}
