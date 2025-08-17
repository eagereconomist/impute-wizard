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

read_stdin_csv <- function(verbose = FALSE, spinner = NULL) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required for CLI IO. Install it first.", call. = FALSE)
  }
  if (isTRUE(verbose)) .progress_bar_safe("Reading CSV from stdin …", total = 1)
  df <- readr::read_csv(file("stdin"), show_col_types = FALSE)
  if (isTRUE(verbose)) .progress_update_safe(1)
  df
}

write_stdout_csv <- function(df, verbose = FALSE, spinner = NULL) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required for CLI IO. Install it first.", call. = FALSE)
  }
  if (isTRUE(verbose)) .progress_bar_safe("Writing CSV to stdout …", total = 1)
  readr::write_csv(df, stdout())
  if (isTRUE(verbose)) .progress_update_safe(1)
  invisible(df)
}

apply_imputation <- function(df, method, cols = NULL, opts = list(), verbose = FALSE, spinner = NULL) {
  stopifnot(is.data.frame(df))
  method <- tolower(method)
  start_time <- Sys.time()
  `%||%` <- function(x, y) if (is.null(x) || (is.character(x) && !nzchar(x))) y else x
  replace_selected <- function(orig, imputed, cols) {
    orig[, cols] <- imputed[, cols, drop = FALSE]
    orig
  }
  if (isTRUE(verbose)) .progress_bar_safe(sprintf("Imputing [%s]", method), total = NA)
  df_before <- df
  res <- NULL
  if (method %in% c("mean", "median", "mode")) {
    if (!is.null(cols)) {
      miss <- setdiff(cols, colnames(df))
      if (length(miss)) cli::cli_abort(c("x Column(s) not found:", paste0("* ", miss)))
      sub <- df[, cols, drop = FALSE]
      sub_imp <- switch(
        method,
        mean   = impute_mean(sub, train_frac = opts$train_frac %||% 1, seed = opts$seed %||% 1),
        median = impute_median(sub, train_frac = opts$train_frac %||% 1, seed = opts$seed %||% 1),
        mode   = impute_mode(sub, train_frac = opts$train_frac %||% 1, seed = opts$seed %||% 1)
      )
      res <- replace_selected(df, sub_imp, cols)
    } else {
      res <- switch(
        method,
        mean   = impute_mean(df,    train_frac = opts$train_frac %||% 1, seed = opts$seed %||% 1),
        median = impute_median(df,  train_frac = opts$train_frac %||% 1, seed = opts$seed %||% 1),
        mode   = impute_mode(df,    train_frac = opts$train_frac %||% 1, seed = opts$seed %||% 1)
      )
    }
  } else if (method == "knn") {
    res <- impute_knn_recipes(df,
                              cols = cols,
                              neighbors = opts$neighbors %||% 5L,
                              train_frac = opts$train_frac %||% 1,
                              seed = opts$seed %||% 1)
  } else if (method == "drf") {
    eh <- try(getFromNamespace("ensure_h2o", "imputeflow"), silent = TRUE)
    if (inherits(eh, "try-error")) {
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
      ntrees    = opts$ntrees %||% 50L,
      max_depth = opts$max_depth %||% 10L,
      min_rows  = if (!is.null(opts$min_rows) && !is.na(opts$min_rows)) opts$min_rows else 5L,
      sample_rate = if (!is.null(opts$sample_rate) && !is.na(opts$sample_rate)) opts$sample_rate else 0.8,
      col_sample_rate_per_tree = if (!is.null(opts$col_sample_rate_per_tree) && !is.na(opts$col_sample_rate_per_tree)) opts$col_sample_rate_per_tree else 0.8,
      balance_classes = if (!is.null(opts$balance_classes)) opts$balance_classes else FALSE,
      seed      = opts$seed %||% 1L,
      round_digits = opts$round_digits %||% NULL,
      cols      = cols,
      verbose   = isTRUE(verbose)
    )
    target_cols <- if (!is.null(cols)) cols else names(df)
    pre_na_sum  <- sum(vapply(df[, target_cols, drop = FALSE], function(x) sum(is.na(x)), integer(1)))
    post_na_sum <- sum(vapply(imputed[, target_cols, drop = FALSE], function(x) sum(is.na(x)), integer(1)))
    fb <- tolower(opts$fallback %||% "none")
    if (fb %in% c("mean","median","mode") && post_na_sum >= pre_na_sum) {
      if (!is.null(cols)) {
        sub <- df[, cols, drop = FALSE]
        sub_imp <- switch(fb,
          mean   = impute_mean(sub,    train_frac = opts$train_frac %||% 1, seed = opts$seed %||% 1),
          median = impute_median(sub,  train_frac = opts$train_frac %||% 1, seed = opts$seed %||% 1),
          mode   = impute_mode(sub,    train_frac = opts$train_frac %||% 1, seed = opts$seed %||% 1)
        )
        imputed <- replace_selected(df, sub_imp, cols)
      } else {
        imputed <- switch(fb,
          mean   = impute_mean(df,    train_frac = opts$train_frac %||% 1, seed = opts$seed %||% 1),
          median = impute_median(df,  train_frac = opts$train_frac %||% 1, seed = opts$seed %||% 1),
          mode   = impute_mode(df,    train_frac = opts$train_frac %||% 1, seed = opts$seed %||% 1)
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
  if (isTRUE(verbose)) {
    elapsed <- as.numeric(Sys.time() - start_time, units = "secs")
    cli::cli_alert_success(sprintf("Completed '%s' imputation in %.2f seconds.", method, elapsed))
  }
  if (!is.null(opts$round_digits) && !is.na(opts$round_digits)) {
    res <- imputeflow:::round_imputed_numerics(df_before, res, digits = opts$round_digits)
  }
  res
}