#' @param x A vector.
#' @return A logical vector indicating missingness.
missing_mask <- function(x) {
  na_mask <- is.na(x)
  if (is.character(x) || is.factor(x)) {
    chr <- as.character(x)
    na_mask <- na_mask | chr == "" | chr == "NA"
  }
  na_mask
}
.is_missing <- missing_mask
#' @param x A vector.
#' @return Logical scalar indicating the presence of missing values.
needs_imputation <- function(x) {
  any(missing_mask(x), na.rm = TRUE)
}

#' @param x A vector.
#' @return A vector with missing-like strings replaced by `NA`.
replace_missing_strings <- function(x) {
  if (is.character(x)) {
    x[x == "" | x == "NA"] <- NA
  } else if (is.factor(x)) {
    tmp <- as.character(x)
    tmp[tmp == "" | tmp == "NA"] <- NA
    x <- factor(tmp)
  }
  x
}

#' @param df_before The original data frame prior to imputation.
#' @param df_after  The data frame after imputation.
#' @param digits    Number of decimal places to round to.
#' @return A data frame with numeric imputations rounded.
round_imputed_numerics <- function(df_before, df_after, digits = NULL) {
  if (is.null(digits) || is.na(digits)) return(df_after)
  stopifnot(is.data.frame(df_before), is.data.frame(df_after))
  stopifnot(nrow(df_before) == nrow(df_after))
  stopifnot(identical(names(df_before), names(df_after)))
  out <- df_after
  for (nm in names(out)) {
    miss <- missing_mask(df_before[[nm]])
    if (is.numeric(out[[nm]]) && any(miss, na.rm = TRUE)) {
      out[[nm]][miss] <- round(out[[nm]][miss], digits = as.integer(digits))
      if (inherits(df_before[[nm]], "integer") && as.integer(digits) == 0) {
        ok <- !is.na(out[[nm]])
        out[[nm]][ok] <- as.integer(out[[nm]][ok])
      }
    }
  }
  out
}

#' @param df        A data frame.
#' @param train_frac Fraction of rows to use when computing the mean.
#' @param seed       RNG seed for reproducible sampling.
#' @return A data frame with missing numeric values replaced by the mean.
#' @export
impute_mean <- function(df, train_frac = 1, seed = 1) {
  if (!is.data.frame(df)) stop("`df` must be a data.frame", call. = FALSE)
  if (!is.numeric(train_frac) || train_frac <= 0 || train_frac > 1)
    stop("`train_frac` must be a number in (0,1].", call. = FALSE)
  n <- nrow(df)
  train_idx <- if (train_frac < 1) {
    set.seed(seed); sample(seq_len(n), floor(train_frac * n))
  } else seq_len(n)
  out <- df
  for (nm in names(out)) {
    col <- out[[nm]]
    if (is.numeric(col) && needs_imputation(col)) {
      stat <- mean(col[train_idx], na.rm = TRUE)
      if (!is.na(stat)) {
        mask <- missing_mask(col)
        col[mask] <- stat
      }
      out[[nm]] <- col
    }
  }
  out
}

#' @inheritParams impute_mean
#' @return A data frame with missing numeric values replaced by the median.
#' @export
impute_median <- function(df, train_frac = 1, seed = 1) {
  if (!is.data.frame(df)) stop("`df` must be a data.frame", call. = FALSE)
  if (!is.numeric(train_frac) || train_frac <= 0 || train_frac > 1)
    stop("`train_frac` must be a number in (0,1].", call. = FALSE)
  n <- nrow(df)
  train_idx <- if (train_frac < 1) {
    set.seed(seed); sample(seq_len(n), floor(train_frac * n))
  } else seq_len(n)
  out <- df
  for (nm in names(out)) {
    col <- out[[nm]]
    if (is.numeric(col) && needs_imputation(col)) {
      stat <- stats::median(col[train_idx], na.rm = TRUE)
      if (!is.na(stat)) {
        mask <- missing_mask(col)
        col[mask] <- stat
      }
      out[[nm]] <- col
    }
  }
  out
}

#' @inheritParams impute_mean
#' @return A data frame with missing values replaced by the mode.
#' @export
impute_mode <- function(df, train_frac = 1, seed = 1) {
  if (!is.data.frame(df)) stop("`df` must be a data.frame", call. = FALSE)
  if (!is.numeric(train_frac) || train_frac <= 0 || train_frac > 1)
    stop("`train_frac` must be a number in (0,1].", call. = FALSE)
  n <- nrow(df)
  train_idx <- if (train_frac < 1) {
    set.seed(seed); sample(seq_len(n), floor(train_frac * n))
  } else seq_len(n)
  mode_of <- function(x) {
    ux <- unique(x[!is.na(x)])
    if (!length(ux)) return(NA)
    ux[which.max(tabulate(match(x, ux)))]
  }
  out <- df
  for (nm in names(out)) {
    col <- out[[nm]]
    if (!needs_imputation(col)) next
    col_norm <- replace_missing_strings(col)
    train_vals <- col_norm[train_idx]
    m <- mode_of(train_vals[!missing_mask(train_vals)])
    if (!is.na(m)) {
      mask <- missing_mask(col_norm)
      col_norm[mask] <- m
    }
    if (is.factor(col)) out[[nm]] <- factor(col_norm) else
    if (is.character(col)) out[[nm]] <- as.character(col_norm) else
      out[[nm]] <- col_norm
  }
  out
}

#' @param df        A data frame.
#' @param cols      A tidyselect specification or character vector of
#' @param neighbors Number of neighbours to use for kNN (default 5).
#' @param train_frac Fraction of rows to use for training (default 1).
#' @param seed       RNG seed for sampling the training subset.
#' @return A data frame with kNN-based imputations applied.
#' @export
impute_knn_recipes <- function(df,
                               cols = NULL,
                               neighbors = 5,
                               train_frac = 1,
                               seed = 1) {
  if (!is.data.frame(df)) stop("`df` must be a data.frame", call. = FALSE)
  if (!requireNamespace("recipes", quietly = TRUE)) stop("Package 'recipes' is required. Install it first.", call. = FALSE)
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' is required. Install it first.", call. = FALSE)
  if (!requireNamespace("tidyselect", quietly = TRUE)) stop("Package 'tidyselect' is required. Install it first.", call. = FALSE)
  if (!is.numeric(train_frac) || train_frac <= 0 || train_frac > 1)
    stop("`train_frac` must be a number in (0,1].", call. = FALSE)
  n <- nrow(df)
  train_idx <- if (train_frac < 1) {
    set.seed(seed); sample(seq_len(n), floor(train_frac * n))
  } else seq_len(n)
  orig_classes <- vapply(df, function(x) class(x)[1], character(1))
  df2 <- df
  for (nm in names(df2)) {
    miss <- missing_mask(df2[[nm]])
    if (any(miss, na.rm = TRUE)) df2[[nm]][miss] <- NA
    if (is.character(df2[[nm]])) df2[[nm]] <- as.factor(df2[[nm]])
  }
  cols_q <- rlang::enquo(cols)
  if (rlang::quo_is_null(cols_q)) {
    candidate_names <- names(df2)
  } else {
    candidate_names <- character(0)
    sel <- try(tidyselect::eval_select(rlang::get_expr(cols_q), data = df2), silent = TRUE)
    if (!inherits(sel, "try-error") && length(sel) > 0) candidate_names <- names(sel)
    if (is.character(cols)) candidate_names <- unique(c(candidate_names, intersect(cols, names(df2))))
    candidate_names <- intersect(names(df2), candidate_names)
    if (!length(candidate_names)) return(df2)
  }
  na_cols <- candidate_names[vapply(df2[candidate_names], needs_imputation, logical(1))]
  if (!length(na_cols)) return(as.data.frame(df2, stringsAsFactors = FALSE))
  rec <- recipes::recipe(~ ., data = df2[train_idx, , drop = FALSE])
  rec <- rec |> recipes::step_impute_knn(
    tidyselect::all_of(na_cols),
    neighbors   = neighbors,
    impute_with = recipes::all_predictors()
  )
  prepped <- recipes::prep(rec, training = df2[train_idx, , drop = FALSE], retain = FALSE)
  baked_train <- recipes::bake(prepped, new_data = df2[train_idx, , drop = FALSE])
  baked_test  <- recipes::bake(prepped, new_data = df2[-train_idx, , drop = FALSE])
  out <- df2
  out[train_idx, names(df2)] <- baked_train
  out[-train_idx, names(df2)] <- baked_test
  out_df <- as.data.frame(out, stringsAsFactors = FALSE)
  for (nm in names(out_df)) {
    cls <- orig_classes[[nm]]
    if (cls %in% c("character")) {
      out_df[[nm]] <- as.character(out_df[[nm]])
    } else if (cls %in% c("integer")) {
      new_vals <- suppressWarnings(as.numeric(out_df[[nm]]))
      if (all(is.na(new_vals) | abs(new_vals - round(new_vals)) < .Machine$double.eps^0.5)) {
        out_df[[nm]] <- as.integer(round(new_vals))
      } else {
        out_df[[nm]] <- new_vals
      }
    }
  }
  out_df
}

#' @param df Data frame containing the data to impute
#' @param ntrees Number of trees in the random forest (default 50)
#' @param max_depth Maximum depth of each tree (default 10)
#' @param min_rows Minimum number of observations in a terminal node (default 5)
#' @param sample_rate Fraction of rows sampled per tree (default 0.8)
#' @param col_sample_rate_per_tree Fraction of columns sampled per tree (default 0.8)
#' @param balance_classes Logical; whether to balance classes (only relevant for classification)
#' @param seed Random seed for reproducibility (default 1)
#' @param round_digits Optional integer; number of decimal places to round numeric imputations
#' @param cols Optional character vector specifying which columns to impute (if NULL, all columns)
#' @param h2o_connection Optional; existing H2O connection (NULL to start/reuse a connection)
#' @param verbose Logical; print progress messages (default TRUE)
#' @return A data frame with imputed values
#' @export
impute_h2o_drf <- function(df,
                           ntrees = 50,
                           max_depth = 10,
                           min_rows = 5,
                           sample_rate = 0.8,
                           col_sample_rate_per_tree = 0.8,
                           balance_classes = FALSE,
                           seed = 1,
                           round_digits = NULL,
                           cols = NULL,
                           h2o_connection = NULL,
                           verbose = TRUE) {
  stopifnot(is.data.frame(df))
  if (!requireNamespace("h2o", quietly = TRUE)) {
    stop("Package 'h2o' is required. Install it first.", call. = FALSE)
  }

  qrun <- function(expr) {
    if (exists("quiet_run", envir = asNamespace("imputeflow"), inherits = FALSE)) {
      get("quiet_run", envir = asNamespace("imputeflow"))(substitute(expr))
    } else {
      eval.parent(substitute(expr))
    }
  }
  if (is.null(h2o_connection)) {
    quiet_flag <- !isTRUE(verbose)
    imputeflow::ensure_h2o(nthreads = 1L, quiet = quiet_flag)
    conn_chk <- tryCatch(h2o::h2o.getConnection(), error = function(e) NULL)
    if (is.null(conn_chk)) {
      if (quiet_flag) {
        suppressWarnings(suppressMessages(
          qrun(h2o::h2o.init(nthreads = 1L, strict_version_check = FALSE))
        ))
      } else {
        h2o::h2o.init(nthreads = 1L, strict_version_check = FALSE)
      }
    }
    conn_chk2 <- tryCatch(h2o::h2o.getConnection(), error = function(e) NULL)
    if (is.null(conn_chk2)) {
      if (quiet_flag) {
        qrun(h2o::h2o.init(nthreads = 1L, strict_version_check = FALSE))
      } else {
        h2o::h2o.init(nthreads = 1L, strict_version_check = FALSE)
      }
    }
  }
  classes <- vapply(df, function(x) class(x)[1], character(1))
  df2 <- df
  df2[] <- lapply(df2, replace_missing_strings)
  ch_cols <- names(df2)[vapply(df2, is.character, logical(1))]
  if (length(ch_cols)) {
    for (cc in ch_cols) df2[[cc]] <- as.factor(df2[[cc]])
  }
  colnames_df <- colnames(df2)
  if (!is.null(cols)) {
    candidate_names <- character(0)
    cols_q <- rlang::enquo(cols)
    if (!rlang::quo_is_null(cols_q)) {
      sel <- try(tidyselect::eval_select(rlang::get_expr(cols_q), data = df2), silent = TRUE)
      if (!inherits(sel, "try-error") && length(sel) > 0) {
        candidate_names <- names(sel)
      }
    }
    if (is.character(cols)) {
      candidate_names <- unique(c(candidate_names, intersect(cols, names(df2))))
    }
    candidate_names <- intersect(names(df2), candidate_names)
    na_cols <- candidate_names[vapply(df2[candidate_names], needs_imputation, logical(1))]
  } else {
    na_cols <- names(df2)[vapply(df2, needs_imputation, logical(1))]
  }

  if (!length(na_cols)) {
    if (isTRUE(verbose)) message("No missing values detected in selected columns.")
    return(df)
  }
  for (y in na_cols) {
    y_vec <- df2[[y]]
    if (all(missing_mask(y_vec))) {
      if (isTRUE(verbose)) message(sprintf("Skipping '%s' (all values are missing).", y))
      next
    }
    if (length(unique(stats::na.omit(y_vec))) < 2) {
      if (isTRUE(verbose)) message(sprintf("Skipping '%s' (insufficient class/variance).", y))
      next
    }
    x <- setdiff(colnames_df, y)
    obs <- which(!missing_mask(y_vec))
    if (!length(obs)) next
    train_df <- df2[obs, , drop = FALSE]
    train_hf <- NULL
    train_conv <- utils::capture.output({
      train_hf <- h2o::as.h2o(train_df)
    }, type = "output")
    if (length(train_conv) > 0) {
      for (ln in train_conv) message(ln)
    }
    params <- list(
      x = x,
      y = y,
      training_frame = train_hf,
      ntrees = ntrees,
      max_depth = max_depth,
      min_rows = min_rows,
      sample_rate = sample_rate,
      col_sample_rate_per_tree = col_sample_rate_per_tree,
      balance_classes = balance_classes,
      seed = seed,
      stopping_rounds = 0
    )
    if (isTRUE(verbose)) {
      msg_type <- if (is.factor(df2[[y]]) || is.logical(df2[[y]])) "classification" else "regression"
      message(sprintf("Imputing '%s' with DRF (%s)…", y, msg_type))
    }
    model <- NULL
    tmp_out <- utils::capture.output({
      model <- do.call(h2o::h2o.randomForest, params)
    }, type = "output")
    if (length(tmp_out) > 0) {
      for (ln in tmp_out) message(ln)
    }
    full_pred_hf <- NULL
    pred_conv <- utils::capture.output({
      full_pred_hf <- h2o::as.h2o(df2[, x, drop = FALSE])
    }, type = "output")
    if (length(pred_conv) > 0) {
      for (ln in pred_conv) message(ln)
    }
    pred_out <- NULL
    tmp_pred <- utils::capture.output({
      pred_out <- h2o::h2o.predict(model, full_pred_hf)
    }, type = "output")
    if (length(tmp_pred) > 0) {
      for (ln in tmp_pred) message(ln)
    }
    pred_vec <- as.vector(pred_out[, "predict"])
    if (is.factor(df2[[y]])) {
      cur_levels <- levels(df2[[y]])
      new_levels <- unique(as.character(pred_vec))
      df2[[y]] <- factor(df2[[y]], levels = union(cur_levels, new_levels))
      miss <- missing_mask(df2[[y]])
      df2[[y]][miss] <- as.character(pred_vec[miss])
    } else {
      vals <- as.numeric(pred_vec)
      miss <- missing_mask(df2[[y]])
      if (!is.null(round_digits) && !is.na(round_digits)) {
        vals[miss] <- round(vals[miss], digits = as.integer(round_digits))
      }
      df2[[y]][miss] <- vals[miss]
    }
    if (requireNamespace("cli", quietly = TRUE)) {
      try(cli::cli_progress_update(force = TRUE), silent = TRUE)
    }
  }
  out <- as.data.frame(df2, stringsAsFactors = FALSE)
  for (nm in names(out)) {
    cls <- classes[[nm]]
    if (cls %in% c("integer", "numeric")) {
      out[[nm]] <- as.numeric(out[[nm]])
      if (identical(cls, "integer") && all(!is.na(out[[nm]]))) {
        if (all(abs(out[[nm]] - round(out[[nm]])) < .Machine$double.eps^0.5)) {
          out[[nm]] <- as.integer(round(out[[nm]]))
        }
      }
    } else if (cls %in% c("factor", "ordered")) {
      out[[nm]] <- as.factor(out[[nm]])
      if (identical(cls, "ordered")) {
        out[[nm]] <- factor(out[[nm]], ordered = TRUE)
      }
    } else if (identical(cls, "logical")) {
      if (is.factor(out[[nm]])) {
        lv <- levels(out[[nm]])
        if (length(lv) == 2) {
          out[[nm]] <- out[[nm]] == lv[2]
        }
      } else {
        out[[nm]] <- as.logical(out[[nm]])
      }
    } else if (identical(cls, "character")) {
      out[[nm]] <- as.character(out[[nm]])
    }
  }
  out
}