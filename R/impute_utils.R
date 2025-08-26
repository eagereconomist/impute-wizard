#' Identify missing-like values
#'
#' Returns a logical vector marking values considered missing:
#' standard NA plus empty strings and literal "NA" for character/factor inputs.
#' @param x A vector of any base type.
#' @return Logical vector of the same length as `x`.
#' @examples
#' missing_mask(c(NA, "", "NA", "x"))
#' @keywords internal
#' @noRd
missing_mask <- function(x) {
  na_mask <- is.na(x)
  if (is.character(x) || is.factor(x)) {
    chr <- as.character(x)
    na_mask <- na_mask | chr == "" | chr == "NA"
  }
  na_mask
}
.is_missing <- missing_mask

#' Does a vector contain any missing-like values?
#'
#' Wrapper over `missing_mask()` that checks if any element is missing-like.
#' @param x A vector.
#' @return Logical scalar.
#' @examples
#' needs_imputation(c(1, NA, 3))
#' @keywords internal
#' @noRd
needs_imputation <- function(x) {
  any(missing_mask(x), na.rm = TRUE)
}

#' Normalize missing-like strings to NA
#'
#' For character/factor vectors, converts "" and "NA" to actual NA values.
#' @param x A vector.
#' @return A vector with "" and "NA" coerced to NA for chr/fct; unchanged otherwise.
#' @keywords internal
#' @noRd
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

#' Optionally round numeric columns after imputation
#'
#' Applies rounding only where both `original` and `imputed` columns are numeric.
#' @param original Original data.frame before imputation.
#' @param imputed Data.frame after imputation.
#' @param digits Integer number of digits to round to; NA disables rounding.
#' @return The `imputed` data.frame (rounded where applicable).
#' @examples
#' # round_imputed_numerics(orig_df, imp_df, digits = 2)
#' @keywords internal
#' @noRd
round_imputed_numerics <- function(original, imputed, digits = NA) {
  if (is.na(digits)) return(imputed)
  roundable_cols <- names(imputed)[
    vapply(imputed, is.numeric, logical(1)) &
      vapply(original, is.numeric, logical(1))
  ]
  imputed[roundable_cols] <- lapply(imputed[roundable_cols], round, digits = digits)
  imputed
}

#' Fallback (“or else”) infix operator
#'
#' Returns `x` if it is non-empty (and not a zero-length/empty character),
#' otherwise returns `y`.
#' @name %||%
#' @usage x %||% y
#' @param x Primary value.
#' @param y Fallback value.
#' @return Either `x` or `y`.
#' @keywords internal
#' @noRd

`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  if (is.character(x)) {
    if (length(x) == 0) return(y)
    if (length(x) == 1 && !nzchar(x)) return(y)
  }
  x
}

#' Fit a mean-imputation specification
#'
#' Computes column-wise means over training rows for numeric columns that have
#' missing values.
#' @param df Data frame to analyze.
#' @param cols Optional character vector of target columns (default: auto-detect).
#' @param train_rows Optional integer vector of row indices used to compute stats.
#' @return A list with fields: `type = "mean"`, `cols`, and numeric `stats`.
#' @export
#' @examples
#' spec <- fit_mean_spec(iris, cols = "Sepal.Length")
fit_mean_spec <- function(df, cols = NULL, train_rows = NULL) {
  if (!inherits(df, "data.frame")) df <- as.data.frame(df, stringsAsFactors = FALSE)
  out_cols <- if (is.null(cols)) names(df) else intersect(cols, names(df))
  out_cols <- out_cols[vapply(df[out_cols], function(x) is.numeric(x) && needs_imputation(x), logical(1))]
  n <- nrow(df)
  if (is.null(train_rows)) train_rows <- seq_len(n)
  stats <- vapply(out_cols, function(nm) mean(df[[nm]][train_rows], na.rm = TRUE), numeric(1))
  list(type = "mean", cols = out_cols, stats = stats)
}

#' Apply a mean-imputation specification
#'
#' Fills missing values in the specified columns using the provided mean spec.
#' @param df Data frame to impute.
#' @param spec A spec produced by [fit_mean_spec()].
#' @param round_digits Optional integer; round numeric columns post-imputation.
#' @return Data frame with imputed values.
#' @export
apply_mean_spec <- function(df, spec, round_digits = NA) {
  stopifnot(is.list(spec), identical(spec$type, "mean"))
  if (!inherits(df, "data.frame")) df <- as.data.frame(df, stringsAsFactors = FALSE)
  out <- df
  for (nm in spec$cols) {
    if (!nm %in% names(out)) next
    col <- out[[nm]]
    m <- spec$stats[[nm]]
    if (!is.na(m)) {
      mask <- missing_mask(col)
      col[mask] <- m
      out[[nm]] <- col
    }
  }
  round_imputed_numerics(df, out, digits = round_digits)
}

#' Fit a median-imputation specification
#'
#' Computes column-wise medians over training rows for numeric columns that have
#' missing values.
#' @inheritParams fit_mean_spec
#' @return A list with fields: `type = "median"`, `cols`, and numeric `stats`.
#' @export
fit_median_spec <- function(df, cols = NULL, train_rows = NULL) {
  if (!inherits(df, "data.frame")) df <- as.data.frame(df, stringsAsFactors = FALSE)
  out_cols <- if (is.null(cols)) names(df) else intersect(cols, names(df))
  out_cols <- out_cols[vapply(df[out_cols], function(x) is.numeric(x) && needs_imputation(x), logical(1))]
  n <- nrow(df); if (is.null(train_rows)) train_rows <- seq_len(n)
  stats <- vapply(out_cols, function(nm) stats::median(df[[nm]][train_rows], na.rm = TRUE), numeric(1))
  list(type = "median", cols = out_cols, stats = stats)
}

#' Apply a median-imputation specification
#'
#' Fills missing values in the specified columns using the provided median spec.
#' @param df Data frame to impute.
#' @param spec A spec produced by [fit_median_spec()].
#' @param round_digits Optional integer; round numeric columns post-imputation.
#' @return Data frame with imputed values.
#' @export
apply_median_spec <- function(df, spec, round_digits = NA) {
  stopifnot(is.list(spec), identical(spec$type, "median"))
  if (!inherits(df, "data.frame")) df <- as.data.frame(df, stringsAsFactors = FALSE)
  out <- df
  for (nm in spec$cols) {
    if (!nm %in% names(out)) next
    col <- out[[nm]]
    m <- spec$stats[[nm]]
    if (!is.na(m)) {
      mask <- missing_mask(col)
      col[mask] <- m
      out[[nm]] <- col
    }
  }
  round_imputed_numerics(df, out, digits = round_digits)
}

#' Fit a mode-imputation specification
#'
#' Computes per-column modes over training rows (after normalizing "" and "NA"
#' to missing). Works with numeric, character, and factor inputs.
#' @inheritParams fit_mean_spec
#' @return A list with fields: `type = "mode"`, `cols`, and list `stats`.
#' @export
fit_mode_spec <- function(df, cols = NULL, train_rows = NULL) {
  if (!inherits(df, "data.frame")) df <- as.data.frame(df, stringsAsFactors = FALSE)
  out_cols <- if (is.null(cols)) names(df) else intersect(cols, names(df))
  out_cols <- out_cols[vapply(df[out_cols], needs_imputation, logical(1))]

  n <- nrow(df); if (is.null(train_rows)) train_rows <- seq_len(n)

  mode_of <- function(x) {
    ux <- unique(x[!is.na(x)])
    if (!length(ux)) return(NA)
    ux[which.max(tabulate(match(x, ux)))]
  }

  stats <- lapply(out_cols, function(nm) {
    col <- df[[nm]]
    col_chr <- as.character(col)
    col_chr[trimws(col_chr) == ""] <- NA_character_
    col_chr[col_chr == "NA"] <- NA_character_
    tr <- col_chr[train_rows]
    mode_of(tr[!is.na(tr)])
  })
  names(stats) <- out_cols
  list(type = "mode", cols = out_cols, stats = stats)
}

#' Apply a mode-imputation specification
#'
#' Fills missing values using the provided mode spec, then restores the original
#' column classes (numeric/integer/logical/character/factor). Optional final
#' rounding is applied to numeric columns.
#' @param df Data frame to impute.
#' @param spec A spec produced by [fit_mode_spec()].
#' @param round_digits Optional integer; round numeric columns post-imputation.
#' @return Data frame with imputed values.
#' @export
apply_mode_spec <- function(df, spec, round_digits = NA) {
  stopifnot(is.list(spec), identical(spec$type, "mode"))
  if (!inherits(df, "data.frame")) df <- as.data.frame(df, stringsAsFactors = FALSE)
  out <- df
  for (nm in spec$cols) {
    if (!nm %in% names(out)) next

    original <- out[[nm]]
    was_factor <- is.factor(original)
    was_ordered <- is.ordered(original)

    col_chr <- as.character(original)
    col_chr[trimws(col_chr) == ""] <- NA_character_
    col_chr[col_chr == "NA"] <- NA_character_

    m <- spec$stats[[nm]]

    if (!is.na(m)) {
      fill <- is.na(col_chr)
      if (any(fill)) col_chr[fill] <- as.character(m)
    }

    if (was_factor) {
      old_lv <- levels(original)
      new_vec <- factor(col_chr, levels = union(old_lv, as.character(m)))
      if (was_ordered) new_vec <- factor(new_vec, levels = levels(new_vec), ordered = TRUE)
      out[[nm]] <- new_vec
    } else {
      cls <- class(original)[1]
      if (cls %in% c("numeric", "integer")) {
        new_num <- suppressWarnings(as.numeric(col_chr))
        out[[nm]] <- new_num
      } else if (cls == "logical") {
        low <- tolower(col_chr)
        is_true  <- low %in% c("true", "t", "1", "yes", "y")
        is_false <- low %in% c("false", "f", "0", "no", "n")
        new_logical <- ifelse(is_true, TRUE, ifelse(is_false, FALSE, NA))
        out[[nm]] <- new_logical
      } else {
        out[[nm]] <- col_chr
      }
    }
  }
  round_imputed_numerics(df, out, digits = round_digits)
}


#' Fit a kNN-imputation specification (recipes)
#'
#' Builds a recipes pipeline to impute specified numeric columns via kNN, using
#' the provided training rows and predictor set.
#' @param df Data frame.
#' @param cols Optional numeric target columns (default: numeric columns with NA).
#' @param neighbors Integer number of neighbors (k).
#' @param train_rows Optional integer vector of training row indices.
#' @param exclude_predictors Optional character vector of predictors to exclude.
#' @return A list with fields: `type = "knn"`, `cols`, `predictors`, `neighbors`,
#'   and a prepped recipes object (`recipe`).
#' @export
#' @seealso recipes::step_impute_knn
fit_knn_spec <- function(df, cols = NULL, neighbors = 5, train_rows = NULL,
                         exclude_predictors = NULL) {
  stopifnot(is.data.frame(df))
  df <- tibble::as_tibble(df)

  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (!is.null(cols)) {
    cols <- intersect(cols, numeric_cols)
  } else {
    cols <- numeric_cols[vapply(df[numeric_cols], function(x) any(is.na(x)), logical(1))]
  }

  predictors <- setdiff(numeric_cols, exclude_predictors %||% character(0))
  predictors <- union(predictors, cols)

  if (is.null(train_rows)) train_rows <- seq_len(nrow(df))
  df_recipe_all   <- dplyr::select(df, tidyselect::all_of(predictors))
  df_train_recipe <- df_recipe_all[train_rows, , drop = FALSE]

  rec <- recipes::recipe(~ ., data = df_train_recipe)
  if (length(cols)) {
    rec <- recipes::step_impute_knn(rec, !!!rlang::syms(cols), neighbors = neighbors)
  } else {
    return(list(
      type        = "knn",
      cols        = character(0),
      predictors  = predictors,
      neighbors   = neighbors,
      recipe      = recipes::prep(rec, training = df_train_recipe, verbose = FALSE)
    ))
  }

  rec_prep <- recipes::prep(rec, training = df_train_recipe, verbose = FALSE)

  list(
    type       = "knn",
    cols       = cols,
    predictors = predictors,
    neighbors  = neighbors,
    recipe     = rec_prep
  )
}

#' Apply a kNN-imputation specification (recipes)
#'
#' Applies a prepped recipes kNN spec to the input data. Ensures predictors
#' exist, imputes the target columns, and optionally rounds numeric outputs.
#' @param df Data frame to impute.
#' @param spec A spec produced by [fit_knn_spec()].
#' @param round_digits Optional integer; round numeric columns post-imputation.
#' @return Data frame with imputed values.
#' @export
apply_knn_spec <- function(df, spec, round_digits = NA) {
  stopifnot(is.list(spec), identical(spec$type, "knn"))
  orig <- if (inherits(df, "data.frame")) df else as.data.frame(df, stringsAsFactors = FALSE)
  df <- tibble::as_tibble(df)

  miss <- setdiff(spec$predictors, names(df))
  if (length(miss)) cli::cli_abort(c("x Missing predictor columns in input for kNN apply:", paste0("* ", miss)))

  df_recipe <- dplyr::select(df, tidyselect::all_of(spec$predictors))
  imputed_subset <- recipes::bake(spec$recipe, new_data = df_recipe)

  df[spec$cols] <- imputed_subset[spec$cols]
  if (!is.na(round_digits)) {
    df_rounded <- round_imputed_numerics(orig, as.data.frame(df, stringsAsFactors = FALSE),
                                         digits = round_digits)
    df <- tibble::as_tibble(df_rounded)
  }
  as.data.frame(df, stringsAsFactors = FALSE)
}

#' Impute with H2O Distributed Random Forest (single-session fit & apply)
#'
#' For each target column with missingness, trains a DRF on observed rows and
#' predicts missing entries, handling both numeric and factor targets. Optionally
#' rounds numeric columns at the end.
#' @param df Data frame.
#' @param cols Optional target columns (default: auto-detect with missingness).
#' @param exclude_predictors Optional predictors to exclude from modeling.
#' @param train_rows Optional integer indices used for model training.
#' @param ntrees,max_depth,min_rows,sample_rate,col_sample_rate_per_tree DRF params.
#' @param balance_classes Logical; whether to balance classes for classification.
#' @param seed Random seed.
#' @param round_digits Optional integer; round numeric columns post-imputation.
#' @param h2o_threads,h2o_mem,h2o_port,h2o_connection H2O cluster settings.
#' @param verbose Logical; emit progress messages.
#' @param progress_hook Optional callback invoked per target column.
#' @return Data frame with imputed values.
#' @export
#' @seealso h2o::h2o.randomForest
impute_h2o_drf_fit_apply <- function(df,
                                     cols = NULL,
                                     exclude_predictors = NULL,
                                     train_rows = NULL,
                                     ntrees = 50,
                                     max_depth = 10,
                                     min_rows = 5,
                                     sample_rate = 0.8,
                                     col_sample_rate_per_tree = 0.8,
                                     balance_classes = FALSE,
                                     seed = 1,
                                     round_digits = NA,
                                     h2o_threads = NULL,
                                     h2o_mem = NULL,
                                     h2o_port = "auto",
                                     h2o_connection = NULL,
                                     nfolds = 0,
                                     fold_assignment = "AUTO",
                                     verbose = TRUE,
                                     progress_hook = NULL) {
  if (!inherits(df, "data.frame")) df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!requireNamespace("h2o", quietly = TRUE)) {
    stop("Package 'h2o' is required. Install it first.", call. = FALSE)
  }

  nthreads_final <- if (!is.null(h2o_threads) && !is.na(h2o_threads)) as.integer(h2o_threads) else -1L
  mem_final      <- if (!is.null(h2o_mem) && nzchar(h2o_mem)) h2o_mem else NULL
  quiet_flag     <- !isTRUE(verbose)

  if (is.null(h2o_connection)) {
    if (!is.null(mem_final) || !is.null(h2o_threads) || !identical(h2o_port, "auto")) {
      suppressWarnings(try(h2o::h2o.shutdown(prompt = FALSE), silent = TRUE))
    }
    if (quiet_flag) {
      suppressWarnings(suppressMessages(
        try(h2o::h2o.init(nthreads = nthreads_final, max_mem_size = mem_final,
                          strict_version_check = FALSE), silent = TRUE)
      ))
    } else {
      try(h2o::h2o.init(nthreads = nthreads_final, max_mem_size = mem_final,
                        strict_version_check = FALSE), silent = TRUE)
    }
  }

  classes <- vapply(df, function(x) class(x)[1], character(1))
  df2 <- df
  df2[] <- lapply(df2, replace_missing_strings)
  ch_cols <- names(df2)[vapply(df2, is.character, logical(1))]
  if (length(ch_cols)) for (cc in ch_cols) df2[[cc]] <- as.factor(df2[[cc]])
  colnames_df <- colnames(df2)

  if (is.null(cols)) {
    cols <- names(df2)[vapply(df2, needs_imputation, logical(1))]
  } else {
    cols <- intersect(cols, names(df2))
    cols <- cols[vapply(df2[cols], needs_imputation, logical(1))]
  }

  predictors_all <- setdiff(colnames_df, exclude_predictors %||% character(0))
  if (is.null(train_rows)) train_rows <- seq_len(nrow(df2))

  for (y in cols) {
    if (is.function(progress_hook)) progress_hook()

    y_vec <- df2[[y]]
    if (all(missing_mask(y_vec))) next
    if (length(unique(stats::na.omit(y_vec))) < 2) next

    x <- setdiff(predictors_all, y)
    obs <- which(!missing_mask(y_vec))
    obs <- intersect(obs, train_rows)
    if (!length(obs)) next

    train_df <- df2[obs, , drop = FALSE]

    train_hf <- h2o::as.h2o(train_df)
    params <- list(
      x = x, y = y, training_frame = train_hf,
      ntrees = ntrees, max_depth = max_depth, min_rows = min_rows,
      sample_rate = sample_rate, col_sample_rate_per_tree = col_sample_rate_per_tree,
      balance_classes = balance_classes, seed = seed, stopping_rounds = 0
    )
    if (!is.null(nfolds) && nfolds > 0) {
      params$nfolds <- as.integer(nfolds)
      params$fold_assignment <- fold_assignment
    }
    model <- do.call(h2o::h2o.randomForest, params)

    full_pred_hf <- h2o::as.h2o(df2[, x, drop = FALSE])
    pred_out <- h2o::h2o.predict(model, full_pred_hf)
    pred_vec <- as.vector(pred_out[, "predict"])

    if (is.factor(df2[[y]])) {
      cur_levels <- levels(df2[[y]]); new_levels <- unique(as.character(pred_vec))
      df2[[y]] <- factor(df2[[y]], levels = union(cur_levels, new_levels))
      miss <- missing_mask(df2[[y]]); df2[[y]][miss] <- as.character(pred_vec[miss])
    } else {
      vals <- suppressWarnings(as.numeric(pred_vec))
      miss <- missing_mask(df2[[y]]); df2[[y]][miss] <- vals[miss]
    }
  }

  out <- as.data.frame(df2, stringsAsFactors = FALSE)
  for (nm in names(out)) {
    cls <- classes[[nm]]
    if (cls %in% c("integer", "numeric")) out[[nm]] <- as.numeric(out[[nm]])
    else if (cls %in% c("factor", "ordered")) {
      out[[nm]] <- as.factor(out[[nm]]); if (identical(cls, "ordered")) out[[nm]] <- factor(out[[nm]], ordered = TRUE)
    } else if (identical(cls, "logical")) {
      if (is.factor(out[[nm]])) { lv <- levels(out[[nm]]); if (length(lv) == 2) out[[nm]] <- out[[nm]] == lv[2] }
      else out[[nm]] <- as.logical(out[[nm]])
    } else if (identical(cls, "character")) {
      out[[nm]] <- as.character(out[[nm]])
    }
  }

  if (!is.null(round_digits) && !is.na(round_digits))
    out <- round_imputed_numerics(df, out, digits = round_digits)

  out
}