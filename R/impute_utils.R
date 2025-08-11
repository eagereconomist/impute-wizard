# R/impute_utils.R


#' Impute mean for each numeric column in a data.frame
#'
#' @param df A data.frame with possible missing values
#' @return A data.frame with missing numeric values replaced by the column mean
#' @export
impute_mean <- function(df) {
  if (!is.data.frame(df)) stop("`df` must be a data.frame", call. = FALSE)
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) col[is.na(col)] <- mean(col, na.rm = TRUE)
    col
  })
  df
}


#' Impute median for numeric columns
#' @param df data.frame
#' @return data.frame
#' @export 
impute_median <- function(df) {
  stopifnot(is.data.frame(df))
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) col[is.na(col)] <- stats::median(col, na.rm = TRUE)
    col
  })
  df
}


#' Impute mode for columns (numeric or character)
#' @param df data.frame
#' @return data.frame
#' @export
impute_mode <- function(df) {
  stopifnot(is.data.frame(df))
  mode_of <- function(x) {
    ux <- unique(x[!is.na(x)])
    if (length(ux) == 0) return(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  df[] <- lapply(df, function(col) {
    m <- mode_of(col)
    col[is.na(col)] <- m
    col
  })
  df
}


#' kNN Imputation (tidymodels/recipes)
#'
#' Impute missing values using k-nearest neighbors on selected columns.
#'
#' @param df A data.frame or tibble.
#' @param cols Columns to impute; tidy-select (bare selectors) or character vector.
#'             Default: all columns that contain any NA.
#' @param neighbors Number of neighbors (k). Default: 5.
#' @param id_cols Optional tidy-select or character vector of ID/index-like columns to
#'                EXCLUDE from neighbor distance calculation. They remain in output.
#' @return A data.frame with NAs imputed (same column classes as input).
#' @export
impute_knn_recipes <- function(df,
                               cols = NULL,
                               neighbors = 5,
                               id_cols = NULL) {
  stopifnot(is.data.frame(df))
  if (!requireNamespace("recipes", quietly = TRUE)) {
    stop("Package 'recipes' is required. Install it first.", call. = FALSE)
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package 'rlang' is required. Install it first.", call. = FALSE)
  }
  if (!requireNamespace("tidyselect", quietly = TRUE)) {
    stop("Package 'tidyselect' is required. Install it first.", call. = FALSE)
  }

  na_cols <- names(df)[vapply(df, function(x) any(is.na(x)), logical(1))]
  if (!length(na_cols)) return(df)

  rec <- recipes::recipe(~ ., data = df)

  # Handle id columns (bare selectors or character)
  id_q <- rlang::enquo(id_cols)
  if (!rlang::quo_is_null(id_q)) {
    rec <- rec |> recipes::update_role(!!id_q, new_role = "id")
  } else if (is.character(id_cols)) {
    rec <- rec |> recipes::update_role(tidyselect::all_of(id_cols), new_role = "id")
  }

  cols_q <- rlang::enquo(cols)
  if (rlang::quo_is_null(cols_q)) {
    # Put all_of() directly in the selecting arg (no temp variable)
    rec <- rec |>
      recipes::step_impute_knn(tidyselect::all_of(na_cols),
                               neighbors = neighbors,
                               impute_with = recipes::all_predictors())
  } else {
    rec <- rec |>
      recipes::step_impute_knn(!!cols_q,
                               neighbors = neighbors,
                               impute_with = recipes::all_predictors())
  }

  # Bake on the original data to avoid retain=TRUE requirement
  prepped <- recipes::prep(rec, training = df, retain = FALSE)
  baked   <- recipes::bake(prepped, new_data = df)

  as.data.frame(baked, stringsAsFactors = FALSE)
}


#' DRF (H2O) Model-based Imputation
#'
#' For each column with missing values, trains an H2O Distributed Random Forest
#' using all *other* columns as predictors, fits on rows where the target is observed,
#' predicts for rows where it is missing, and writes back predictions.
#'
#' @param df A data.frame or tibble.
#' @param ntrees Number of trees. Default: 200.
#' @param max_depth Max tree depth. Default: 20.
#' @param seed RNG seed. Default: 1.
#' @param h2o_connection Optional; if NULL will call h2o::h2o.init().
#' @param verbose Logical; print progress. Default: TRUE.
#' @return A data.frame with model-based imputations applied.
#' @details
#' - Characters are treated as factors (categoricals) for H2O.
#' - Numeric targets use regression; categorical targets use classification.
#' - Columns with all NA or no variance are skipped.
#' @export
impute_h2o_drf <- function(df,
                           ntrees = 200,
                           max_depth = 20,
                           seed = 1,
                           h2o_connection = NULL,
                           verbose = TRUE) {
  stopifnot(is.data.frame(df))
  if (!requireNamespace("h2o", quietly = TRUE)) {
    stop("Package 'h2o' is required. Install it first.", call. = FALSE)
  }

  # Reuse an existing conn if present; otherwise start one
  if (is.null(h2o_connection)) {
    existing <- try(h2o::h2o.getConnection(), silent = TRUE)
    if (!inherits(existing, "H2OConnection")) {
      if (verbose) message("Starting H2O…")
      h2o::h2o.init(nthreads = 1, strict_version_check = FALSE)
    } else if (verbose) {
      message("Reusing existing H2O connection.")
    }
  }

  classes <- vapply(df, function(x) class(x)[1], character(1))

  df2 <- df
  char_cols <- names(df2)[vapply(df2, is.character, logical(1))]
  if (length(char_cols)) for (cc in char_cols) df2[[cc]] <- as.factor(df2[[cc]])

  hf <- h2o::as.h2o(df2)
  colnames_df <- colnames(df2)

  na_cols <- names(df2)[vapply(df2, function(x) any(is.na(x)), logical(1))]
  if (!length(na_cols)) {
    if (verbose) message("No missing values detected.")
    return(df)
  }

  for (y in na_cols) {
    y_vec <- df2[[y]]
    if (all(is.na(y_vec))) {
      if (verbose) message(sprintf("Skipping '%s' (all values are NA).", y))
      next
    }
    if (length(unique(stats::na.omit(y_vec))) < 2) {
      if (verbose) message(sprintf("Skipping '%s' (insufficient class/variance).", y))
      next
    }

    x <- setdiff(colnames_df, y)
    observed <- !is.na(hf[[y]])
    train <- hf[observed, ]

    if (verbose) {
      msg_type <- if (is.factor(df2[[y]]) || is.logical(df2[[y]])) "classification" else "regression"
      message(sprintf("Imputing '%s' with DRF (%s)…", y, msg_type))
    }

    model <- h2o::h2o.randomForest(
      x = x, y = y, training_frame = train,
      ntrees = ntrees, max_depth = max_depth, seed = seed,
      stopping_rounds = 0
    )

    # Predict over ALL rows, then coalesce where original is NA.
    preds <- h2o::h2o.predict(model, hf[, x])
    new_vals <- preds[,"predict"]               # works for regression & classification

    # IMPORTANT: use is.na() (H2O S3), not h2o.isna (not exported in 3.44)
    hf[[y]] <- h2o::h2o.ifelse(is.na(hf[[y]]), new_vals, hf[[y]])
  }

  out <- as.data.frame(hf, stringsAsFactors = FALSE)

  # restore classes (unchanged from your last version)
  for (nm in names(out)) {
    cls <- classes[[nm]]
    if (cls %in% c("integer", "numeric")) {
      out[[nm]] <- as.numeric(out[[nm]])
      if (cls == "integer" && all(!is.na(out[[nm]]))) {
        if (all(abs(out[[nm]] - round(out[[nm]])) < .Machine$double.eps^0.5)) {
          out[[nm]] <- as.integer(round(out[[nm]]))
        }
      }
    } else if (cls %in% c("factor", "ordered")) {
      out[[nm]] <- as.factor(out[[nm]])
      if (cls == "ordered") out[[nm]] <- factor(out[[nm]], ordered = TRUE)
    } else if (cls == "logical") {
      if (is.factor(out[[nm]])) {
        lv <- levels(out[[nm]])
        if (length(lv) == 2) out[[nm]] <- out[[nm]] == lv[2]
      } else {
        out[[nm]] <- as.logical(out[[nm]])
      }
    } else if (cls == "character") {
      out[[nm]] <- as.character(out[[nm]])
    }
  }

  out
}
