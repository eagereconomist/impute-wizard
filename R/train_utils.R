#' Read newline-separated integers from a file
#'
#' Reads lines, trims whitespace, drops empties, and converts to integers.
#' @param path Path to a text file.
#' @return Integer vector (may contain NAs if unparsable lines exist).
#' @keywords internal
#' @noRd
.read_lines_int <- function(path) {
  x <- readLines(path, warn = FALSE)
  x <- x[nzchar(x)]
  as.integer(trimws(x))
}

#' Read a boolean training mask from CSV
#'
#' Loads a CSV and returns a logical vector. Uses the first logical column by
#' default, otherwise accepts numeric 0/1 or common true/false strings.
#' @param path Path to the CSV file.
#' @param mask_col Optional column name to use.
#' @return Logical vector the length of the CSV.
#' @keywords internal
#' @noRd
.read_mask_csv <- function(path, mask_col = NULL) {
  if (!requireNamespace("readr", quietly = TRUE))
    stop("Package 'readr' is required for reading mask CSV.", call. = FALSE)
  mdf <- readr::read_csv(path, show_col_types = FALSE)
  if (is.null(mask_col)) {
    is_log <- vapply(mdf, is.logical, logical(1))
    if (any(is_log)) return(mdf[[which(is_log)[1]]])
    is_num <- vapply(mdf, is.numeric, logical(1))
    if (any(is_num)) return(mdf[[which(is_num)[1]]]!=0)
    stop("Could not find a logical/numeric mask column in mask CSV; specify --mask-col.", call. = FALSE)
  } else {
    if (!mask_col %in% names(mdf)) stop("mask_col not found in mask CSV.", call. = FALSE)
    col <- mdf[[mask_col]]
    if (is.logical(col)) return(col)
    if (is.numeric(col)) return(col != 0)
    if (is.character(col)) return(tolower(trimws(col)) %in% c("true", "t", "1", "yes", "y"))
    stop("mask column must be logical/numeric/character (true/false).", call. = FALSE)
  }
}

#' Resolve training row indices
#'
#' Determines training rows using (in order of precedence): `train_rows_path`
#' (newline-separated 1-based indices), `train_mask_path` (CSV boolean mask),
#' or a random sample fraction when `train_frac < 1`.
#' @param df Data frame whose rows define the universe.
#' @param train_rows_path Optional path to text file of indices.
#' @param train_mask_path Optional path to CSV with mask column.
#' @param mask_col Optional name of mask column within the CSV.
#' @param train_frac Numeric in (0,1]; fraction of rows to sample when paths are NULL.
#' @param seed Integer seed for sampling when `train_frac < 1`.
#' @return Integer vector of 1-based row indices.
#' @keywords internal
#' @noRd
resolve_train_rows <- function(df, train_rows_path, train_mask_path, mask_col,
                               train_frac = 1, seed = 1) {
  n <- nrow(df)
  if (!is.null(train_rows_path)) {
    idx <- .read_lines_int(train_rows_path)
    idx <- idx[!is.na(idx) & idx >= 1L & idx <= n]
    idx <- sort(unique(idx))
    if (!length(idx)) stop("No valid indices found in --train-rows.", call. = FALSE)
    return(idx)
  }

  if (!is.null(train_mask_path)) {
    mk <- .read_mask_csv(train_mask_path, mask_col = mask_col)
    if (length(mk) != n) stop("Train mask length does not match input rows.", call. = FALSE)

    mk <- as.logical(mk)
    idx <- which(!is.na(mk) & mk)

    if (!length(idx)) {
      vals <- tryCatch({
        tbl <- sort(table(mk, useNA = "ifany"), decreasing = TRUE)
        paste(capture.output(print(tbl)), collapse = " | ")
      }, error = function(e) "unavailable")
      stop(sprintf("No TRUE rows in train mask. Value counts: %s", vals), call. = FALSE)
    }
    return(idx)
  }

  if (!is.numeric(train_frac) || train_frac <= 0 || train_frac > 1)
    stop("`train_frac` must be in (0,1].", call. = FALSE)
  if (train_frac < 1) { set.seed(seed); sample(seq_len(n), floor(train_frac * n)) } else seq_len(n)
}

#' Deterministic 2/3-way split for train/val/test
#' @keywords internal
#' @noRd
make_split_mask <- function(df, probs = c(0.7, 0.15, 0.15),
                            id_col = NULL, stratify_by = NULL, seed = 2798) {
  stopifnot(length(probs) %in% c(2L,3L))
  probs <- probs / sum(probs)
  if (length(probs) == 2L) probs <- c(probs, 1 - sum(probs))

  if (!is.null(id_col) && id_col %in% names(df)) {
    if (!requireNamespace("digest", quietly = TRUE)) {
      stop("Package 'digest' is required for deterministic split.", call. = FALSE)
    }
    u <- vapply(df[[id_col]], function(x) {
      h <- digest::digest(x, algo = "murmur32", serialize = FALSE)
      as.numeric(as.hexmode(substr(h, 1, 8))) / 2^32
    }, numeric(1))
  } else { set.seed(seed); u <- runif(nrow(df)) }

  assign_bucket <- function(u_vec) {
    cuts <- c(0, cumsum(probs))
    as.integer(cut(u_vec, breaks = cuts, include.lowest = TRUE, labels = FALSE))
  }

  if (!is.null(stratify_by) && stratify_by %in% names(df)) {
    buckets <- integer(nrow(df))
    for (lvl in unique(df[[stratify_by]])) {
      idx <- which(df[[stratify_by]] == lvl)
      buckets[idx] <- assign_bucket(u[idx])
    }
    buckets
  } else assign_bucket(u)
}