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