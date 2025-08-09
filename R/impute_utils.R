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
