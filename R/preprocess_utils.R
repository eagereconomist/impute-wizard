# R/preprocess_utils.R

#' Load CSV file into a data.frame
#'
#' @param file_path A character string representing the path to the CSV file.
#' @return A data.frame containing the loaded data.
#' @examples
#' \dontrun{
#'   df <- load_csv("path/to/your/data.csv")
#'   head(df)
#' }
#' @export
load_csv <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("File does not exist at the specified path.", call. = FALSE)
  }

  df <- read.csv(file_path, stringsAsFactors = FALSE)
  df
}
