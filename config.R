if (requireNamespace("dotenv", quietly = TRUE)) {
  dotenv::load_dot_env(file = here::here(".env"))
}

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Please install the 'here' package: install.packages('here')")
}
PROJ_ROOT <- here::here()
message("PROJ_ROOT: ", PROJ_ROOT)

if (!requireNamespace("fs", quietly = TRUE)) {
  stop("Please install the 'fs' package: install.packages('fs')")
}

DATA_DIR <- fs::path(PROJ_ROOT, "data")
RAW_DATA_DIR <- fs::path(DATA_DIR, "raw")
PREPROCESSED_DATA_DIR <- fs::path(DATA_DIR, "preprocessed")
PROCESSED_DATA_DIR <- fs::path(DATA_DIR, "processed")
EXTERNAL_DATA_DIR <- fs::path(DATA_DIR, "external")

MODELS_DIR <- fs::path(PROJ_ROOT, "models")

REPORTS_DIR <- fs::path(PROJ_ROOT, "models")
FIGURES_DIR <- fs::path(REPORTS_DIR, "figures")

dirs <- list(
  DATA_DIR, RAW_DATA_DIR, PREPROCESSED_DATA_DIR, PROCESSED_DATA_DIR,
  EXTERNAL_DATA_DIR, MODELS_DIR, REPORTS_DIR, FIGURES_DIR
)
invisible(lapply(dirs, function(d) {
  if (!fs::dir_exists(d)) fs::dir_create(d)
}))

if (requireNamespace("logger", quitely = TRUE) &&
      requireNamespace("cli", quietly = TRUE)) {
  logger::log_appender(
    logger::appender_tee(function(line) cli::cat_line(line))
  )
}
