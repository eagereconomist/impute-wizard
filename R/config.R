# R/config.R

# Determine the package root directory (where DESCRIPTION lives)
PROJ_ROOT <- rprojroot::find_root(rprojroot::is_r_package)

# Standardized paths relative to the project root
DATA_RAW       <- fs::path(PROJ_ROOT, "data", "raw")
MODELS_PATH    <- fs::path(PROJ_ROOT, "models")
REPORTS_DIR    <- fs::path(PROJ_ROOT, "reports", "figures")
RESULTS_PATH   <- fs::path(PROJ_ROOT, "results")
