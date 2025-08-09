#' inst/scripts/impute.R
#'
#' Reads CSV from stdin, runs `impute_mean()` helper, writes CSV to stdout.

suppressPackageStartupMessages({
  library(rprojroot)
  library(readr)
})

# 1 Find project root (uses DESCRIPTION in your root)
root_crit <- rprojroot::is_r_package
PROJ_ROOT  <- root_crit$find_file()

# 2️ Source config (sets WD & paths) and helper
source(root_crit$find_file("R/config.R"))
source(root_crit$find_file("R/impute_utils.R"))

# 3️ I/O: read stdin -> helper -> write stdout
df_in  <- read_csv(file("stdin"), show_col_types = FALSE)
df_out <- impute_mean(df_in)
write_csv(df_out, stdout())
