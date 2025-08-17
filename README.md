imputeflow
===========

This package provides a collection of imputation utilities and a command-line
interface (CLI) for imputing missing data.  Supported methods include simple
statistics (mean, median, mode), k-nearest-neighbour (kNN) imputation via
`tidymodels/recipes`, and model-based imputation using a distributed random
forest (DRF) implemented in the H2O platform.  Additional helpers handle
input/output, progress reporting, numeric rounding, and train/test splitting
to avoid data leakage.  See `exec/imputeflow` for the CLI.