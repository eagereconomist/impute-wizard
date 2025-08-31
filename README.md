# imputeflow

Modular CLI-based data imputation in R

---

## Overview

`imputeflow` is a lightweight R package and CLI toolset for imputing missing values in tabular data using both statistical and machine learning methods. It supports `mean`, `median`, `mode`, `kNN` (K-nearest neighbors), and H2O’s `drf` (Distributed Random Forest) — all through a unified CLI interface. It is designed for stdin/stdout workflows and includes rich visual feedback (progress bars, timers, and CLI alerts).

Key features:

- CLI-first workflows for applying imputation at scale
- Supports statistical and ML-based imputation (via H2O, recipes)
- Clean stdin -> stdout support for UNIX-style pipelines
- Mudular, testable utils for imputation, rounding, and CLI feedback
- Rich terminal output with progress bars, alerts, and timers

---

### Quickstart

#### 1. Clone the repo
```bash
git clone git@github.com:you/imputeflow.git
cd imputeflow
```

#### 2. Set up the R environment
This project uses [renv](https://rstudio.github.io/renv/) for dependency management and reproducibility.

#### 2A. Install R packages required for setup
Open an R terminal (or RStudio) and install `renv` if it's not already available:

`renv` should already be installed, but if you're not sure, run:

```r
system.file(package = "renv")
```

If R spits out a path after running the above, `renv` is already installed, but if that doesn't happen, run the following:

```r
install.packages("renv")
```

#### 2B. Troubleshooting

If you see the following message in the R terminal at startup:

```r
— The project is out-of-sync —— use `renv::status()` for details.
```

After running the status command above, `renv` will most likely tell you that the project is out of sync.

Then use `renv::restore()` to install all packages pinned in the `renv.lock` file:

```r
renv::restore()
```

This will:
- Recreate the exact package versions used during development
- Set up a local project-specific library
- Ensure compatibility for CLI workflows (e.g., `optparse`, `cli`, `h2o`, `recipes`)

Alternative (fresh start):
If you're starting the project from scratch or want to regenerate the lockfile:

```r
renv::init()
```

And if as a result of running `renv::restore()` you see:

```r
— The library is already synchronized with the lockfile.
```

Move on to **Step 3** below.

#### 3. Build the package (registers helpers/exports)
Fresh clones can hit **"object not found"** for helpers until the pkg is documented & installed.

```r
devtools::document()
devtools::install()
```

#### 4. Snapshot and check status of environment
Sometimes `{h2o}` can be fussy on first load; a quick snapshot/restore should stabilize things.

```r
renv::snapshot()
renv::status()
```


## CLI Flags (common)

| Flag                  | Description                                                                 |
|-----------------------|-----------------------------------------------------------------------------|
| `--cols`              | Comma-separated list of columns to impute                                   |
| `--round`             | Rounds numeric imputations to N decimal places                              |
| `--train-frac`        | Fraction of rows to use for training (used when no mask is present)         |
| `--train-mask`        | Name of a column containing a binary mask for training (e.g. `__mask__`)    |
| `--exclude-predictors`| Comma-separated list of columns to exclude as predictors (DRF only)         |
| `--nfolds`            | Number of folds for H2O cross-validation (DRF only)                         |
| `--fold-assignment`   | H2O fold assignment method (e.g. `AUTO`, `Modulo`, `Random`)                |
| `--neighbors`         | Number of neighbors (kNN only)                                              |
| `--h2o-mem`           | H2O memory allocation (e.g. `16G`, `32G`)                                   |

## 📦 Project Structure

```
├── DESCRIPTION                 # Package metadata
├── LICENSE                     # License info
├── README.md                   # You are here
├── NAMESPACE                   # Exported functions
├── renv/                       # Environment snapshot + activation
│   ├── activate.R
│   └── settings.json
├── renv.lock                   # Pinned dependencies and versions
├── exec/                       # CLI entrypoint
│   └── imputeflow             # Main CLI script (e.g., cat file.csv | imputeflow mean)
├── R/                          # Core package logic
│   ├── config.R               # Path constants via rprojroot + fs
│   ├── h2o_utils.R            # H2O cluster mgmt + connection helpers
│   ├── impute_utils.R         # Imputation logic (mean, median, mode, kNN, rounding)
│   └── train_utils.R          # Helpers for fitting & splitting data
├── man/                        # Auto-generated Rd docs
│   ├── apply_knn_spec.Rd
│   ├── apply_mean_spec.Rd
│   ├── apply_median_spec.Rd
│   ├── apply_mode_spec.Rd
│   ├── ensure_h2o.Rd
│   ├── fit_knn_spec.Rd
│   ├── fit_mean_spec.Rd
│   ├── fit_median_spec.Rd
│   ├── fit_mode_spec.Rd
│   └── impute_h2o_drf_fit_apply.Rd
└── tests/                      # Test suite (testthat)
    ├── testthat.R
    └── testthat/
        ├── fixtures/
        │   └── bank_test_data.rds    # Reproducible imputation test input
        ├── helper-h2o-setup.R        # DRF test cluster config
        ├── teardown-h2o.R            # DRF shutdown logic
        ├── test-drf.R                # Tests for H2O DRF imputation
        ├── test-knn.R                # Tests for kNN imputation
        ├── test-mean.R               # Tests for mean imputation
        ├── test-median.R             # Tests for median imputation
        └── test-mode.R               # Tests for mode imputation
```

---

## Description

`exec/imputeflow`

This is the main CLI entrypoint for the `imputeflow` package. It dispatches to all available imputation methods (`mean`, `median`, `mode`, `knn`, `drf`) and handles:
- Argument parsing via `{optparse}`
- Reading input CSV from `stdin`, writing to `stdout`
- Column parsing, training row resolution, and rounding
- Method-specific CLI output (progress bars, alerts, missingness summaries)
- Theming and consistent UI across methods

The file uses the structure:

- **Global helpers**: `read_stdin_csv()`, `write_stdout_csv()`, `parse_cols()`, `redirect_start()` etc.
- **Themed CLI output**: consistent `{cli}` formatting, percentage banners, and status summaries
- **Method dispatch**: `run_mean()`, `run_median()`, `run_mode()`, `run_knn()`, `run_drf()` are registered and executed based on the first CLI argument

Not exported from the package — invoked only via the CLI as:

```bash
cat data.csv | exec/imputeflow <method> [options] > output.csv
```

## Examples

1. Mean imputation with rounding

```bash
cat data/example.csv | exec/imputeflow mean \
--cols <COLUMN>
--round 2
> output.csv
```

2. Median imputation (round to the nearest integer and training-only stats)

- If for some reason you wanted to impute a subset of your data using a naïve imputation method, you can!
(Although you shouldn't, see the "Why" section below)

```bash
cat data/example.csv | exec/imputeflow \
--round 0 \
--train-frac 0.3 \
> output.csv
```

3. kNN imputation with custom numbers of neighbors

```bash
cat data/example.csv | exec/imputeflow knn \
--cols <COLUMN>,<COLUMN1>,<COLUMN2> \
--neighbors 3 \
--round 2 \
> output.csv
```

4. kNN (train fraction, exlude id column, and rounding)

```bash
cat data/example.csv | exec/imputeflow \
--neighbors 8 \
--round 1 \
--exclude-predictors customer_id \
--train-frac 0.8 \
> output.csv
```

5. DRF imputation with memory setting

```bash
cat data/example.csv | exec/imputeflow drf
--cols <COLUMN>,<COLUMN1>,<COLUMN2>
--h2o-mem 32G
> output.csv
```

6. DRF (explicit train/val/test mask + CV + theme styling)

```bash
IMPUTEFLOW_STYLE=minimal \
cat data/example.csv | exec/imputeflow \
--nfolds 3 \
--fold-assignment Random \
--h2o-mem 4G \
--round 3 \
--exclude-predictors <COLUMN>,<COLUMN1>,<COLUMN2>,<COLUMN3> \
--train-mask /tmp/example_mask.csv --mask-col is_train \
> output.csv
```

## Why?

---

The point of `imputeflow` isn't to promote naïve imputation — it’s to have a **CLI-first** way to push any **dataset through a proper imputation pipeline**. You can:

- **stream data in via stdin and out via stdout** — no need to juggle intermediate files
- **mask rows into train/validation/test splits** (`--train-mask`), which is a best practice for fair evaluation
- choose **non-naïve methods** like:

1. kNN (via [recipes](https://recipes.tidymodels.org/) from [tidymodels](https://www.tidymodels.org/)) — imputes based on nearest neighbors in feature space, borrowing strength from correlated variables.

2. DRF (via [H2O](https://docs.h2o.ai/h2o/latest-stable/h2o-docs/welcome.html)) — uses H2O’s Distributed Random Forests to learn predictive models for missingness, allowing imputations that respect nonlinearities and interactions in the data.

Naïve imputation methods like mean, median, or mode could be used because they are simple and fast — but they are also extremely limited. Each missing value is replaced with the same constant number per column, which:

- distorts the natural variance of your data
- ignores relationships between variables
- can bias downstream models and analysis



































