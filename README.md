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

- #### Install R packages required for setup
Open an R terminal (or RStudio) and install `renv` if it's not already available:

```r
install.packages("renv")
```

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

## Project Organization

```bash

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



































