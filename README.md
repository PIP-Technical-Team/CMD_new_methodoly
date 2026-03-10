# CMD New Methodology

Estimate welfare distributions for missing-data (CMD) countries and save them as `.fst` files.

## How to Run

1. Open the project in RStudio/Positron.
2. Set the environment variable `PIPAPI_DATA_ROOT_FOLDER_LOCAL` to the root of your PIP data folder.
3. Open `R/estimate_dist.R` and update:
   - `release` — the release tag (e.g. `"20260324_2021_01_02_PROD"`).
   - The `load_coeff(branch = ...)` call — set the correct GitHub branch for coefficients.
   - `dir_dist_stats` — path where distribution stats are saved.
4. Source the file: `source("R/estimate_dist.R")`.

The script will:
- Load coefficients from GitHub and auxiliary population/missing-data files.
- Compute quantile-based welfare distributions for every CMD country-year.
- Scale weights to match auxiliary population totals.
- Write per-country `.fst` files to the lineup data folder.
- Combine CMD and lineup-data (LD) distribution stats and save them.

## Key Files

### `R/utils.R`

Core functions for the CMD pipeline:

| Function | Purpose |
|---|---|
| `estimate_and_write_full_cmd()` | Main loop — estimates, scales, and writes distributions country by country. |
| `get_cmd_welfare()` | Computes welfare vector for a single country-year from coefficients and quantiles. |
| `list_cmd_welfare()` | Wraps `get_cmd_welfare()` over all rows in the missing-data table. |
| `add_cmd_attributes()` | Attaches metadata (dist stats, lineup approach) as attributes. |
| `write_cmd_dist()` | Writes each distribution to an `.fst` file. |
| `load_coeff()` | Downloads coefficient data from GitHub. |
| `calc_quantiles()` | Generates logit-transformed quantile vector of length *n*. |
| `get_csum_dist()` | Computes cumulative weight/welfare sums used downstream. |

### `R/scale_weights.R`

| Function | Purpose |
|---|---|
| `get_pop_to_scale()` | Loads and reshapes auxiliary population data (`pop.fst`). |
| `scale_weights()` | Compares the sum of distribution weights to population totals and rescales where they differ. |
