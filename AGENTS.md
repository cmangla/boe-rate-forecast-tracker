# AGENTS.md — `boe-rate-forecast-tracker`

Repository-specific context and runbook for coding agents maintaining this repository.

## Project Scope & Architecture

This repository tracks **UK Bank of England (BoE) Official Bank Rate** forecasts (ING Think & Scotiabank Economics) against realized MPC policy decisions, and renders econometric `ggplot2` visualizations displayed in `README.md`.

* **Other countries/metrics** (GDP, CPI, FX, commodities, 3-month interbank rates) are archived in the raw root `*.html` / `*.csv` files by `table-scrape.py`, **but must be ignored** by the BoE rate pipeline and plots.
* **Two-stage CI commit model** (`.github/workflows/scrape.yml`):
  1. `wget` + `uv run python table-scrape.py` runs first. If no root `*.csv` changed, noisy `*.html` diffs are discarded and the workflow exits cleanly.
  2. If any root `*.csv` changed, raw `*.html` + `*.csv` are **committed and pushed first**—so raw evidence is preserved in git even if step 3 fails.
  3. `uv run python scripts/update_boe_tables.py` validates strict upstream schemas and appends new UK records to `data/boe_forecasts.csv` and `data/boe_base_rate.csv`. On schema drift it emits `::error::` and exits `1` (triggering a GitHub Actions failure email).
  4. `Rscript scripts/generate_plots.R` runs only when `data/` changed (or on `workflow_dispatch`), reading **exclusively** from `data/*.csv` and writing `.svg` + 300-DPI `.png` files to `plots/`.

## Checked-In Data Ledgers (`data/`)

Never overwrite or regenerate `data/` from scratch unless explicitly asked—both files are **append-only historical ledgers** (pre-populated from 1,158 commits back to Sep 2023):

* `data/boe_forecasts.csv`: `bank,forecast_date,horizon_label,horizon_type,target_date,forecast_rate,recorded_at`
  * `forecast_date`: Bank's published `"as of"` / `"Last updated"` date parsed from HTML (not the git commit date).
  * `recorded_at`: UTC date our pipeline first observed that curve.
  * Deduplicated per `(bank, forecast_date, target_date)` (preferring `quarter` over `annual` if both map to `YYYY-12-31`). New rows are appended only when a bank's `(target_date, forecast_rate)` vector changes.
* `data/boe_base_rate.csv`: `date,rate,event_type,source`
  * `event_type == "change"`: MPC rate-change effective dates (`2020-01-02` onward).
  * `event_type == "latest_verified"`: Single trailing anchor row updated only when a new MPC change occurs or `data/boe_forecasts.csv` changes.
  * Sourced from BoE IADB (`IUDBEDR`, `CSVF=TN`, browser `User-Agent`) with automatic `Bank-Rate.asp` HTML fallback.

## Playbook: Fixing an Upstream Schema Change Failure

When GitHub Actions emails a failure from `scripts/update_boe_tables.py`:

1. **Inspect the latest raw commit**: The raw `*.html` and `*.csv` files were already committed before `update_boe_tables.py` failed. Check `git log -n 3` and the `::error::` message.
2. **Known historical precedent (`2026-06-12`)**: ING removed a preceding table, shifting Policy Rates from `ing-forecasts_3.csv` to `ing-forecasts_2.csv` (leaving 3-month interbank rates in `_3.csv`).
   * **Never** blindly point `ING_CSV` at another index without verifying `US (upper bound)` / `Eurozone (deposit rate)` in col 1 (or checking `<div id="policy-rates">` in `ing-forecasts.html`).
3. **Common drift points in `scripts/update_boe_tables.py`**:
   * `ING_CSV` / `SCOTIA_CSV`: Table index shifted—inspect `ing-forecasts_*.csv` or `scotiabank-forecast-snapshot_*.csv` to locate Policy Rates.
   * `parse_ing_horizon` / `parse_scotia_horizon`: New horizon header format (note: unmarked `YYYY` columns in Scotiabank are realized history and intentionally skipped).
   * `parse_ing_banner_date` / `parse_scotia_banner_date`: Changed HTML `"Last updated:"` or `"Forecasts as of"` markup.
   * Country label renamed (`"UK"` / `"United Kingdom"`).
4. **Verify locally before pushing**:
   ```bash
   uv sync --frozen
   uv run python scripts/update_boe_tables.py
   Rscript scripts/generate_plots.R
   ```

## Playbook: Modifying Plots or Adding a Forecaster

* **R style & conventions (`scripts/generate_plots.R`)**:
  * Scientific/econometric style (`theme_bw`, explicit `n`, $\mu$, $\sigma$, Student-$t$ 95% CIs, OLS regressions with $\beta, R^2, p$-values, `ggrepel` callouts, and `ggnewscale` graphical legends for every symbol/line/ribbon/bar).
  * Keep `.github/workflows/scrape.yml` `packages:` list in sync with any new R packages added to `scripts/generate_plots.R`.
  * Outputs must be deterministic (fixed `seed` in `ggrepel`) and saved as both `.svg` (`svglite`) and 300-DPI `.png` (`ragg::agg_png`).
* **Python dependencies**: Managed via `uv` (`pyproject.toml` + `uv.lock`). Always run Python scripts via `uv run python ...` and keep `.venv/` gitignored.
