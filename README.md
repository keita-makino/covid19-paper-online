# COVID-19 paper analysis

## Requirements

Use R 4.3.2. Package versions are recorded in `renv.lock`.

## Running the analysis

Run all commands from the repository root.

### 1. Prepare the input data

Obtain the following files through the approved access process:

- `mm.sav` (mail-out / mail-back survey)
- `altogether_cleaned.rds` (2021 online survey)

Create a `.env` file at the repository root (see `.env.example`) pointing at the
two analysis inputs:

```sh
DATA_2021_PATH=/path/to/altogether_cleaned.rds
DATA_MM_PATH=/path/to/mm.sav
```

Relative paths are resolved against the directory containing the `.env` file.
`src/preparation/export_cs.R` is not run by the entry point. If it is run manually,
it expects `cs.sav` in `src/data/`.

The `.env` file and the `src/data/` directory are excluded from Git. Do not commit raw data.

### 2. Restore the packages

Install `renv` if it is not already available:

```sh
Rscript --vanilla -e "install.packages('renv', repos = 'https://cloud.r-project.org')"
```

Restore the package versions recorded in `renv.lock`:

```sh
Rscript --vanilla -e "renv::restore(prompt = FALSE)"
```

### 3. Run the code

```sh
Rscript --vanilla -e "renv::load(); source('src/index.R')"
```

`src/index.R` runs the preparation and analysis scripts in dependency order. The scripts share objects created by earlier steps, so use `src/index.R` as the entry point rather than running individual files.

## Outputs

All generated files are written under `output/`. Numeric tables are written as tidy CSV files with matching programmatically generated LaTeX fragments. Regression outputs are stored in `output/lr/`, and statistical test plots are stored in `output/test/`. The `output/` directory is not tracked in Git.

## Project structure

- `src/index.R`: entry point and execution order
- `src/preparation/`: raw data import, cleaning, and creation of analysis-ready data
- `src/analysis/`: factor analysis, statistical tests, regression, and plots

## Updating dependencies

After changing a package dependency, run the following command from the repository root and commit the updated `renv.lock`:

```sh
Rscript --vanilla -e "renv::snapshot(prompt = FALSE)"
```
