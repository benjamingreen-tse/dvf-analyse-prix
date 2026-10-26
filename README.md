# DVF price analysis (Haute-Garonne)

Personal R project: import, clean, describe and visualise real-estate transaction prices, to practise a reproducible data-analysis workflow (R, tidyverse, Git, later Quarto).

## Data
Source: DVF (Demandes de valeurs foncières), published on [data.gouv.fr](https://www.data.gouv.fr). Scope: Haute-Garonne, one year (to be specified once downloaded).

The raw file is **not** in this repository. Download it manually and place it in `data/raw/` before running the scripts.

## Structure
- `data/raw/`: original files, never modified, not tracked by Git
- `data/clean/`: cleaned data produced by the scripts
- `R/`: scripts, run in numerical order (`01_import.R`, `02_clean.R`, `03_describe.R`)
- `output/`: figures and tables
- `report/`: final report (Quarto, planned)

## Status
Work in progress.
