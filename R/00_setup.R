# 00_setup.R
# Shared setup: packages, paths and column types. Reads no data.
# Called with source() at the top of every other script.

library(tidyverse)
library(here)

path_raw   <- here("data", "raw", "full-2025.csv.gz")
path_dep31 <- here("data", "clean", "dvf_31_2025.csv")

# Explicit types: read_csv guesses from 1000 rows only and loses values
# (lot5_surface_carrez, lot*_numero with letters). Identifiers as character.
# Must also be used when re-reading the CSVs written by our scripts.
dvf_col_types <- cols(
  ancien_code_commune = col_character(),
  numero_volume       = col_character(),
  lot1_numero         = col_character(),
  lot2_numero         = col_character(),
  lot3_numero         = col_character(),
  lot4_numero         = col_character(),
  lot5_numero         = col_character(),
  lot5_surface_carrez = col_double()
)

# --- Parameters -------------------------------------------------------------
prix_min    <- 10000   # EUR. Only ~40 single-dwelling sales fall below it, and the
# median price/m2 moves by 0.2% between 1,000 and 30,000 EUR.
surface_max <- 400     # m2. Above: estates, châteaux, data-entry errors.
prix_m2_min <- 300     # EUR/m2. Below: not a market price for a dwelling
# (ruin, share of a property, entry error).
# Sensitivity: moving the floor from 500 to 300 adds 74 sales (55 houses,
# 19 flats); median price/m2 changes by less than 0.2%.
prix_m2_max <- 10000   # EUR/m2. Median of the departement is ~2,800 EUR/m2.
# The last three bounds come from market orders of magnitude, not from a
# check on the data: the funnel in 03_clean.R shows how many sales each one removes,
# and 05_robustness.R shows how much the medians move when they change.
