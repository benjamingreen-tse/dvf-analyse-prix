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