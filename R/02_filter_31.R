# 02_filter_31.R
# Keep Haute-Garonne (departement 31) from the national 2025 file.
# This script filters only: no cleaning (that is 03_clean.R).
# Input : data/raw/full-2025.csv.gz   Output: data/clean/dvf_31_2025.csv

source(here::here("R", "00_setup.R"))

dvf_raw <- read_csv(path_raw, col_types = dvf_col_types)

# code_departement is character ("01", "2A"), hence the quotes around "31"
dvf_31 <- dvf_raw |>
  filter(code_departement == "31")

# --- Checks -----------------------------------------------------------------
n_before <- nrow(dvf_raw)
n_after  <- nrow(dvf_31)
n_before
n_after                                  # expected: 74,536
n_distinct(dvf_31$id_mutation)           # number of sales in the departement

stopifnot(
  n_after == 74536,
  n_distinct(dvf_31$code_departement) == 1,
  all(str_starts(dvf_31$code_commune, "31"))   # INSEE commune codes of the 31
)

# --- Output -----------------------------------------------------------------
write_csv(dvf_31, path_dep31)

# Re-read check: same rows after the round trip
stopifnot(nrow(read_csv(path_dep31, col_types = dvf_col_types)) == n_after)