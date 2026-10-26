# 01_import.R
# Project question: price per m2 of housing (houses and flats) in Haute-Garonne
# in 2025. This script only READS and CHECKS the data; it removes nothing.
# Filtering is in 02_filter_31.R, exclusions in 03_clean.R.
#
# Input : data/raw/full-2025.csv.gz  (national DVF géolocalisées 2025, not in Git)

source(here::here("R", "00_setup.R"))   # packages, paths, dvf_col_types

# --- 1. Import --------------------------------------------------------------
# dvf_col_types is defined in 00_setup.R. read_csv guesses types from the first
# 1000 rows only, which lost values twice:
# - lot5_surface_carrez: empty at the top, guessed logical, 1,798 values lost
# - lot*_numero: guessed numeric, values like "12C" or "4A" became NA
# Identifiers are read as character since we never compute on them.
dvf_raw <- read_csv(path_raw, col_types = dvf_col_types)

# --- 2. Parsing issues ------------------------------------------------------
problems(dvf_raw)                          # must be 0 rows

# Values recovered by the explicit types (lot1_numero: 1,174,617 -> 1,174,864)
sum(!is.na(dvf_raw$lot1_numero))
sum(!is.na(dvf_raw$lot5_surface_carrez))   # 1,385

# --- 3. Structure -----------------------------------------------------------
glimpse(dvf_raw)

# --- 4. Unit of observation -------------------------------------------------
nrow(dvf_raw)                              # 3,793,113 rows
n_distinct(dvf_raw$id_mutation)            # 1,384,341 mutations

# Rows per mutation (530,325 have 1 row; the rest have 2 or more)
dvf_raw |>
  count(id_mutation, name = "n_rows") |>
  count(n_rows) |>
  slice_head(n = 10)

# Is the price repeated on every row of a mutation? n_values = 1 for all.
dvf_raw |>
  group_by(id_mutation) |>
  summarise(n_values = n_distinct(valeur_fonciere), .groups = "drop") |>
  count(n_values)

# Several dispositions per mutation? 6,832 have more than one, but the price
# is still a single value per id_mutation.
dvf_raw |>
  group_by(id_mutation) |>
  summarise(n_disp = n_distinct(numero_disposition), .groups = "drop") |>
  count(n_disp)

# One row = one component (parcel or local) of a mutation, not a sale.
# A sale is identified by id_mutation (1,384,341 sales for 3,793,113 rows).
# valeur_fonciere is the price of the WHOLE mutation, repeated on every row.
# Consequence: mean(valeur_fonciere) over rows counts a sale as many times as
# it has rows (~2.7 on average) and overweights large multi-parcel sales.
# Prices must be aggregated per id_mutation first.

# --- 5. Coverage ------------------------------------------------------------
range(dvf_raw$date_mutation)               # 2025-01-01 to 2025-12-31

# Rows per month, then sales per month. All 12 months present; December is the
# highest in both (153,653 sales vs 83,401 in August): cause unknown, open question.
dvf_raw |>
  count(month = floor_date(date_mutation, "month"))

dvf_raw |>
  distinct(id_mutation, date_mutation) |>
  count(month = floor_date(date_mutation, "month"))

# Rows per departement; 31 is 9th with 74,536 rows (rows, not sales)
dvf_raw |>
  count(code_departement, sort = TRUE) |>
  print(n = 10)

# --- 6. Suspicious values ---------------------------------------------------
dvf_raw |>
  summarise(
    na_value    = sum(is.na(valeur_fonciere)),
    min_value   = min(valeur_fonciere, na.rm = TRUE),   # 0.1 EUR: no zero, but symbolic prices
    na_surface  = sum(is.na(surface_reelle_bati)),
    big_surface = sum(surface_reelle_bati > 10000, na.rm = TRUE),
    na_coord    = sum(is.na(longitude))
  )

# Missing prices, in rows and in sales (50,484 each: one row per sale)
dvf_raw |>
  filter(is.na(valeur_fonciere)) |>
  summarise(n_rows = n(), n_mutations = n_distinct(id_mutation))

dvf_raw |> count(nature_mutation, sort = TRUE)
dvf_raw |> count(type_local, sort = TRUE)

# Symbolic prices, counted in SALES: 27 under 1 EUR, 15,383 under 100 EUR,
# 49,751 under 1,000 EUR (3.6% of sales). Not negligible.
dvf_raw |>
  distinct(id_mutation, valeur_fonciere) |>
  summarise(
    under_1   = sum(valeur_fonciere < 1, na.rm = TRUE),
    under_100 = sum(valeur_fonciere < 100, na.rm = TRUE),
    under_1k  = sum(valeur_fonciere < 1000, na.rm = TRUE)
  )

# Does a missing surface go with a missing type_local? (1,499,963 vs 1,497,725 NA)
dvf_raw |> count(type_local, surface_missing = is.na(surface_reelle_bati))

# --- 7. Decisions (applied in 03_clean.R, not here) -------------------------
# - valeur_fonciere NA (50,484 sales, 3.6%): drop from any price analysis and
#   report the count. A missing price cannot be imputed honestly.
# - Symbolic prices: drop below a threshold, to be chosen after looking at their
#   distribution by nature_mutation; document the threshold in the README.
# - surface_reelle_bati > 10,000 (623 rows): flag, exclude from price per m2.
#   Probably entry errors or atypical premises (check on a sample).
# - surface_reelle_bati NA (1.5M): not corrected. The near-identical count with
#   type_local NA suggests rows with no building (land); confirm with the
#   table above before relying on it.
# - longitude/latitude NA (44,415): keep, only needed for maps.
# - nature_mutation: keep "Vente"; treat VEFA (new-build) separately; drop
#   Echange, Adjudication, Expropriation, Vente terrain à bâtir, whose prices
#   are not comparable market prices.