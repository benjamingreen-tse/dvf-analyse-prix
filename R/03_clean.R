# 03_clean.R
# Project question: price per m2 of housing (houses and flats) sold in
# Haute-Garonne in 2025.
# Unit of output: one row per SALE (id_mutation) containing exactly one dwelling.
#
# Input : data/clean/dvf_31_2025.csv
# Output: data/clean/dvf_31_2025_logements.csv
#         output/tables/exclusions_03_clean.csv  (sales left after each step)

source(here::here("R", "00_setup.R"))

path_out    <- here("data", "clean", "dvf_31_2025_logements.csv")
path_funnel <- here("output", "tables", "exclusions_03_clean.csv")

# --- Parameters -------------------------------------------------------------
prix_min    <- 10000   # EUR. Only ~40 single-dwelling sales fall below it, and the
# median price/m2 moves by 0.2% between 1,000 and 30,000 EUR.
surface_max <- 400     # m2. Above: estates, châteaux, data-entry errors.
prix_m2_min <- 500     # EUR/m2. Below: not a market price for a dwelling
# (ruin, share of a property, entry error).
prix_m2_max <- 10000   # EUR/m2. Median of the departement is ~2,800 EUR/m2.
# The last three bounds come from market orders of magnitude, not from a
# check on the data: the funnel below shows how many sales each one removes.

# --- Import -----------------------------------------------------------------
dvf_31 <- read_csv(path_dep31, col_types = dvf_col_types)

# --- Exclusion funnel -------------------------------------------------------
# Every filter works at the level of the sale (id_mutation), not the row.
funnel <- tibble(step = "All sales in departement 31",
                 n_ventes = n_distinct(dvf_31$id_mutation))

add_step <- function(funnel, step, n) bind_rows(funnel, tibble(step = step, n_ventes = n))

# 1. Ordinary sales only. Drops exchanges, auctions, expropriations, building
#    land and VEFA (new-build, handled separately: different price logic).
ventes <- dvf_31 |> filter(nature_mutation == "Vente")
funnel <- add_step(funnel, "Nature = Vente", n_distinct(ventes$id_mutation))

# 2. Sales containing at least one house or flat.
logement_rows <- ventes |> filter(type_local %in% c("Maison", "Appartement"))
funnel <- add_step(funnel, "At least one house or flat", n_distinct(logement_rows$id_mutation))

# 3. Exactly one dwelling.
#    Definition: a dwelling = a distinct (parcel, type, surface, rooms).
#    The same dwelling can appear on several rows (one per land use), so
#    counting rows would overcount. Limitation: two identical flats on the same
#    parcel would count as one, and their price/m2 would be doubled; this shows
#    in the upper tail of prix_m2 and is handled by prix_m2_max.
n_logements <- logement_rows |>
  distinct(id_mutation, id_parcelle, type_local,
           surface_reelle_bati, nombre_pieces_principales) |>
  count(id_mutation, name = "n_logements")

ids_one <- n_logements |> filter(n_logements == 1) |> pull(id_mutation)
logement_rows <- logement_rows |> filter(id_mutation %in% ids_one)
funnel <- add_step(funnel, "Exactly one dwelling", n_distinct(logement_rows$id_mutation))

# 4. No professional premises in the same sale: one price covers both, so it
#    cannot be attributed to the dwelling.
ids_pro <- ventes |> filter(str_starts(type_local, "Local")) |> distinct(id_mutation)
logement_rows <- logement_rows |> anti_join(ids_pro, by = "id_mutation")
funnel <- add_step(funnel, "No professional premises", n_distinct(logement_rows$id_mutation))

# 5. One row per sale. valeur_fonciere is the price of the whole sale and is
#    repeated on each row, so the dedup keeps it once. Coordinates and land
#    columns differ by parcel and are dropped here.
logements <- logement_rows |>
  distinct(id_mutation, date_mutation, valeur_fonciere, type_local,
           surface_reelle_bati, nombre_pieces_principales,
           code_commune, nom_commune, code_postal, code_departement)

stopifnot(n_distinct(logements$id_mutation) == nrow(logements))

# 6. Price known.
logements <- logements |> filter(!is.na(valeur_fonciere))
funnel <- add_step(funnel, "Price not missing", nrow(logements))

# 7. No symbolic price.
logements <- logements |> filter(valeur_fonciere >= prix_min)
funnel <- add_step(funnel, paste0("Price >= ", prix_min, " EUR"), nrow(logements))

# 8. Usable surface: not missing, positive, below the ceiling.
logements <- logements |>
  filter(!is.na(surface_reelle_bati), surface_reelle_bati > 0,
         surface_reelle_bati <= surface_max)
funnel <- add_step(funnel, paste0("Surface in (0, ", surface_max, "] m2"), nrow(logements))

# 9. Price per m2, then plausibility range.
logements <- logements |>
  mutate(prix_m2 = valeur_fonciere / surface_reelle_bati) |>
  filter(prix_m2 >= prix_m2_min, prix_m2 <= prix_m2_max)
funnel <- add_step(funnel, paste0("Price/m2 in [", prix_m2_min, ", ", prix_m2_max, "]"),
                   nrow(logements))

# --- Checks -----------------------------------------------------------------
stopifnot(
  n_distinct(logements$id_mutation) == nrow(logements),
  all(is.finite(logements$prix_m2)),
  all(logements$type_local %in% c("Maison", "Appartement"))
)

# --- Outputs ----------------------------------------------------------------
funnel <- funnel |> mutate(excluded = lag(n_ventes) - n_ventes)
print(funnel)

write_csv(funnel, path_funnel)
write_csv(logements, path_out)