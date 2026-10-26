# 05_robustness.R
# (a) Sensitivity of the medians to the four cleaning bounds, one at a time.
# (b) Which side (low / high) the price/m2 bounds remove, and what those sales look like.
# (c) December check: monthly number of sales at each stage of the funnel.
#
# Input : data/clean/dvf_31_2025_logements_avant_bornes.csv  (written by 03_clean.R)
#         data/clean/dvf_31_2025.csv
#         output/tables/exclusions_03_clean.csv
# Output: output/tables/robustesse_parametres.csv
#         output/tables/bornes_prix_m2_exclus.csv
#         output/tables/ventes_par_mois_et_etape.csv

source(here::here("R", "00_setup.R"))

path_avant  <- here("data", "clean", "dvf_31_2025_logements_avant_bornes.csv")
path_funnel <- here("output", "tables", "exclusions_03_clean.csv")

avant  <- read_csv(path_avant,
                   col_types = cols(code_commune = col_character(),
                                    code_postal = col_character(),
                                    code_departement = col_character()))
funnel <- read_csv(path_funnel, show_col_types = FALSE)

# --- (a) Sensitivity to the bounds -------------------------------------------
# Applying the four bounds to the table saved before them gives the same table
# as the funnel in 03_clean.R (filters commute). The baseline is checked below.
apply_bounds <- function(df, prix_min, surface_max, prix_m2_min, prix_m2_max) {
  df |> filter(valeur_fonciere >= prix_min,
               surface_reelle_bati <= surface_max,
               prix_m2 >= prix_m2_min,
               prix_m2 <= prix_m2_max)
}

n_expected <- funnel |> slice_tail(n = 1) |> pull(n_ventes)
base <- apply_bounds(avant, prix_min, surface_max, prix_m2_min, prix_m2_max)
stopifnot(nrow(base) == n_expected)

summarise_scenario <- function(df) {
  bind_rows(
    df |> group_by(type_local) |>
      summarise(n_ventes = n(), prix_m2_median = median(prix_m2), .groups = "drop"),
    df |> summarise(type_local = "Ensemble", n_ventes = n(),
                    prix_m2_median = median(prix_m2))
  )
}

# One parameter changes, the other three stay at their baseline value (00_setup.R).
grid <- bind_rows(
  tibble(parametre = "baseline",    valeur = NA_real_),
  tibble(parametre = "prix_min",    valeur = c(1000, 5000, 20000, 30000)),
  tibble(parametre = "surface_max", valeur = c(250, 600)),
  tibble(parametre = "prix_m2_min", valeur = c(100, 500, 800)),
  tibble(parametre = "prix_m2_max", valeur = c(8000, 15000))
)

run_one <- function(parametre, valeur) {
  p <- list(prix_min = prix_min, surface_max = surface_max,
            prix_m2_min = prix_m2_min, prix_m2_max = prix_m2_max)
  if (parametre != "baseline") p[[parametre]] <- valeur
  df <- apply_bounds(avant, p$prix_min, p$surface_max, p$prix_m2_min, p$prix_m2_max)
  summarise_scenario(df) |> mutate(parametre = parametre, valeur = valeur, .before = 1)
}

res <- pmap(grid, run_one) |> list_rbind()

base_ref <- res |>
  filter(parametre == "baseline") |>
  select(type_local, n_base = n_ventes, med_base = prix_m2_median)

res <- res |>
  left_join(base_ref, by = "type_local") |>
  mutate(ecart_n = n_ventes - n_base,
         ecart_median_pct = round(100 * (prix_m2_median / med_base - 1), 2)) |>
  select(-n_base, -med_base)

print(res, n = Inf)
write_csv(res, here("output", "tables", "robustesse_parametres.csv"))

# --- (b) Sales removed by the price/m2 bounds ---------------------------------
# Same population as the last step of the funnel: single-dwelling sales that pass
# the price and surface filters, then split by side of the price/m2 bounds.
bornes <- avant |>
  filter(valeur_fonciere >= prix_min, surface_reelle_bati <= surface_max) |>
  mutate(cote = case_when(prix_m2 < prix_m2_min ~ "sous le plancher",
                          prix_m2 > prix_m2_max ~ "au-dessus du plafond",
                          TRUE ~ "conserve"))

# The excluded sales must match the last step of the funnel exactly.
stopifnot(sum(bornes$cote != "conserve") == funnel |> slice_tail(n = 1) |> pull(excluded))

bornes_resume <- bornes |>
  filter(cote != "conserve") |>
  group_by(cote, type_local) |>
  summarise(n_ventes = n(),
            prix_median = median(valeur_fonciere),
            surface_mediane = median(surface_reelle_bati),
            prix_m2_median = median(prix_m2),
            .groups = "drop")

print(bornes_resume)
write_csv(bornes_resume, here("output", "tables", "bornes_prix_m2_exclus.csv"))

# --- (c) December check ---------------------------------------------------------
dvf_31 <- read_csv(path_dep31, col_types = dvf_col_types)

ventes_par_mois <- function(df, etape) {
  df |>
    distinct(id_mutation, date_mutation) |>
    mutate(mois = floor_date(date_mutation, "month")) |>
    count(mois, name = "n_ventes") |>
    mutate(etape = etape)
}

etapes <- bind_rows(
  ventes_par_mois(dvf_31, "1_toutes_mutations"),
  ventes_par_mois(filter(dvf_31, nature_mutation == "Vente"), "2_nature_vente"),
  ventes_par_mois(filter(dvf_31, nature_mutation == "Vente",
                         type_local %in% c("Maison", "Appartement")), "3_avec_logement"),
  ventes_par_mois(base, "4_echantillon_final")
)

etapes_large <- etapes |> pivot_wider(names_from = etape, values_from = n_ventes) |> arrange(mois)
print(etapes_large, n = Inf)
write_csv(etapes_large, here("output", "tables", "ventes_par_mois_et_etape.csv"))

# December relative to the average of January-November, per stage.
indice_dec <- etapes |>
  group_by(etape) |>
  summarise(decembre_sur_moyenne_jan_nov =
              round(n_ventes[mois == as.Date("2025-12-01")] /
                      mean(n_ventes[mois < as.Date("2025-12-01")]), 2))
print(indice_dec)

# Mix of transaction natures, December vs the other months.
nature_mix <- dvf_31 |>
  distinct(id_mutation, date_mutation, nature_mutation) |>
  mutate(periode = if_else(month(date_mutation) == 12, "decembre", "jan_nov")) |>
  count(periode, nature_mutation) |>
  group_by(periode) |>
  mutate(part_pct = round(100 * n / sum(n), 1)) |>
  ungroup()
print(nature_mix, n = Inf)

write_csv(nature_mix, here("output", "tables", "nature_mutation_decembre.csv"))
