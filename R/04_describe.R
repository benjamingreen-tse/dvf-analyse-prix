# 04_describe.R
# Descriptive statistics and figures: price per m2 of housing (houses and
# flats) sold in Haute-Garonne in 2025.
#
# Input : data/clean/dvf_31_2025_logements.csv   (one row per sale)
# Output: output/tables/  and  output/figures/

source(here::here("R", "00_setup.R"))

path_logements <- here("data", "clean", "dvf_31_2025_logements.csv")

# Codes are identifiers, never computed on: read as character, so a leading
# zero (e.g. departement 01) would survive. The other columns are guessed
# correctly (numbers, a date, text).
logements_col_types <- cols(
  code_commune     = col_character(),
  code_postal      = col_character(),
  code_departement = col_character()
)

logements <- read_csv(path_logements, col_types = logements_col_types)

# --- Checks on the input ----------------------------------------------------
# Expected size = last line of the exclusion funnel written by 03_clean.R,
# so the check follows any change of bounds without a hard-coded number.
path_funnel <- here("output", "tables", "exclusions_03_clean.csv")
n_expected  <- tail(read_csv(path_funnel, show_col_types = FALSE)$n_ventes, 1)

glimpse(logements)
stopifnot(
  nrow(logements) == n_expected,
  n_distinct(logements$id_mutation) == nrow(logements),
  is.character(logements$code_commune)
)

# Zone: Toulouse (INSEE code 31555) vs the rest of the departement. Prices and
# the type of sales differ a lot between the two, so every comparison below is
# also read within zone before any conclusion (composition effect).
logements <- logements |>
  mutate(zone = if_else(code_commune == "31555", "Toulouse", "Rest of the 31"))

source_caption <- "Source: DVF géolocalisées (Etalab), 2025. Own calculations."

# --- 1. Price per m2 by type of dwelling ------------------------------------
path_table_type      <- here("output", "tables", "prix_m2_par_type.csv")
path_table_zone_type <- here("output", "tables", "prix_m2_par_zone_type.csv")

# Median, not mean: prix_m2 is right-skewed (a few very expensive sales pull
# the mean up), and the median is the price of the "middle" sale, so it is
# robust to the outliers that survive the 300-10,000 EUR/m2 filter.
prix_par_type <- logements |>
  group_by(type_local) |>
  summarise(
    n_ventes        = n(),
    prix_m2_q1      = quantile(prix_m2, 0.25, names = FALSE),
    prix_m2_median  = median(prix_m2),
    prix_m2_q3      = quantile(prix_m2, 0.75, names = FALSE),
    surface_mediane = median(surface_reelle_bati),
    .groups = "drop"
  ) |>
  mutate(across(c(prix_m2_q1, prix_m2_median, prix_m2_q3), round)) |>
  arrange(desc(n_ventes))

stopifnot(sum(prix_par_type$n_ventes) == nrow(logements))   # no sale lost
print(prix_par_type)
write_csv(prix_par_type, path_table_type)

# Same table within zone: the flat-vs-house gap is read here, not above.
prix_par_zone_type <- logements |>
  group_by(zone, type_local) |>
  summarise(
    n_ventes        = n(),
    prix_m2_median  = round(median(prix_m2)),
    surface_mediane = median(surface_reelle_bati),
    .groups = "drop"
  )

stopifnot(sum(prix_par_zone_type$n_ventes) == nrow(logements))
print(prix_par_zone_type)
write_csv(prix_par_zone_type, path_table_zone_type)

# --- 2. Distribution of price per m2 by type --------------------------------
path_fig_dist <- here("output", "figures", "prix_m2_distribution_par_type.png")

fig_dist <- ggplot(logements, aes(x = prix_m2)) +
  geom_histogram(binwidth = 250, boundary = 0, fill = "grey40") +
  geom_vline(data = prix_par_type, aes(xintercept = prix_m2_median),
             linetype = "dashed", colour = "firebrick") +
  facet_wrap(~ type_local, ncol = 1) +
  coord_cartesian(xlim = c(0, 10000)) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    title    = "Price per m2 of housing sold in Haute-Garonne, 2025",
    subtitle = paste0("One observation per sale (n = ",
                      format(nrow(logements), big.mark = ","),
                      "). Dashed line: median."),
    x        = "Price per m2 (EUR)",
    y        = "Number of sales",
    caption  = source_caption
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot")

print(fig_dist)
ggsave(path_fig_dist, fig_dist, width = 8, height = 6, dpi = 300)

# --- 3. Median price per m2 by commune --------------------------------------
# A median is only defendable with enough sales. n_min = 30 is my choice:
# below it, a few atypical sales move the median too much.
n_min <- 30
path_table_commune <- here("output", "tables", "prix_m2_par_commune.csv")
path_fig_commune   <- here("output", "figures", "prix_m2_par_commune.png")

prix_par_commune <- logements |>
  group_by(code_commune, nom_commune) |>
  summarise(
    n_ventes       = n(),
    prix_m2_q1     = round(quantile(prix_m2, 0.25, names = FALSE)),
    prix_m2_median = round(median(prix_m2)),
    prix_m2_q3     = round(quantile(prix_m2, 0.75, names = FALSE)),
    .groups = "drop"
  ) |>
  arrange(desc(n_ventes))

stopifnot(sum(prix_par_commune$n_ventes) == nrow(logements))
write_csv(prix_par_commune, path_table_commune)

# How many communes pass the threshold, and what share of sales they cover
prix_par_commune |>
  summarise(n_communes = n(),
            n_ok = sum(n_ventes >= n_min),
            part_ventes_ok = sum(n_ventes[n_ventes >= n_min]) / sum(n_ventes))

top15 <- prix_par_commune |>
  filter(n_ventes >= n_min) |>
  slice_head(n = 15) |>
  mutate(label = paste0(nom_commune, " (", n_ventes, ")"))

fig_commune <- ggplot(top15, aes(y = fct_reorder(label, prix_m2_median))) +
  geom_pointrange(aes(x = prix_m2_median, xmin = prix_m2_q1, xmax = prix_m2_q3)) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    title    = "Price per m2 in the 15 most active communes, Haute-Garonne 2025",
    subtitle = "Dot: median. Bar: Q1 to Q3. Number of sales in brackets.",
    x = "Price per m2 (EUR)", y = NULL,
    caption  = source_caption
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot")

print(fig_commune)
ggsave(path_fig_commune, fig_commune, width = 8, height = 6, dpi = 300)

# --- 4. Monthly median price per m2 and number of sales ----------------------
path_table_mois      <- here("output", "tables", "prix_m2_par_mois.csv")
path_table_mois_zone <- here("output", "tables", "prix_m2_par_mois_zone.csv")
path_fig_mois        <- here("output", "figures", "prix_m2_par_mois.png")
path_fig_mois_zone   <- here("output", "figures", "prix_m2_par_mois_zone.png")

# date_mutation is the date of the deed. Months are labelled by their first day.
logements <- logements |> mutate(mois = floor_date(date_mutation, "month"))

prix_par_mois <- logements |>
  group_by(mois) |>
  summarise(
    n_ventes       = n(),
    prix_m2_q1     = round(quantile(prix_m2, 0.25, names = FALSE)),
    prix_m2_median = round(median(prix_m2)),
    prix_m2_q3     = round(quantile(prix_m2, 0.75, names = FALSE)),
    .groups = "drop"
  )

stopifnot(
  nrow(prix_par_mois) == 12,                       # all 12 months present
  sum(prix_par_mois$n_ventes) == nrow(logements)   # no sale lost
)
print(prix_par_mois)
write_csv(prix_par_mois, path_table_mois)

# Same table within zone, plus the share of Toulouse in each month's sales.
prix_par_mois_zone <- logements |>
  group_by(mois, zone) |>
  summarise(n_ventes = n(),
            prix_m2_median = round(median(prix_m2)),
            .groups = "drop") |>
  group_by(mois) |>
  mutate(part_ventes = n_ventes / sum(n_ventes)) |>
  ungroup()

stopifnot(
  nrow(prix_par_mois_zone) == 24,                  # 12 months x 2 zones
  sum(prix_par_mois_zone$n_ventes) == nrow(logements)
)
print(prix_par_mois_zone, n = 24)
write_csv(prix_par_mois_zone, path_table_mois_zone)

# Figure A: price (top) and volume (bottom), whole departement.
# The price axis does not start at 0, so the Q1-Q3 band is drawn to keep the
# scale readable: the monthly median moves little compared with the spread.
niveaux <- c("Median price per m2 (EUR)", "Number of sales")

mois_long <- bind_rows(
  prix_par_mois |>
    transmute(mois, indicateur = niveaux[1], valeur = prix_m2_median,
              ymin = prix_m2_q1, ymax = prix_m2_q3),
  prix_par_mois |>
    transmute(mois, indicateur = niveaux[2], valeur = n_ventes,
              ymin = NA_real_, ymax = NA_real_)
) |>
  mutate(indicateur = factor(indicateur, levels = niveaux))

# Invisible point so the volume panel starts at 0
zero_volume <- tibble(mois = min(prix_par_mois$mois), valeur = 0,
                      indicateur = factor(niveaux[2], levels = niveaux))

fig_mois <- ggplot(mois_long, aes(x = mois, y = valeur)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "grey85", na.rm = TRUE) +
  geom_line(colour = "grey30") +
  geom_point(size = 2) +
  geom_blank(data = zero_volume) +
  facet_wrap(~ indicateur, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    title    = "Monthly price per m2 and number of housing sales, Haute-Garonne 2025",
    subtitle = "Month of the deed. Price panel: line = median, band = Q1 to Q3, axis not from 0.",
    x = NULL, y = NULL,
    caption  = source_caption
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot")

print(fig_mois)
ggsave(path_fig_mois, fig_mois, width = 8, height = 6, dpi = 300)

# Figure B: same months within zone (composition check).
# If the Toulouse share moves while the zone medians stay flat, the overall
# median is stable because both zones are stable, not by cancellation.
niveaux_zone <- c("Median price per m2 (EUR)", "Toulouse share of sales (%)")

zone_long <- bind_rows(
  prix_par_mois_zone |>
    transmute(mois, zone, indicateur = niveaux_zone[1], valeur = prix_m2_median),
  prix_par_mois_zone |>
    filter(zone == "Toulouse") |>
    transmute(mois, zone, indicateur = niveaux_zone[2], valeur = 100 * part_ventes)
) |>
  mutate(indicateur = factor(indicateur, levels = niveaux_zone))

fig_mois_zone <- ggplot(zone_long, aes(x = mois, y = valeur, colour = zone)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ indicateur, ncol = 1, scales = "free_y") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  scale_colour_manual(values = c("Toulouse" = "#1f4e79", "Rest of the 31" = "#c0504d")) +
  labs(
    title    = "Monthly price per m2 by zone, Haute-Garonne 2025",
    subtitle = "Month of the deed. Price axis not from 0.",
    x = NULL, y = NULL, colour = NULL,
    caption  = source_caption
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot", legend.position = "bottom")

print(fig_mois_zone)
ggsave(path_fig_mois_zone, fig_mois_zone, width = 8, height = 6, dpi = 300)