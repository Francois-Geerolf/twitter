# Hausse du déficit de 2017 à 2024: baisse des recettes ou hausse des dépenses ?
# Source du graphique : https://twitter.com/FrancoisGeerolf/status/1945587985228980306

# Chargement des packages et paramètres initiaux (chemins, options, etc.)
source("_rinit.R")

# ---- Paramètres ----

# Code idbank de la série PIB nominal (en milliards d'euros)
idbank_codes <- c("011779992")

# Identifiant de version et nom du fichier Excel pour les données détaillées de finances publiques
version <- "8574705"
filename <- "T_3201.xlsx"

# ---- Construction de l’URL ----

# URL pour télécharger la série BDM (PIB)
url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(idbank_codes, collapse = "+")
)

# URL pour télécharger le fichier Excel complet de la comptabilité nationale
url2 <- paste(
  "https://www.insee.fr/fr/statistiques/fichier",
  version,
  filename,
  sep = "/"
)

# ---- Import et traitement du PIB ----

# Téléchargement de la série PIB via SDMX, conversion en tibble et mise au bon format
gdp <- url |>
  readSDMX() |>                   # Lecture SDMX
  as_tibble() |>                 # Conversion en tibble
  transmute(
    date = as.Date(paste0(TIME_PERIOD, "-01-01")),   # Conversion en date (1er janvier de chaque année)
    gdp = as.numeric(OBS_VALUE) / 1000               # Passage en milliards d'euros
  ) |>
  arrange(date)

# ---- Import et traitement des dépenses ----

# Création d’un fichier temporaire pour stocker le fichier Excel téléchargé
file <- tempfile()
curl_download(url2, destfile = file, quiet = FALSE)

# Lecture du fichier Excel (saut de 1 ligne d'entête)
T_3201 <- read_excel(file, skip = 1) |>
  rename(Line = ...1) |>                            # Renommer la première colonne
  filter(!is.na(`2019`)) |>                         # Supprimer les lignes vides
  mutate(line = 1:n()) |>                           # Ajouter un numéro de ligne
  gather(year, value, -Line, -line) |>              # Passage du format wide à long
  mutate(value = as.numeric(value)) |>              # Conversion des valeurs en numérique
  filter(!is.na(value))                             # Nettoyage des valeurs manquantes

# Sélection des lignes correspondant à :
# - ligne 44 : solde public
# - ligne 19 : prélèvements obligatoires (par exemple)
data <- T_3201 |>
  filter(line %in% c(44, 19)) |>                    # Filtrage sur les lignes d'intérêt
  mutate(date = as.Date(paste0(year, "-01-01"))) |> # Conversion en date
  left_join(gdp, by = "date") |>                    # Jointure avec le PIB
  filter(date >= as.Date("2017-01-01"))             # Filtrage à partir de 2017

# ---- Calcul de l’écart entre les deux indicateurs (delta) pour 2017 et 2024 ----

delta_data <- data |>
  filter(format(date, "%Y") %in% c("2017", "2024")) |> # Garde uniquement les dates 2017 et 2024
  group_by(date) |>
  summarise(
    y_max = max(value / gdp),   # Valeur maximale (% du PIB)
    y_min = min(value / gdp),   # Valeur minimale (% du PIB)
    delta = y_max - y_min,      # Écart entre les deux
    .groups = "drop"
  )

# ---- Extraction des valeurs de 2017 pour dessiner les lignes horizontales ----

y_vals_2017 <- delta_data |>
  filter(date == as.Date("2017-01-01")) |>     # Filtrage uniquement pour 2017
  select(y_max, y_min) |>
  unlist()                                     # Conversion en vecteur

# ---- Graphique final ----

ggplot(data = data) +
  geom_line(aes(x = date, y = value / gdp, color = Line), size = 1) +  # Tracé des courbes en % du PIB
  theme_minimal() +
  scale_color_viridis_d(option = "D", end = 0.7) +
  scale_x_date(
    breaks = seq(1960, 2100, 1) |> paste0("-01-01") |> as.Date(),     # Graduation annuelle
    labels = date_format("%Y")
  ) +
  scale_y_continuous(
    breaks = 0.01 * seq(0, 500, 1),
    labels = percent_format(accuracy = 1)                             # Affichage en %
  ) +
  theme(
    legend.position = c(0.2, 0.85),              # Position personnalisée de la légende
    legend.title = element_blank(),              # Pas de titre dans la légende
    legend.text = element_text(face = "bold")    # Texte en gras
  ) +
  
  # Ajout de labels pour les points extrêmes (début et fin de période)
  geom_label(
    data = data |> filter(date %in% c(max(date), min(date))),
    aes(x = date, y = value / gdp, label = percent(value / gdp, accuracy = 0.1), color = Line),
    fontface = "bold"
  ) +
  
  # Ajout de flèches verticales représentant l’écart delta entre deux lignes
  geom_segment(
    data = delta_data,
    aes(x = date, xend = date, y = y_min + 0.005, yend = y_max - 0.005),
    arrow = arrow(length = unit(0.2, "cm"), ends = "both"),
    color = "black"
  ) +
  
  # Lignes horizontales en pointillés correspondant aux valeurs de 2017
  geom_hline(yintercept = y_vals_2017, linetype = "dotted", color = "grey50") +
  
  # Affichage du delta en % au milieu de la flèche
  geom_label(
    data = delta_data,
    aes(x = date + months(6), y = (y_max + y_min) / 2,
        label = paste0("-", round(delta * 100, 1), " %")),
    color = "black",
    fontface = "bold",
    vjust = -0.5
  ) +
labs(
  title = "Hausse du déficit de 2017 à 2024",
  subtitle = "Baisse des recettes ou hausse des dépenses ?",
  y = "% du PIB", x = NULL,
  caption = "Source : INSEE, calculs @FrancoisGeerolf"
)


