# Salaires des cadres
# Source du graphique : https://twitter.com/FrancoisGeerolf/status/1938858533727916434

source("_rinit.R")

# ---- Paramètres ----

idbank_codes <- list(
  salaires = c("010752321", "010752324", "010752330"),
  prix     = c("001762489", "001764363")
)

labels_salaires <- c(
  "Cadres y compris chefs d'entreprise",
  "Professions intermédiaires",
  "Ouvriers"
)

labels_prix <- c("IPC", "IPCH")

# ---- Fonction d'import et traitement ----

read_and_transform <- function(url, expected_n, labels) {
  url |>
    readSDMX() |>
    as_tibble() |>
    mutate(
      date = as.Date(paste0(TIME_PERIOD, "-01-01")),
      OBS_VALUE = as.numeric(OBS_VALUE)
    ) |>
    group_by(date) |>
    filter(n() == expected_n) |>
    ungroup() |>
    mutate(Label = case_when(
      grepl("Cadres y compris chefs d'entreprise", TITLE_FR) ~ "Cadres y compris chefs d'entreprise",
      grepl("Professions intermédiaires", TITLE_FR) ~ "Professions intermédiaires",
      grepl("Ouvriers", TITLE_FR) ~ "Ouvriers",
      grepl("harmonisé", TITLE_FR) ~ "IPCH",
      TRUE ~ "IPC"
    )) |>
    group_by(Label) |>
    arrange(date) |>
    mutate(OBS_VALUE = 100 * OBS_VALUE / first(OBS_VALUE)) |>
    ungroup() |>
    select(date, OBS_VALUE, Label)
}

# ---- Construction des URLs ----

make_url <- function(idbanks) {
  paste0(
    "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
    paste(idbanks, collapse = "+")
  )
}

# ---- Données ----

data_salaires <- read_and_transform(make_url(idbank_codes$salaires), 3, labels_salaires)
data_prix     <- read_and_transform(make_url(idbank_codes$prix), 2, labels_prix) %>%
  pivot_wider(names_from = Label, values_from = OBS_VALUE)

# ---- Fusion et calcul des salaires réels ----

data <- data_salaires %>%
  rename(Metier = Label) %>%
  left_join(data_prix, by = "date") %>%
  arrange(date) %>%
  transmute(
    date, Metier,
    `Salaire déflaté par l'indice IPCH harmonisé par Eurostat` = IPCH[1] * OBS_VALUE / IPCH,
    `Salaire déflaté par l'indice IPC` = IPC[1] * OBS_VALUE / IPC
  ) %>%
  pivot_longer(cols = -c(date, Metier), names_to = "Indice", values_to = "Valeur")

# ---- Graphique ----

ggplot(data) +
  geom_line(aes(x = date, y = Valeur, linetype = Indice, color = Metier)) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  geom_label_repel(
    data = data %>% filter(date == max(date)),
    aes(x = date, y = Valeur, color = Metier, label = percent(Valeur / 100 - 1, accuracy = 0.1, style_positive = "plus"))
  ) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_date(
    breaks = seq(1996, 2100, 2) %>% paste0("-01-01") %>% as.Date(),
    labels = date_format("%Y")
  ) +
  scale_y_log10(breaks = seq(10, 200, 1)) +
  labs(
    y = "Pouvoir d'achat du salaire (Indice 100 = 1996)",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.28, 0.8),
    legend.title = element_blank()
  )


