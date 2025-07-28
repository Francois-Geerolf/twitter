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
  geom_line(aes(x = date, y = Valeur, linetype = Indice, color = Metier), size = 1) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray40") +
  geom_label_repel(
    data = data %>% filter(date == max(date)),
    aes(x = date, y = Valeur, color = Metier,
        label = percent(Valeur / 100 - 1, accuracy = 0.1, style_positive = "plus")),
    size = 3, fontface = "bold", show.legend = FALSE,
    box.padding = 0.35, point.padding = 0.2, max.overlaps = Inf
  ) +
  scale_color_viridis_d(option = "D", end = 0.85) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.1))
  ) +
  scale_y_continuous(
    breaks = seq(80, 140, 2),
    labels = function(x) paste0(x, " (", percent(x / 100 - 1, accuracy = 1), ")")
  ) +
  labs(
    title = "Pouvoir d'achat des salaires par catégorie socioprofessionnelle",
    subtitle = "Déflaté par l'IPC ou l'IPCH (base 100 en 1996)",
    y = NULL, x = NULL,
    caption = "Source : INSEE, calculs @FrancoisGeerolf"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = c(0.3, 0.75),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

