# Naissances et décès: chiffres sur la France Entière
# https://twitter.com/FrancoisGeerolf/status/1917855106449448963

source("_rinit.R")

# ---- Paramètres ----

idbank_codes <- c("001641603", "001641601")

# ---- Construction de l’URL ----

url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(idbank_codes, collapse = "+")
)

# ---- Import et traitement des données ----

data <- url |>
  readSDMX() |>
  as_tibble() |>
  mutate(
    date = as.Date(paste0(TIME_PERIOD, "-01")),
    OBS_VALUE = as.numeric(OBS_VALUE),
    DEMOGRAPHIE2 = if_else(str_detect(TITLE_FR, "décès"), "Décès", "Naissances")
  ) |>
  group_by(TITLE_FR) |>
  arrange(date) |>
  mutate(
    OBS_VALUE = rollsum(OBS_VALUE, k = 12, align = "right", fill = NA)
  ) |>
  ungroup() |>
  arrange(desc(date)) |>
  select(date, OBS_VALUE, TITLE_FR, DEMOGRAPHIE2)

# ---- Graphique ----

ggplot(data) +
  geom_line(aes(x = date, y = OBS_VALUE, color = TITLE_FR), size = 1) +
  scale_color_viridis_d(option = "D", end = 0.7) +
  geom_label(
    data = data |> filter(date == as.Date("2000-01-01")),
    aes(x = date, y = OBS_VALUE, label = DEMOGRAPHIE2, color = TITLE_FR),
    size = 3, show.legend = FALSE
  ) +
  geom_label_repel(
    data = data |> filter(date == max(date, na.rm = TRUE)),
    aes(x = date, y = OBS_VALUE, label = format(round(OBS_VALUE), big.mark = " "), color = TITLE_FR),
    size = 3, show.legend = FALSE
  ) +
  scale_x_date(
    breaks = seq(1880, 2100, 5) |> paste0("-01-01") |> as.Date(),
    labels = date_format("%Y")
  ) +
  scale_y_continuous(
    breaks = seq(0, 1e6, 20000),
    labels = ~ format(.x, big.mark = " ")
  ) +
  labs(
    x = NULL,
    y = "Nombre sur les 12 derniers mois\nFrance Entière",
    title = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none"
  )
