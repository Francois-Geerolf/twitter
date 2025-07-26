# Indice de production industrielle vs. Mai 2017
# https://twitter.com/FrancoisGeerolf/status/1816121032928874928

source("_rinit.R")

# ---- Paramètres ----

idbank_codes <- c("010768261", "010768307")
base_date <- as.Date("2017-05-01")
covid_start <- as.Date("2020-02-01")
covid_end   <- as.Date("2020-07-01")

# ---- Import des données ----

url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(idbank_codes, collapse = "+")
)

data <- url |>
  readSDMX() |>
  as_tibble() |>
  mutate(
    date = as.Date(paste0(TIME_PERIOD, "-01")),
    OBS_VALUE = as.numeric(OBS_VALUE),
    OBS_VALUE = if_else(
      year(date) == 2020 & month(date) %in% 3:6,
      NA_real_, OBS_VALUE
    ),
    Naf2 = str_extract(TITLE_FR, "(?<=- ).*(?= \\(NAF)")
  ) |>
  filter(date >= base_date) |>
  group_by(Naf2) |>
  mutate(OBS_VALUE = 100 * OBS_VALUE / OBS_VALUE[date == base_date]) |>
  ungroup() |>
  select(date, OBS_VALUE, Naf2)

# ---- Zone COVID (shading) ----

covid_shading <- tibble(
  xmin = covid_start,
  xmax = covid_end,
  ymin = 0,
  ymax = Inf
)

# ---- Graphique ----

ggplot(data) +
  # Zone grisée COVID
  geom_rect(
    data = covid_shading,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = viridis(4)[4], alpha = 0.2, inherit.aes = FALSE
  ) +
  # Courbes principales
  geom_line(aes(x = date, y = OBS_VALUE, color = Naf2), size = 1) +
  # Étiquettes de fin de série
  geom_label_repel(
    data = data |> filter(date == max(date, na.rm = TRUE)),
    aes(x = date, y = OBS_VALUE, label = percent(OBS_VALUE / 100 - 1, accuracy = 0.1), color = Naf2),
    show.legend = FALSE
  ) +
  # Ligne horizontale de référence
  geom_hline(yintercept = 100, linetype = "dashed") +
  # Axes et légendes
  scale_x_date(
    breaks = seq(2017, year(Sys.Date()), 1) |> paste0("-01-01") |> as.Date(),
    labels = date_format("%Y")
  ) +
  scale_y_log10(
    breaks = seq(90, 110, 1),
    labels = ~ percent(.x / 100 - 1, accuracy = 1)
  ) +
  #scale_color_viridis_d(option = "plasma") +
  labs(
    x = NULL,
    y = "Indice de production industrielle vs. mai 2017\n(hors mars-juin 2020)",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.7, 0.9),
    legend.title = element_blank()
  )
