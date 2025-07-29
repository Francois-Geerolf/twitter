# Emploi Trimestriel (2016T1-)
# https://twitter.com/FrancoisGeerolf/status/1487713516127768576

source("_rinit.R")

# ---- Paramètres ----

base_year <- "2017-01-01"
idbank_codes <- c("001791539", "001791541", "010599703", "010600319")

# ---- Construction de l’URL ----

url <- glue::glue(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/{paste(idbank_codes, collapse = '+')}?startPeriod={year(base_year)}"
)

# ---- Chargement et traitement des données ----

data <- url |>
  readSDMX() |>
  as_tibble() |>
  mutate(
    OBS_VALUE = as.numeric(OBS_VALUE),
    TIME_PERIOD = as.Date(as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"))
  ) |>
  group_by(TITLE_FR) |>
  mutate(
    OBS_VALUE = 100 * OBS_VALUE / OBS_VALUE[TIME_PERIOD == as.Date(base_year)],
    TITLE_FR = TITLE_FR |>
      str_remove("^Emploi salarié en fin de trimestre - ") |>
      str_remove(" - France métropolitaine$") |>
      str_remove(" \\([^\\)]+\\)$") # Enlever ce qui est derrière la parenthèse
  ) |>
  ungroup() |>
  select(TIME_PERIOD, OBS_VALUE, TITLE_FR)

# ---- Construction du graphique ----

ggplot(data) +
  geom_line(aes(x = TIME_PERIOD, y = OBS_VALUE, color = TITLE_FR), size = 1) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "grey50") +
  scale_x_date(
    breaks = seq(2016, 2100, 1) |> paste0("-01-01") |> as.Date(),
    labels = date_format("%Y")
  ) +
  scale_y_log10(
    breaks = seq(100, 200, by = 2)
  ) +
  labs(
    x = NULL,
    y = glue::glue(
      "Emploi salarié en fin de trimestre\nFrance métropolitaine (100 = {year(base_year)}-T1)"
    )
  ) +
  theme_minimal(base_size = 13) +
  scale_color_viridis_d(option = "D", end = 0.9) +
  theme(
    legend.position = c(0.3, 0.85),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
