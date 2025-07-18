# Hausse des prix dans la grande distribution (Octobre 2021 -)
# https://twitter.com/FrancoisGeerolf/status/1826530570236469418
library(tidyverse)
library(rsdmx)
library(scales)
library(lubridate)

# ---- Paramètres ----

idbank_codes <- c("001768737", "001768738", "001768739", "001768740")
start_date <- as.Date("2021-10-01")

# ---- Construction de l’URL ----

url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(idbank_codes, collapse = "+")
)

# ---- Import et traitement ----

data <- url |>
  readSDMX() |>
  as_tibble() |>
  mutate(
    date = as.Date(paste0(TIME_PERIOD, "-01")),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) |>
  filter(date >= start_date) |>
  mutate(
    `Forme-Vente` = str_extract(TITLE_FR, "[^-]+$") |> str_trim()
  ) |>
  arrange(date) |>
  group_by(`Forme-Vente`) |>
  mutate(
    OBS_VALUE = 100 * OBS_VALUE / OBS_VALUE[1]
  ) |>
  ungroup()

# ---- Graphique ----

ggplot(data) +
  geom_line(aes(x = date, y = OBS_VALUE, color = `Forme-Vente`), size = 1) +
  scale_y_continuous(
    breaks = seq(0, 500, 2),
    labels = ~ percent(.x / 100 - 1)
  ) +
  scale_x_date(
    breaks = seq(start_date, max(data$date, na.rm = TRUE), by = "2 months"),
    labels = date_format("%b %Y")
  ) +
  labs(
    x = NULL,
    y = "Augmentation vs. Octobre 2021"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.25, 0.8),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

