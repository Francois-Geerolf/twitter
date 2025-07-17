# Inflation Juillet 2021-2024: IPCH, énergie, alimentation
# https://twitter.com/FrancoisGeerolf/status/1832855610448048625
library(tidyverse)
library(scales)
library(rsdmx)

# ---- Paramètres ----

idbank_codes <- c("001762445", "001762847", "001759971")
start_date <- as.Date("2021-07-01")

# ---- Construction de l'URL ----

url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(idbank_codes, collapse = "+")
)

# ---- Import et transformation des données ----

data <- url |>
  readSDMX() |>
  as_tibble() |>
  mutate(
    date = as.Date(paste0(TIME_PERIOD, "-01")),
    OBS_VALUE = as.numeric(OBS_VALUE)
  ) |>
  group_by(date) |>
  filter(n() == 3) |>  # Garder uniquement les dates où les 3 séries sont présentes
  ungroup() |>
  filter(date >= start_date) |>
  mutate(
    Coicop2016 = str_extract(TITLE_FR, "[^-]+$") |> str_trim()
  ) |>
  arrange(date) |>
  group_by(Coicop2016) |>
  mutate(
    OBS_VALUE = 100 * OBS_VALUE / OBS_VALUE[1]
  ) |>
  ungroup()

# ---- Graphique ----

ggplot(data) +
  geom_line(aes(x = date, y = OBS_VALUE, color = Coicop2016), size = 1) +
  geom_label(
    data = data |> filter(date == max(date, na.rm = TRUE)),
    aes(x = date, y = OBS_VALUE, label = round(OBS_VALUE, 1), color = Coicop2016),
    fontface = "plain", size = 3, show.legend = FALSE
  ) +
  scale_x_date(
    breaks = seq(start_date, max(data$date), by = "2 months"),
    labels = date_format("%b %Y")
  ) +
  scale_y_log10(
    breaks = seq(80, 200, by = 2),
    labels = dollar_format(accuracy = 1, prefix = "")
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.28, 0.87),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

