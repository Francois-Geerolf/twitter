# Inflation des loyers
# https://twitter.com/FrancoisGeerolf/status/1519713704857718784
library(rsdmx)
library(tidyverse)
library(scales)
library(zoo)
library(lubridate)

# ---- Paramètres ----

idbank_code <- "001763862"
base_url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  idbank_code,
  "?startPeriod=2016"
)

# ---- Import et traitement des données ----

data <- base_url |>
  readSDMX() |>
  as_tibble() |>
  mutate(
    OBS_VALUE = as.numeric(OBS_VALUE),
    date = as.Date(paste0(TIME_PERIOD, "-01"))
  ) |>
  arrange(date) |>
  transmute(
    date,
    IRL = rollmean(OBS_VALUE, 12, fill = NA, align = "right"),
    IPC = OBS_VALUE
  ) |>
  filter(month(date) %in% c(3, 6, 9, 12)) |>
  pivot_longer(cols = c(IRL, IPC), names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  mutate(value_d12 = value / lag(value, 4) - 1) |>
  ungroup() |>
  filter(!is.na(value_d12), date >= as.Date("2008-01-01"))

# ---- Graphique ----

ggplot(data) +
  geom_line(aes(x = date, y = value_d12, color = variable), size = 1) +
  scale_color_manual(
    values = c("IPC" = "red", "IRL" = "blue"),
    labels = c(
      "IRL" = "Inflation IRL :\nMoyenne sur 12 mois de l’IPC hors loyers, hors tabac",
      "IPC" = "Inflation IPC hors loyers, hors tabac :\nGlissement annuel"
    )
  ) +
  scale_x_date(
    breaks = seq(2008, year(Sys.Date()), 1) |> paste0("-01-01") |> as.Date(),
    labels = date_format("%Y")
  ) +
  scale_y_continuous(
    breaks = seq(-1, 3, 0.005),
    labels = percent_format(accuracy = 0.1, prefix = "")
  ) +
  labs(
    x = NULL,
    y = "Inflation (glissement annuel)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.45, 0.9),
    legend.title = element_blank(),
    legend.key.size = unit(1, "cm")
  )

