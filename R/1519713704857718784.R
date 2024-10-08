# Inflation des loyers
# https://twitter.com/FrancoisGeerolf/status/1519713704857718784

library("rsdmx")
library("tidyverse")
library("scales")
library("zoo")

data <- "001763862" |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", a = _, "?startPeriod=2016") |>
  readSDMX() |>
  as_tibble() |>
  mutate(OBS_VALUE = OBS_VALUE |> as.numeric(),
         date =  paste0(TIME_PERIOD, "-01") |>  as.Date()) |>
  arrange(date) |>
  transmute(date,
            `Inflation Indice de Référence des Loyers (IRL):\nVariation sur 12 mois de la moyenne sur 12 mois de l'IPC hors loyers, hors tabac` =
              zoo::rollmean(OBS_VALUE, 12, fill = NA, align = "right"),
            `Inflation Indice des Prix à la Consommation (IPC-) hors loyers, hors tabac: \nVariation sur 12 mois de l'IPC hors loyers, hors tabac` =
              OBS_VALUE) |>
  filter(month(date) %in% c(12, 3, 6, 9)) |>
  gather(variable, value, -date) |>
  group_by(variable) |>
  mutate(value_d12 = value/lag(value, 4) - 1) |>
  na.omit() |>
  filter(date >= as.Date("2008-01-01"))

data |>
  ggplot() + ylab("Inflation") + xlab("") + theme_minimal() +
  geom_line(aes(x = date, y = value_d12, color = variable)) +
  scale_color_manual(values = c("blue", "red")) +
  scale_x_date(breaks = seq(1920, 2025, 2) |> paste0("-01-01") |> as.Date(),
               labels = date_format("%Y")) +
  theme(legend.position = c(0.45, 0.9),
        legend.title = element_blank(),
        legend.key.size= unit(1.0, 'cm')) +
  scale_y_continuous(breaks = 0.01*seq(-100, 300, 0.5),
                     labels = percent_format(accuracy = .1, prefix = ""))
