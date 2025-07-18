# Exportations nettes de biens, biens industriels, biens manufacturés
# https://twitter.com/FrancoisGeerolf/status/1487364702841749504

library(rsdmx)
library(tidyverse)
library(scales)
library(viridis)
library(zoo)

# Identifiants INSEE base 2020
idbanks <- c(
  biens_X = "011795478",
  biens_M = "011795520",
  biens_manuf_X = "011795480", 
  biens_manuf_M = "011795522",
  biens_indus_X = "011795482",
  biens_indus_M = "011795524",
  pib = "011794859"
)

# Import des données
url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(idbanks, collapse = "+")
)

data <- url |>
  readSDMX() |>
  as_tibble() |>
  mutate(
    OBS_VALUE = as.numeric(OBS_VALUE),
    TIME_PERIOD = as.Date(as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"))
  ) |>
  select(IDBANK, TIME_PERIOD, OBS_VALUE) |>
  pivot_wider(names_from = IDBANK, values_from = OBS_VALUE) |>
  transmute(
    TIME_PERIOD,
    `Biens` = (get(idbanks["biens_X"]) - get(idbanks["biens_M"])) / get(idbanks["pib"]),
    `Biens manufacturés` = (get(idbanks["biens_manuf_X"]) - get(idbanks["biens_manuf_M"])) / get(idbanks["pib"]),
    `Biens industriels` = (get(idbanks["biens_indus_X"]) - get(idbanks["biens_indus_M"])) / get(idbanks["pib"])
  ) |>
  pivot_longer(-TIME_PERIOD, names_to = "Cna_produit", values_to = "OBS_VALUE")

# Graphique amélioré
ggplot(data, aes(x = TIME_PERIOD, y = OBS_VALUE, color = Cna_produit)) +
  geom_line(size = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_date(
    breaks = seq(1940, year(Sys.Date()), by = 10) %>% paste0("-01-01") %>% as.Date(),
    labels = date_format("%Y")
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 0.1),
    limits = c(min(data$OBS_VALUE, na.rm = TRUE), max(data$OBS_VALUE, na.rm = TRUE))
  ) +
  scale_color_viridis_d(begin = 0.2, end = 0.9, option = "D") +
  labs(
    title = "Exportations nettes de biens en % du PIB",
    subtitle = "Données trimestrielles",
    caption = "Source : Insee (comptes trimestriels), @FrancoisGeerolf",
    x = NULL, y = NULL, color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10)),
    plot.caption = element_text(size = 9)
  )

