library(rsdmx)
library(tidyverse)
library(scales)
library(viridis)
library(zoo)

# ---- Préparation des données ----
## Base 2014 (original) ----

idbanks <- c(
  "010565588", "010565630", # Biens
  "010565590", "010565632", # Biens manufacturés
  "010565592", "010565634", # Biens industriels
  "010565707"               # PIB total
)

## Base 2020 (à jour) ----

idbanks <- c(
  biens_X = "011795478",
  biens_M = "011795520", # Biens
  biens_manuf_X = "011795480", 
  biens_manuf_M = "011795522", # Biens manufacturés
  biens_indus_X = "011795482",  # Biens industriels
  biens_indus_M = "011795524",
  pib = "011794859"               # PIB total
)


# Construction de l'URL
url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(idbanks, collapse = "+")
)

# Import et transformation des données
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

# ---- Graphique ----

ggplot(data) +
  geom_line(aes(x = TIME_PERIOD, y = OBS_VALUE, color = Cna_produit)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_date(
    breaks = seq(1940, 2025, by = 10) %>% paste0("-01-01") %>% as.Date(),
    labels = date_format("%Y")
  ) +
  scale_y_continuous(
    breaks = seq(-1, 5, by = 0.01),
    labels = percent_format(accuracy = 1)
  ) +
  scale_color_manual(values = viridis(4)[1:3]) +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(
    legend.position = c(0.75, 0.9),
    legend.title = element_blank()
  )

