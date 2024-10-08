# Exportations nettes (% du PIB)
# https://twitter.com/FrancoisGeerolf/status/1487364702841749504

library("rsdmx")
library("tidyverse")
library("scales")
library("viridis")
library("zoo")

data <- "010565588+010565630+010565590+010565632+010565592+010565634+010565707" |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", a = _) |>
  readSDMX() |>
  as_tibble() |>
  mutate(OBS_VALUE = OBS_VALUE |> as.numeric(),
         TIME_PERIOD = as.Date(as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"))) |>
  select(IDBANK, TIME_PERIOD, OBS_VALUE) |>
  spread(IDBANK, OBS_VALUE) |>
  transmute(TIME_PERIOD,
            `Biens` = (`010565588` - `010565630`) / `010565707`,
            `Biens manufacturés ` = (`010565590` - `010565632`) / `010565707`,
            `Biens industriels` = (`010565592` - `010565634`) / `010565707`) |>
  gather(Cna_produit, OBS_VALUE, -TIME_PERIOD)

data |>
  ggplot() + geom_line(aes(x = TIME_PERIOD, y = OBS_VALUE, color = Cna_produit)) +
  theme_minimal() + xlab("") + ylab("") +
  scale_x_date(breaks = seq(1940, 2025, 10) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_y_continuous(breaks = 0.01*seq(-100, 500, 1),
                     labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = viridis(4)[1:3]) +
  theme(legend.position = c(0.75, 0.9),
        legend.title = element_blank()) + 
  geom_hline(yintercept = 0, linetype = "dashed")
