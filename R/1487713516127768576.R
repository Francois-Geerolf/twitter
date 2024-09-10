# Emploi Trimestriel (2016T1-)
# https://twitter.com/FrancoisGeerolf/status/1487713516127768576

library("rsdmx")
library("tidyverse")
library("scales")
library("zoo")

data <- "001791539+001791541+010599703+010600319" |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", a = _, "?startPeriod=2016") |>
  readSDMX() |>
  as_tibble() |>
  mutate(OBS_VALUE = OBS_VALUE |> as.numeric(),
         TIME_PERIOD = as.Date(as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"))) |>
  group_by(TITLE_FR) |>
  mutate(OBS_VALUE = 100*OBS_VALUE / OBS_VALUE[TIME_PERIOD == as.Date("2016-01-01")],
         TITLE_FR = gsub("Emploi salarié en fin de trimestre - ", "", TITLE_FR),
         TITLE_FR = gsub(" - France métropolitaine", "", TITLE_FR))

data |>
  ggplot() + geom_line(aes(x = TIME_PERIOD, y = OBS_VALUE, color = TITLE_FR)) +
  scale_x_date(breaks = seq(1960, 2100, 1) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_y_log10(breaks = seq(0, 120, 1)) +
  xlab("") + ylab("Emploi salarié en fin de trimestre \n France métropolitaine (100 = 2016-Q1)") +
  theme_minimal() +
  theme(legend.position = c(0.4, 0.9),
        legend.title = element_blank())
