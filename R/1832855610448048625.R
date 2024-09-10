# Inflation Juillet 2021-2024: IPCH, énergie, alimentation
# @FrancoisGeerolf
# https://twitter.com/FrancoisGeerolf/status/1832855610448048625
n <- "1832855610448048625"

library("tidyverse")
library("scales")

data <- "001762445+001762847+001759971" |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", a = _) |>
  rsdmx::readSDMX() |>
  as_tibble()  |>
  mutate(date = as.Date(paste0(TIME_PERIOD, "-01")),
         OBS_VALUE = as.numeric(OBS_VALUE)) |>
  group_by(date) |>
  filter(n() == 3) |>
  ungroup() |>
  filter(date >= as.Date("2021-07-01")) |>
  # Extraire après le dernier tiret -----
  mutate(Coicop2016 = str_extract(TITLE_FR, "[^-]+$")) |>
  arrange(date) |>
  group_by(Coicop2016) |>
  mutate(OBS_VALUE = 100*OBS_VALUE/OBS_VALUE[1])

data |>
  ggplot() + ylab("") + xlab("") + theme_minimal() +
  geom_line(aes(x = date, y = OBS_VALUE, color = Coicop2016)) +
  theme_minimal() + xlab("") + ylab("") +
  scale_x_date(breaks = "2 months",
               labels = date_format("%b %Y")) +
  theme(legend.position = c(0.28, 0.87),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  scale_y_log10(breaks = seq(0, 200, 2),
                labels = dollar_format(accuracy = 1, prefix = "")) +
  geom_label(data = . %>% filter(date == max(date)),
             aes(x = date, y = OBS_VALUE, label = round(OBS_VALUE, 1), color = Coicop2016), 
             fontface ="plain", size = 3)
