# Hausse des prix dans la grande distribution (Octobre 2021 -)


# https://twitter.com/FrancoisGeerolf/status/1826530570236469418

library("tidyverse")

data <- "001768737+001768738+001768739+001768740" |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", a = _) |>
  rsdmx::readSDMX() |>
  as_tibble()  |>
  mutate(date = as.Date(paste0(TIME_PERIOD, "-01")),
         OBS_VALUE = as.numeric(OBS_VALUE)) |>
  filter(date >= as.Date("2021-10-01")) |>
  # Extraire après le dernier tiret -----
  mutate(`Forme-Vente` = str_extract(TITLE_FR, "[^-]+$")) |>
  arrange(date) |>
  group_by(`Forme-Vente`) %>%
  mutate(OBS_VALUE = 100*OBS_VALUE/OBS_VALUE[1])

data |>
  ggplot() + geom_line(aes(x = date, y = OBS_VALUE, color = `Forme-Vente`)) +
  theme_minimal() + xlab("") + ylab("Augmentation vs. Octobre 2021") +
  theme(legend.position = c(0.25, 0.8),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks = seq(-100, 500, 2),
                     labels = percent(seq(-100, 500, 2)/100-1)) +
  scale_x_date(breaks = "2 months",
               labels = date_format("%b %Y"))

ggsave("1826530570236469418.png", height = 1.25*3.375, width = 1.25*6, bg = "white")
ggsave("1826530570236469418.pdf", height = 1.25*3.375, width = 1.25*6)