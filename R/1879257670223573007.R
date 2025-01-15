# Naissances et décès: 627 895 naissances vs. 627 894 décès, selon l'Insee !
# https://twitter.com/FrancoisGeerolf/status/1879257670223573007

library("tidyverse")
library("scales")

data <- "000436394+000436391" |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", a = _) |>
  rsdmx::readSDMX() |>
  as_tibble()  |>
  mutate(date = as.Date(paste0(TIME_PERIOD, "-01")),
         OBS_VALUE = as.numeric(OBS_VALUE)) %>%
  mutate(date = as.Date(date)) |>
  group_by(TITLE_FR) |>
  arrange(date) |>
  mutate(OBS_VALUE = rollsum(x = OBS_VALUE, 12, align = "right", fill = NA),
         DEMOGRAPHIE2 = ifelse(grepl("décès", TITLE_FR), "Décès", "Naissances")) |>
  arrange(desc(date))

data |>
  ggplot() + geom_line(aes(x = date, y = OBS_VALUE, color = TITLE_FR)) +
  scale_x_date(breaks = seq(1880, 2100, 5) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_y_continuous(breaks = seq(0, 1000000, 20000),
                     labels = dollar_format(pre = "")) +
  theme_minimal() + xlab("") + ylab("Nombre sur les 12 derniers mois") +
  theme(legend.position = "none") +
  geom_label(data = . %>% filter(date == as.Date("1950-01-01")), aes(x = date, y = OBS_VALUE, label = DEMOGRAPHIE2, color = TITLE_FR, size = 2)) +
  ggtitle("Naissances et décès sur les 12 derniers mois - France Métropolitaine") +
  geom_text_repel(data = . %>% filter(max(date) == date),
                  aes(x =  date, y = OBS_VALUE, color = TITLE_FR, label = OBS_VALUE))
