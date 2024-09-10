# Indice de production industrielle vs. Mai 2017
# @FrancoisGeerolf, status: 1816121032928874928
# https://twitter.com/FrancoisGeerolf/status/1816121032928874928

library("tidyverse")

data <- "010768261+010768307" |>
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", a = _) |>
  rsdmx::readSDMX() |>
  as_tibble()  |>
  mutate(date = as.Date(paste0(TIME_PERIOD, "-01"))) |>
  filter(date >= as.Date("2017-05-01")) |>
  mutate(OBS_VALUE = ifelse(year(date) == 2020 & month(date) %in% c(3, 4, 5, 6), NA, as.numeric(OBS_VALUE)),
         Naf2 = str_extract(TITLE_FR, "(?<=- ).*(?= \\(NAF)")) |>
  group_by(Naf2) |>
  mutate(OBS_VALUE = 100*OBS_VALUE/OBS_VALUE[date == as.Date("2017-05-01")])

data |>
  ggplot() + ylab("Indice de Production Industrielle vs. Mai 2017\nhors Mars-Juin 2020") + xlab("") + theme_minimal() +
  geom_line(aes(x = date, y = OBS_VALUE, color = Naf2))  +
  geom_label_repel(data = data |>
                     filter(date == max(date)),
                   aes(x = date, y = OBS_VALUE, label = percent(OBS_VALUE/100-1, acc = .1), color = Naf2)) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  scale_x_date(breaks = as.Date(paste0(seq(1920, 2100, 1), "-01-01")),
               labels = date_format("%Y")) +
  theme(legend.position = c(0.7, 0.9),
        legend.title = element_blank()) +
  scale_y_log10(breaks = seq(90, 110, 1),
                labels = percent(seq(90, 110, 1)/100 - 1, 1)) +
  geom_rect(data = data_frame(start = as.Date("2020-02-01"), 
                              end = as.Date("2020-07-01")), 
            aes(xmin = start, xmax = end, ymin = 0, ymax = +Inf), fill = viridis(4)[4], alpha = 0.2)

ggsave("1816121032928874928.png", height = 1.25*3.375, width = 1.25*6, bg = "white")
ggsave("1816121032928874928.pdf", height = 1.25*3.375, width = 1.25*6)