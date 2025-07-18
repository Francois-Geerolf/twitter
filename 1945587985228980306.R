# Hausse du déficit depuis 2017
# https://twitter.com/FrancoisGeerolf/status/1945587985228980306

source("_rinit.R")

# ---- Paramètres ----

idbank_codes <- c("011779992")
version <- "8574705"
filename <- "T_3201.xlsx"

# ---- Construction de l’URL ----

url <- paste0(
  "https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/",
  paste(idbank_codes, collapse = "+")
)

url2 <- paste(
  "https://www.insee.fr/fr/statistiques/fichier",
  version,
  filename,
  sep = "/"
)

# ---- Import et traitement des données ----

gdp <- url |>
  readSDMX() |>
  as_tibble() |>
  transmute(date = as.Date(paste0(TIME_PERIOD, "-01-01")),
            gdp = (OBS_VALUE |> as.numeric())/1000) |>
  arrange(date)

# ---- Import et traitement des données ----

file <- tempfile()
curl_download(url2, destfile = file, quiet = F)

T_3201 <- readxl::read_excel(file, skip = 1) |>
  rename(Line = ...1) |>
  filter(!is.na(`2019`)) |>
  mutate(line = 1:n()) |>
  gather(year, value, -Line, -line) |>
  mutate(value = as.numeric(value)) |>
  filter(!is.na(value))

data <- T_3201 |>
  filter(line %in% c(44, 19)) |>
  mutate(date = as.Date(paste0(year, "-01-01"))) |>
  left_join(gdp, by = "date") |>
  filter(date >= as.Date("2017-01-01"))

delta_data <- data |>
  filter(format(date, "%Y") %in% c("2017", "2024")) |>
  group_by(date) |>
  summarise(
    y_max = max(value / gdp),
    y_min = min(value / gdp),
    delta = y_max - y_min,
    .groups = "drop"
  )

# ---- Extraire les y_max et y_min de 2017  ----
y_vals_2017 <- delta_data |>
  filter(date == as.Date("2017-01-01")) |>
  select(y_max, y_min) |>
  unlist()


ggplot(data = data) + geom_line(aes(x = date, y = value / gdp, color = Line), size = 1) +
  theme_minimal() + xlab("") + ylab("% du PIB") +
  scale_x_date(breaks = seq(1960, 2100, 1) |> paste0("-01-01") |> as.Date(),
               labels = date_format("%Y")) +
  scale_y_continuous(breaks = 0.01*seq(0, 500, 1),
                     labels = percent_format(accuracy = 1)) +
  
  theme(legend.position = c(0.2, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(face = "bold")) +
  geom_label(
    data = data |> filter(date %in% c(max(date), min(date))),
    aes(x = date, y = value / gdp, label = percent(value/gdp, acc = 0.1),
        color = Line),
    fontface = "bold"
  ) +
  geom_segment(
    data = delta_data,
    aes(x = date, xend = date, y = y_min+0.005, yend = y_max-0.005),
    arrow = arrow(length = unit(0.2, "cm"), ends = "both"),
    color = "black"
  ) +
  geom_hline(yintercept = y_vals_2017, linetype = "dotted", color = "grey50") +
  geom_label(
    data = delta_data,
    aes(x = date+months(6), y = (y_max + y_min) / 2,
        label = paste0("-", round(delta * 100, 1), " %")),
    color = "black",
    fontface = "bold",
    vjust = -0.5
  )

