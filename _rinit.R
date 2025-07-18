# Define the package list
packages <- c(
  "tidyverse",
  "rsdmx",
  "scales",
  "viridis",
  "zoo",
  "lubridate",
  "ggrepel",
  "curl"
)

# Install any missing packages
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed], dependencies = TRUE)
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))
