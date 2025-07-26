
source("_rinit.R")

# Définir les dimensions des graphiques
plot_height <- 1.25 * 3.375
plot_width  <- 1.25 * 6

# Créer les répertoires de sortie s'ils n'existent pas
dir.create("png", showWarnings = FALSE)
dir.create("pdf", showWarnings = FALSE)
dir.create("csv", showWarnings = FALSE)

# Lister les fichiers .R dans le dossier "R"
r_files <- list.files(pattern = "\\.R$", full.names = FALSE, recursive = T) |>
  as_tibble() |>
  filter(!startsWith(value, "_")) |>
  pull(value)

# Boucle sur chaque fichier
for (file_path in r_files) {
  
  # Extraire le nom de fichier sans extension ni chemin
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  cat("Currently replicating: https://x.com/FrancoisGeerolf/status/", file_name, "\n")
  
  # Exécuter le fichier .R
  
  source(file_path)
  
  # Sauvegarder le dernier graphique généré
  ggsave(filename = file.path("png", paste0(file_name, ".png")),
         height = plot_height, width = plot_width, bg = "white")
  
  ggsave(filename = file.path("pdf", paste0(file_name, ".pdf")),
         height = plot_height, width = plot_width)
  
  
  write.csv(data, file.path("csv", paste0(file_name, ".csv")))
}
