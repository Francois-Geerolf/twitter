#!/usr/bin/env Rscript

# Exécuter le fichier _run.R s'il existe
if (file.exists("_run.R")) {
  message("▶️ Exécution de _run.R")
  source("_run.R")
}

# Compiler le fichier README.Rmd vers README.md
if (file.exists("README.Rmd")) {
  message("📄 Compilation de README.Rmd")
  rmarkdown::render("README.Rmd", output_format = "md_document", output_file = "README.md")
}
