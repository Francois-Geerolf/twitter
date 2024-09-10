for (n in gsub("\\.R$", "", list.files(path = "R"))){
  cat(paste0("Currently replicating: https://x.com/FrancoisGeerolf/status/", n), "\n")
  source(paste0("R/", n, ".R"))
  ggsave(paste0("png/", n, ".png"), height = 1.25*3.375, width = 1.25*6, bg = "white")
  ggsave(paste0("pdf/", n, ".pdf"), height = 1.25*3.375, width = 1.25*6)
}
