
library(purrr)

fichiers <- data.frame(
  fichier = c(
  "Pop_legales_2019.xlsx",
  "Taux_pauvrete_2018.xlsx",
  "pop_region_2019.xlsx",
  "bpe20_sport_loisir_xy.csv",
  "base-cc-evol-struct-pop-2018_echantillon.csv",
  "commune_francemetro_2021.gpkg",
  "dep_francemetro_2021.gpkg",
  "merf_2021.gpkg",
  "reg_francemetro_2021.gpkg"
  ),
  type = c(rep("data",5),rep("fonds",4)),
  remote = c(rep("Data",5),rep("Fonds",4))
)

walk(
  unique(fichiers$type),
  \(t) if(!dir.exists(t)) dir.create(paste0(t, "/"), recursive = TRUE)
)

upload_fichier <- function(fichier, type, remote){
  
  base_url <- "https://minio.lab.sspcloud.fr/daudenaert"
  
  tryCatch(
    {
      download.file(url = paste0(base_url, "/", remote, "/", fichier),
                    destfile = paste0(type, "/", fichier))
    },
    error = function(e){print(paste0("pb de téléchargement pour le fichier ", fichier))}
  )
}

pwalk(fichiers, upload_fichier)


