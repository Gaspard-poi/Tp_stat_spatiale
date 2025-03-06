
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

library(dplyr)
library(sf)
install.packages("mapsf")
library(mapsf)
library(classInt)
library(leaflet)


install.packages("readxl")
pop_com_2019 <- openxlsx::read.xlsx("data/Pop_legales_2019.xlsx")
communes_fm <- st_read("fonds/commune_francemetro_2021.gpkg")

pop_com_2019 <- pop_com_2019 %>%
  mutate(COM=if_else(substr(COM,1,3)=="751", "75056", COM))%>%
  group_by(code=COM) %>%
  summarise(pop=sum(PMUN19))

communes_fm <- communes_fm %>%
  left_join(pop_com_2019, by="code")%>%
  mutate(densite=pop/surf)

plot(communes_fm %>% st_geometry(), lwd = 0.1)
summary(communes_fm$densite)
hist(communes_fm$densite)

plot(communes_fm['densite'], border = F)
plot(communes_fm['densite'], breaks = 'quantile', main = "quantile", border = F)
plot(communes_fm['densite'], breaks = 'kmeans', main = "quantile", border = F)
plot(communes_fm['densite'], breaks = 'jenks', main = "quantile", border = F)
plot(communes_fm['densite_log'], breaks = "quantile", main = "Echelle logarithmique", border = FALSE)
communes_fm$densite_log <- log(communes_fm$densite + 1)

denspop_quant <- classIntervals(
  communes_fm$densite,
  style = 'quantile',
  n=5)
str(denspop_quant)
denspop_quant$brks

quantile(communes_fm$densite, probs = seq(0,1,0.1))
denspop_man_brks5 <- c(0,40,250,2000,15000, 27000)
popcomfm_sf <- communes_fm %>%
  mutate(
    densite_c = cut(
      densite,
      breaks = denspop_man_brks5,
      labels = paste0(denspop_man_brks5[1:5], "-", denspop_man_brks5[2:6]),
      include.lowest = T,
      right = F,
      ordered_result = T
    )
  )

plot(popcomfm_sf['densite_c'], border = F, pal = pa12)
pa12 <- c(
  RColorBrewer::brewer.pal(
    n=5,
    name = "Greens"
    )[4:3],
    RColorBrewer::brewer.pal(
    n=5,
    name="YlOrRd"
    )[c(2,4:5)]
  )
plot(popcomfm_sf['densite_c'], border = F, pal = pa12)
